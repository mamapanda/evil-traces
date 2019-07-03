;;; evil-traces.el --- Visual hints for `evil-ex' -*- lexical-binding: t -*-

;; Author: Daniel Phan <daniel.phan36@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (evil "1.0") (cl-lib "0.5"))
;; Homepage: TODO: homepage
;; Keywords: evil, visual

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Add visual hints for certain `evil-ex' commands.
;; To enable the hints, turn on `evil-traces-mode'.

;;; Code:

;; * Setup
(require 'cl-lib)
(require 'evil)

(defgroup evil-traces nil
  "Visual feedback for `evil-ex' commands."
  :prefix "evil-traces"
  :group 'evil)

;; * Timer
(defcustom evil-traces-idle-delay 0.1
  "The idle delay, in seconds, before evil-traces should update."
  :type 'float)

(defvar evil-traces--timer nil
  "The timer for evil-traces updates.")

(defun evil-traces--run-timer (fn &rest args)
  "Use evil-traces' timer to run FN with ARGS.
If the timer is currently running, then it is canceled first."
  (when evil-traces--timer
    (cancel-timer evil-traces--timer))
  (setq evil-traces--timer
        (apply #'run-at-time evil-traces-idle-delay nil fn args)))

(defun evil-traces--cancel-timer ()
  "Cancel evil-traces' timer if it is currently running."
  (cancel-timer evil-traces--timer)
  (setq evil-traces--timer nil))

;; * Overlay Management
(defvar evil-traces--overlays (make-hash-table)
  "A table of overlays where the keys are overlay names.")

(defun evil-traces--make-or-move-overlay (name beg end &rest props)
  "Set the range of the overlay named NAME to be from BEG to END.
If the overlay doesn't exist, then create it with PROPS first."
  (let ((ov (gethash name evil-traces--overlays)))
    (if ov
        (move-overlay ov beg end)
      (setq ov (make-overlay beg end evil-ex-current-buffer))
      (while props
        (let ((prop (pop props))
              (value (pop props)))
          (overlay-put ov prop value)))
      (puthash name ov evil-traces--overlays))
    ov))

(defun evil-traces--delete-overlay (name)
  "Delete the overlay named NAME."
  (when-let ((ov (gethash name evil-traces--overlays)))
    (delete-overlay ov)
    (remhash name evil-traces--overlays)))

;; * Runners
;; ** Helper Functions
(defun evil-traces--simple-hl (name flag range face)
  "Perform an operation on the overlay named NAME.
If FLAG is 'update, then move the overlay to RANGE, creating it with
FACE if it does not exist. If it is 'stop, then delete the overlay."
  (with-current-buffer evil-ex-current-buffer
    (cl-case flag
      (start nil)
      (update (evil-traces--run-timer #'evil-traces--make-or-move-overlay
                                      name
                                      (evil-range-beginning range)
                                      (evil-range-end range)
                                      'face face))
      (stop (evil-traces--cancel-timer)
            (evil-traces--delete-overlay name)))))

(defun evil-traces--simple-hl-range (name flag range face)
  "A helper function for commands that don't have a default range.
NAME, FLAG, RANGE, and FACE are the same as `evil-traces--simple-hl'."
  (evil-traces--simple-hl name (if range flag 'stop) range face))

(defun evil-traces--simple-hl-range-or-line (name flag range face)
  "A helper function for commands whose default range is the current line.
NAME, FLAG, RANGE, and FACE are the same as `evil-traces--simple-hl'."
  (with-current-buffer evil-ex-current-buffer
    (evil-traces--simple-hl name
                            flag
                            (or range (evil-ex-range (evil-ex-current-line)))
                            face)))

(defun evil-traces--simple-hl-range-or-buffer (name flag range face)
  "A helper function for commands whose default range is the whole buffer.
NAME, FLAG, RANGE, and FACE are the same as `evil-traces--simple-hl'."
  (with-current-buffer evil-ex-current-buffer
    (evil-traces--simple-hl name flag (or range (evil-ex-full-range)) face)))

;; ** Default
(defface evil-traces-default-face '((t (:inherit region)))
  "The default face for evil-traces overlays.")

(defun evil-traces-hl-range (flag &optional _arg)
  "Highlight `evil-ex-range' if it is non-nil.
FLAG is as described in `evil-ex-define-argument-type'."
  (evil-traces--simple-hl-range 'evil-traces-default
                                flag
                                evil-ex-range
                                'evil-traces-default-face))

(evil-ex-define-argument-type evil-traces-default
  :runner evil-traces-hl-range)

(defun evil-traces-hl-range-or-line (flag &optional _arg)
  "Highlight `evil-ex-range' if it is non-nil, or the current line otherwise.
FLAG is as described in `evil-ex-define-argument-type'."
  (evil-traces--simple-hl-range-or-line 'evil-traces-default-line
                                        flag
                                        evil-ex-range
                                        'evil-traces-default-face))

(evil-ex-define-argument-type evil-traces-default-line
  :runner evil-traces-hl-range-or-line)

(defun evil-traces-hl-range-or-buffer (flag &optional _arg)
  "Highlight `evil-ex-range' if it is non-nil, or the current buffer otherwise.
FLAG is as described in `evil-ex-define-argument-type'."
  (evil-traces--simple-hl-range-or-buffer 'evil-traces-default-buffer
                                          flag
                                          evil-ex-range
                                          'evil-traces-default-face))

(evil-ex-define-argument-type evil-traces-default-buffer
  :runner evil-traces-hl-range-or-buffer)

;; * Minor Mode
;; TODO: implement the types
(defcustom evil-traces-argument-type-alist
  ;; TODO: sort these in alphabetical order?
  '(
    (evil-ex-global . evil-traces-global)
    (evil-ex-global-inverted . evil-traces-global-inverted)
    (evil-ex-join . evil-traces-default-line)
    (evil-ex-sort . evil-traces-default-buffer)
    (evil-change . evil-traces-default-line)
    (evil-copy . evil-traces-default-line)
    (evil-move . evil-traces-default-line)
    (evil-ex-yank . evil-traces-default-line)
    (evil-ex-delete . evil-traces-default-line)
    (evil-ex-normal . evil-traces-default-line))
  "An alist mapping `evil-ex' functions to their argument types."
  :type '(alist :key function :value symbol))

(defcustom evil-traces-lighter " traces"
  "Lighter for evil-traces."
  :type 'string)

(defvar evil-traces--old-argument-types-alist nil
  "An alist mapping `evil-ex' function to their old argument types.
This is used to restore the appropriate argument types when
evil-traces is turned off.")

(defun evil-traces--register-argument-types ()
  "Register evil-traces' argument types with `evil-ex'."
  (dolist (type-desc evil-traces-argument-type-alist)
    (cl-destructuring-bind (fn . type) type-desc
      (when-let ((old-type (evil-get-command-property fn :ex-arg)))
        (add-to-list 'evil-traces--old-argument-types-alist (cons fn old-type)))
      (evil-set-command-property fn :ex-arg type))))

(defun evil-traces--unregister-argument-types ()
  "Unregister evil-traces' argument types from `evil-ex'."
  (dolist (type-desc evil-traces-argument-type-alist)
    (evil-remove-command-properties (car type-desc) :ex-arg))
  (dolist (old-type-desc evil-traces--old-argument-types-alist)
    (evil-set-command-property (car old-type-desc) :ex-arg (cdr old-type-desc)))
  (setq evil-traces--old-argument-types-alist nil))

;;;###autoload
(define-minor-mode evil-traces-mode
  "Global minor mode for evil-traces."
  :global t
  :lighter evil-traces-lighter
  (if evil-traces-mode
      (evil-traces--register-argument-types)
    (evil-traces--unregister-argument-types)))

;; * End
(provide 'evil-traces)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; evil-traces.el ends here

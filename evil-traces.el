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

(defun evil-traces--window-ranges (buffer)
  "Calculate the ranges covered by BUFFER's active windows."
  (let ((ranges (cl-loop for window in (get-buffer-window-list buffer nil t)
                         collect (cons (window-start window) (window-end window))))
        combined-ranges)
    (setq ranges (cl-sort ranges #'< :key #'car))
    (while (>= (length ranges) 2)
      (let ((range-1 (pop ranges))
            (range-2 (pop ranges)))
        (if (>= (cdr range-1) (car range-2))
            (push (cons (car range-1) (max (cdr range-1) (cdr range-2))) ranges)
          (push range-1 combined-ranges)
          (push range-2 ranges))))
    (when ranges
      (push (pop ranges) combined-ranges)) ; only one range is left
    (nreverse combined-ranges)))

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

;; ** Global
(defface evil-traces-global-range-face '((t :inherit evil-traces-default-face))
  "The face for :global's range.")

(defface evil-traces-global-match-face '((t :inherit evil-traces-default-face))
  "The face for matched :global terms.")

(defun evil-traces--global-match-name (number)
  "Return a match identifier based on NUMBER."
  (intern (format "evil-traces-global-match-%s" number)))

(defun evil-traces--global-matches (pattern &optional beg end)
  "Find :global matches for PATTERN between BEG and END.
If there are multiple matches in a line, only the first is considered."
  (let ((case-fold-search (eq (evil-ex-regex-case pattern evil-ex-search-case) 'insensitive))
        (beg (or beg (point-min)))
        (end (or end (point-max))))
    (save-excursion
      (save-match-data
        (goto-char beg)
        (cl-loop while (< (point) end)
                 when (re-search-forward pattern (line-end-position) t)
                 collect (cons (match-beginning 0) (match-end 0))
                 do (forward-line))))))

(defun evil-traces--delete-global-match-overlays (start end)
  "Delete :global match overlays for matches numbered from START to END, exclusive."
  (cl-loop for i from start below end
           for ov-name = (evil-traces--global-match-name i)
           do (evil-traces--delete-overlay ov-name)))

(defvar evil-traces--last-global-params nil
  "The last parameters passed to :global.")

(defvar evil-traces--last-global-match-count 0
  "The number of matches found by the last :global update.")

(defun evil-traces--update-global (pattern range)
  "Highlight RANGE and :global matches for PATTERN in RANGE."
  (with-current-buffer evil-ex-current-buffer
    (let* ((beg (evil-range-beginning range))
           (end (evil-range-end range))
           (params (list range pattern))
           (match-count 0))
      ;; don't have to update if the call is the exact same
      (unless (equal evil-traces--last-global-params params)
        (evil-traces--make-or-move-overlay
         'evil-traces-global-range beg end 'face 'evil-traces-global-range-face)
        (when pattern
          (condition-case nil
              (dolist (win-range (evil-traces--window-ranges (current-buffer)))
                (dolist (match-bounds (evil-traces--global-matches
                                       pattern (max beg (car win-range)) (min end (cdr win-range))))
                  (evil-traces--make-or-move-overlay (evil-traces--global-match-name match-count)
                                                     (car match-bounds)
                                                     (cdr match-bounds)
                                                     'face
                                                     'evil-traces-global-match-face)
                  (cl-incf match-count)))
            (invalid-regexp nil))) ; trailing backslash
        (evil-traces--delete-global-match-overlays match-count evil-traces--last-global-match-count)
        (setq evil-traces--last-global-params params
              evil-traces--last-global-match-count match-count)))))

(defun evil-traces-hl-global (flag &optional arg)
  "Highlight :global's range and matches.
FLAG is one of 'start, 'update, or 'stop and signals what to do.
ARG is the ex argument to :global."
  (cl-case flag
    (start
     (setq evil-traces--last-global-params nil
           evil-traces--last-global-match-count 0))
    (update
     (let ((range (or evil-ex-range
                      (with-current-buffer evil-ex-current-buffer
                        (evil-ex-full-range))))
           ;; if ARG is nil or "/", leave pattern as nil instead of the last pattern
           (pattern (and (> (length arg) 1)
                         (condition-case nil
                             (cl-first (evil-ex-parse-global arg))
                           (user-error nil))))) ; no previous pattern
       (evil-traces--run-timer #'evil-traces--update-global pattern range)))
    (stop
     (evil-traces--cancel-timer)
     (evil-traces--delete-overlay 'evil-traces-global-range)
     (evil-traces--delete-global-match-overlays 0 evil-traces--last-global-match-count))))

(evil-ex-define-argument-type evil-traces-global
  :runner evil-traces-hl-global)

;; * Minor Mode
;; TODO: implement the types
(defcustom evil-traces-argument-type-alist
  ;; TODO: sort these in alphabetical order?
  ;; TODO: change, copy, sort, join, yank, delete, and normal can have special runners
  '(
    (evil-ex-global . evil-traces-global)
    (evil-ex-global-inverted . evil-traces-global)
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

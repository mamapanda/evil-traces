;;; evil-traces.el --- Visual hints for `evil-ex' -*- lexical-binding: t -*-

;; Author: Daniel Phan <daniel.phan36@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (evil "1.2.13") (cl-lib "0.6"))
;; Homepage: https://github.com/mamapanda/evil-traces
;; Keywords: emulations, evil, visual

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
(eval-when-compile
  (require 'subr-x))

(defgroup evil-traces nil
  "Visual feedback for `evil-ex' commands."
  :prefix "evil-traces"
  :group 'evil)

;; * User Options
;; ** Faces
(defface evil-traces-default '((t (:inherit region)))
  "The default face for evil-traces overlays.")

(defface evil-traces-change '((t (:inherit evil-traces-default)))
  "Face for :change commands.")

(defface evil-traces-delete '((t (:inherit evil-traces-default)))
  "Face for :delete commands.")

(defface evil-traces-normal '((t (:inherit evil-traces-default)))
  "Face for :normal commands.")

(defface evil-traces-shell-command '((t (:inherit evil-traces-default)))
  "Face for :! commands.")

(defface evil-traces-yank '((t (:inherit evil-traces-default)))
  "Face for :yank commands.")

(defface evil-traces-move-range '((t (:inherit evil-traces-default)))
  "Face for :move's range.")

(defface evil-traces-move-preview '((t (:inherit evil-traces-default)))
  "Face for :move's preview.")

(defface evil-traces-copy-range '((t (:inherit evil-traces-default)))
  "Face for :copy's range.")

(defface evil-traces-copy-preview '((t (:inherit evil-traces-default)))
  "Face for :copy's preview.")

(defface evil-traces-global-range '((t :inherit evil-traces-default))
  "The face for :global's range.")

(defface evil-traces-global-match '((t :inherit isearch))
  "The face for matched :global terms.")

(defface evil-traces-join-range '((t (:inherit evil-traces-default)))
  "The face for :join's range.")

(defface evil-traces-join-indicator nil
  "The face for :join's indicator.")

(defface evil-traces-sort '((t (:inherit evil-traces-default)))
  "The face for :sort.")

(defface evil-traces-substitute-range '((t (:inherit evil-traces-default)))
  "The face for :substitute's range.")

;; ** Variables
(defcustom evil-traces-argument-type-alist
  '((evil-change             . evil-traces-change)
    (evil-copy               . evil-traces-copy)
    (evil-ex-delete          . evil-traces-delete)
    (evil-ex-global          . evil-traces-global)
    (evil-ex-global-inverted . evil-traces-global)
    (evil-ex-join            . evil-traces-join)
    (evil-ex-normal          . evil-traces-normal)
    (evil-ex-sort            . evil-traces-sort)
    (evil-ex-substitute      . evil-traces-substitute)
    (evil-ex-yank            . evil-traces-yank)
    (evil-move               . evil-traces-move)
    (evil-shell-command      . evil-traces-shell-command))
  "An alist mapping `evil-ex' functions to their argument types."
  :type '(alist :key function :value symbol))

(defcustom evil-traces-lighter " traces"
  "Lighter for evil-traces."
  :type 'string)

(defcustom evil-traces-idle-delay 0.05
  "The idle delay, in seconds, before evil-traces should update."
  :type 'float)

(defcustom evil-traces-join-indicator "<<<"
  "The indicator for :join.
This will be placed at the end of each line that will be joined with
the next."
  :type 'string)

(defcustom evil-traces-join-indicator-padding 2
  "The number of spaces to add before :join's indicator."
  :type 'integer)

;; * General Functions
;; ** Changing Faces
(defun evil-traces-use-diff-faces ()
  "Use `diff-mode' faces for evil-traces."
  (require 'diff-mode)
  ;; Setting :sort's face can be kind of distracting.
  (custom-set-faces
   '(evil-traces-change           ((t (:inherit diff-removed))))
   '(evil-traces-copy-preview     ((t (:inherit diff-added))))
   '(evil-traces-copy-range       ((t (:inherit diff-changed))))
   '(evil-traces-delete           ((t (:inherit diff-removed))))
   '(evil-traces-global-match     ((t (:inherit diff-added))))
   '(evil-traces-global-range     ((t (:inherit diff-changed))))
   '(evil-traces-join-indicator   ((t (:inherit diff-added))))
   '(evil-traces-join-range       ((t (:inherit diff-changed))))
   '(evil-traces-move-preview     ((t (:inherit diff-added))))
   '(evil-traces-move-range       ((t (:inherit diff-removed))))
   '(evil-traces-normal           ((t (:inherit diff-changed))))
   '(evil-traces-shell-command    ((t (:inherit diff-changed))))
   '(evil-traces-substitute-range ((t (:inherit diff-changed))))
   '(evil-traces-yank             ((t (:inherit diff-changed))))))

;; ** Overlays
(defvar evil-traces--highlights (make-hash-table)
  "A table where the keys are names and values are lists of overlays.")

(defun evil-traces--set-hl (name range &rest props)
  "Highlight RANGE with PROPS in the current buffer.
NAME is the name for the highlight.  RANGE may be a (beg . end) pair or
a list of such pairs, and PROPS is an overlay property list."
  (let ((ranges (if (numberp (cl-first range)) (list range) range))
        (overlays (gethash name evil-traces--highlights))
        new-overlays)
    (dolist (range ranges)
      (let ((ov (or (pop overlays) (make-overlay 0 0)))
            (props props))
        (move-overlay ov (car range) (cdr range))
        (while props
          (overlay-put ov (pop props) (pop props)))
        (push ov new-overlays)))
    (mapc #'delete-overlay overlays)
    (puthash name new-overlays evil-traces--highlights)
    new-overlays))

(defun evil-traces--delete-hl (name)
  "Delete the highlight named NAME."
  (mapc #'delete-overlay (gethash name evil-traces--highlights))
  (remhash name evil-traces--highlights))

;; ** Timer
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
  (when evil-traces--timer
    (cancel-timer evil-traces--timer)
    (setq evil-traces--timer nil)))

;; ** Windows
(defun evil-traces--window-ranges (buffer)
  "Calculate the ranges covered by BUFFER's active windows."
  (let ((ranges (cl-loop for window in (get-buffer-window-list buffer nil t)
                         collect (cons (window-start window)
                                       (window-end window))))
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

(defun evil-traces--points-at-visible-bol (beg end)
  "Find the positions of the beginnings of each line between BEG and END."
  (let (positions)
    (dolist (win-range (evil-traces--window-ranges (current-buffer)))
      (let ((subrange-beg (max beg (car win-range)))
            (subrange-end (min end (cdr win-range))))
        (save-excursion
          (goto-char subrange-beg)
          (while (< (point) subrange-end)
            (push (point) positions)
            (forward-line)))))
    (nreverse positions)))

;; * Visual Previews
;; ** Simple
(defun evil-traces--update-simple (hl-name hl-face range buffer
                                           &optional default-range)
  "Highlight RANGE in BUFFER using HL-FACE.
HL-NAME is the name for the highlight, and DEFAULT-RANGE may be either
'line or 'buffer."
  (with-current-buffer buffer
    (if-let ((range (or range
                        (cl-case default-range
                          (line (evil-ex-range (evil-ex-current-line)))
                          (buffer (evil-ex-full-range))))))
        (evil-traces--set-hl hl-name
                             (cons (evil-range-beginning range)
                                   (evil-range-end range))
                             'face
                             hl-face)
      (evil-traces--set-hl hl-name nil))))

(defun evil-traces--hl-simple (hl-name hl-face flag &optional default-range)
  "Handle highlighting the range of the current ex command.
The highlight will be named HL-NAME and use HL-FACE.
FLAG is the typical `evil-ex' argument runner flag.
DEFAULT-RANGE may be either 'line or 'buffer."
  (cl-case flag
    (update
     (evil-traces--run-timer #'evil-traces--update-simple
                             hl-name
                             hl-face
                             evil-ex-range
                             evil-ex-current-buffer
                             default-range))
    (stop
     (evil-traces--cancel-timer)
     (evil-traces--delete-hl hl-name))))

(defun evil-traces--hl-change (flag &optional _arg)
  "Update :change's highlight according to FLAG."
  (evil-traces--hl-simple 'evil-traces-change 'evil-traces-change flag 'line))

(evil-ex-define-argument-type evil-traces-change
  :runner evil-traces--hl-change)

(defun evil-traces--hl-delete (flag &optional _arg)
  "Update :delete's highlight according to FLAG."
  (evil-traces--hl-simple 'evil-traces-delete 'evil-traces-delete flag 'line))

(evil-ex-define-argument-type evil-traces-delete
  :runner evil-traces--hl-delete)

(defun evil-traces--hl-normal (flag &optional _arg)
  "Update :normal's highlight according to FLAG."
  (evil-traces--hl-simple 'evil-traces-normal 'evil-traces-normal flag 'line))

(evil-ex-define-argument-type evil-traces-normal
  :runner evil-traces--hl-normal)

(defun evil-traces--hl-shell-command (flag &optional _arg)
  "Update :!'s highlight according to FLAG."
  (evil-traces--hl-simple
   'evil-traces-shell-command 'evil-traces-shell-command flag))

(evil-ex-define-argument-type evil-traces-shell-command
  :runner evil-traces--hl-shell-command)

(defun evil-traces--hl-yank (flag &optional _arg)
  "Update :yank's highlight according to FLAG."
  (evil-traces--hl-simple 'evil-traces-yank 'evil-traces-yank flag 'line))

(evil-ex-define-argument-type evil-traces-yank
  :runner evil-traces--hl-yank)

;; ** Move / Copy
(defun evil-traces--parse-move (arg)
  "Parse ARG into the position where :move will place its text."
  (let ((parsed-arg (evil-ex-parse arg)))
    (when (eq (cl-first parsed-arg) 'evil-goto-line)
      (let* ((address (eval (cl-second parsed-arg))))
        (save-excursion
          (goto-char (point-min))
          (point-at-bol (1+ address)))))))

(defun evil-traces--get-move-text (beg end insert-pos)
  "Obtain the text between BEG and END, adding newlines as necessary for :move.
INSERT-POS is where the text will be inserted."
  (let ((text (buffer-substring beg end)))
    (unless (string-suffix-p "\n" text)
      (setq text (concat text "\n")))
    (save-excursion
      (goto-char insert-pos)
      (when (and (eolp) (not (bolp)))
        (setq text (concat "\n" text))))
    text))

(defun evil-traces--update-mover (range-hl-name
                                  range-hl-face
                                  preview-hl-name
                                  preview-hl-face
                                  range
                                  arg
                                  buffer)
  "Update the visual feedback for a :move type command.

RANGE-HL-NAME, RANGE-HL-FACE, PREVIEW-HL-NAME, PREVIEW-HL-FACE are the
names and faces to use for the command's range and preview
highlights.

RANGE and ARG are the current ex range and argument, and BUFFER is the
buffer to highlight in."
  (with-current-buffer buffer
    (let* ((range (or range (evil-ex-range (evil-ex-current-line))))
           (beg (evil-range-beginning range))
           (end (evil-range-end range)))
      (evil-traces--set-hl range-hl-name (cons beg end) 'face range-hl-face)
      (if-let ((insert-pos (evil-traces--parse-move arg)))
          (let ((move-text (evil-traces--get-move-text beg end insert-pos)))
            (evil-traces--set-hl preview-hl-name
                                 (cons insert-pos insert-pos)
                                 'before-string
                                 (propertize move-text 'face preview-hl-face)))
        (evil-ex-echo "Invalid address")))))

(defun evil-traces--hl-mover (range-hl-name
                              range-hl-face
                              preview-hl-name
                              preview-hl-face
                              flag
                              &optional arg)
  "Highlight a :move type command's range and preview its result.

RANGE-HL-NAME, RANGE-HL-FACE, PREVIEW-HL-NAME, PREVIEW-HL-FACE are the
names and faces to use for the command's range and preview
highlights.

FLAG ('start, 'update, or 'stop) signals what to do, while ARG is the
command's ex argument."
  (cl-case flag
    (update
     (evil-traces--run-timer #'evil-traces--update-mover
                             range-hl-name
                             range-hl-face
                             preview-hl-name
                             preview-hl-face
                             evil-ex-range
                             arg
                             evil-ex-current-buffer))
    (stop
     (evil-traces--cancel-timer)
     (evil-traces--delete-hl range-hl-name)
     (evil-traces--delete-hl preview-hl-name))))

(defun evil-traces--hl-move (flag &optional arg)
  "Show a visual preview for :move based on FLAG and ARG."
  (evil-traces--hl-mover 'evil-traces-move-range
                         'evil-traces-move-range
                         'evil-traces-move-preview
                         'evil-traces-move-preview
                         flag
                         arg))

(evil-ex-define-argument-type evil-traces-move
  :runner evil-traces--hl-move)

(defun evil-traces--hl-copy (flag &optional arg)
  "Show a visual preview for :copy based on FLAG and ARG."
  (evil-traces--hl-mover 'evil-traces-copy-range
                         'evil-traces-copy-range
                         'evil-traces-copy-preview
                         'evil-traces-copy-preview
                         flag
                         arg))

(evil-ex-define-argument-type evil-traces-copy
  :runner evil-traces--hl-copy)

;; ** Global
(defvar evil-traces--last-global-params nil
  "The last parameters passed to :global.")

(defun evil-traces--global-matches (pattern beg end)
  "Return the bounds of each match of PATTERN between BEG and END."
  (when pattern
    (let ((case-fold-search (eq (evil-ex-regex-case pattern evil-ex-search-case)
                                'insensitive)))
      (save-excursion
        (save-match-data
          (cl-loop for pos in (evil-traces--points-at-visible-bol beg end)
                   when (progn
                          (goto-char pos)
                          (re-search-forward pattern (point-at-eol) t))
                   collect (cons (match-beginning 0) (match-end 0))))))))

(defun evil-traces--update-global (range arg buffer)
  "Highlight :global's range and every visible match of its pattern.
RANGE is :global's ex range, ARG is its ex argument, and BUFFER is the
buffer to highlight in."
  (with-current-buffer buffer
    (condition-case error-info
        (let* ((range (or range (evil-ex-full-range)))
               (beg (evil-range-beginning range))
               (end (evil-range-end range))
               ;; don't use last pattern if ARG is nil or "/"
               (pattern (and (> (length arg) 1)
                             (cl-first (evil-ex-parse-global arg))))
               (params (list pattern range)))
          (unless (equal evil-traces--last-global-params params)
            (evil-traces--set-hl 'evil-traces-global-range
                                 (cons beg end)
                                 'face
                                 'evil-traces-global-range)
            (evil-traces--set-hl 'evil-traces-global-matches
                                 (evil-traces--global-matches pattern beg end)
                                 'face
                                 'evil-traces-global-match)
            (setq evil-traces--last-global-params params)))
      (user-error
       (evil-ex-echo (cl-second error-info)))
      (invalid-regexp
       (evil-ex-echo (cl-second error-info))))))

(defun evil-traces--hl-global (flag &optional arg)
  "Highlight :global's range and every visible match of its pattern.
FLAG is one of 'start, 'update, or 'stop and signals what to do.
ARG is the ex argument to :global."
  (cl-case flag
    (start
     (setq evil-traces--last-global-params nil))
    (update
     (evil-traces--run-timer
      #'evil-traces--update-global evil-ex-range arg evil-ex-current-buffer))
    (stop
     (evil-traces--cancel-timer)
     (evil-traces--delete-hl 'evil-traces-global-range)
     (evil-traces--delete-hl 'evil-traces-global-matches))))

(evil-ex-define-argument-type evil-traces-global
  :runner evil-traces--hl-global)

;; ** Join
(defun evil-traces--join-indicator-positions (beg end)
  "Find where to place :join's indicators.
BEG and END define the range of the lines to join."
  (or (save-excursion
        (cl-loop for pos in (evil-traces--points-at-visible-bol beg end)
                 do (goto-char pos)
                 when (/= (point-at-bol 2) end)
                 collect (point-at-eol)))
      (list (save-excursion
              (goto-char beg)
              (point-at-eol)))))

;; We need to differentiate between indicators inside and outside
;; `evil-ex-range' so that the background color isn't messed up.
(defun evil-traces--place-join-indicators (in-positions &optional out-positions)
  "Place :join's line indicators.
IN-POSITIONS are positions within the current ex range.
OUT-POSITIONS are positions outside the current ex range."
  (let ((padding (make-string evil-traces-join-indicator-padding ?\s)))
    (dolist (settings (list (list in-positions
                                  'evil-traces-join-in-indicators
                                  'evil-traces-join-range)
                            (list out-positions
                                  'evil-traces-join-out-indicators
                                  'default)))
      (cl-destructuring-bind (positions hl-name padding-face) settings
        (evil-traces--set-hl hl-name
                             (cl-loop for pos in positions
                                      collect (cons pos pos))
                             'after-string
                             (concat
                              (propertize padding 'face padding-face)
                              (propertize evil-traces-join-indicator
                                          'face
                                          'evil-traces-join-indicator)))))))

(defun evil-traces--update-join (range arg buffer)
  "Highlight RANGE and add indicators for :join lines in BUFFER.
ARG is :join's ex argument."
  (with-current-buffer buffer
    (let* ((range (or range (evil-ex-range (evil-ex-current-line))))
           (beg (evil-range-beginning range))
           (end (evil-range-end range)))
      (evil-traces--set-hl
       'evil-traces-join-range (cons beg end) 'face 'evil-traces-join-range)
      (cond
       ((not arg)
        (evil-traces--place-join-indicators
         (evil-traces--join-indicator-positions beg end)))
       ((string-match-p "^[1-9][0-9]*$" arg)
        (let ((indicator-positions
               (save-excursion
                 (goto-char end)
                 (evil-traces--join-indicator-positions
                  (point-at-bol 0) (point-at-bol (string-to-number arg))))))
          (evil-traces--place-join-indicators (list (pop indicator-positions))
                                              indicator-positions)))
       (t
        (evil-ex-echo "Invalid count"))))))

(defun evil-traces--hl-join (flag &optional arg)
  "Highlight the range covered by a :join command.
FLAG indicates whether to update or stop highlights, and ARG is a
string representing the count argument to :join."
  (cl-case flag
    (update
     (evil-traces--run-timer
      #'evil-traces--update-join evil-ex-range arg evil-ex-current-buffer))
    (stop
     (evil-traces--cancel-timer)
     (evil-traces--delete-hl 'evil-traces-join-range)
     (evil-traces--delete-hl 'evil-traces-join-in-indicators)
     (evil-traces--delete-hl 'evil-traces-join-out-indicators))))

(evil-ex-define-argument-type evil-traces-join
  :runner evil-traces--hl-join)

;; ** Sort
(defun evil-traces--sort-option-p (option)
  "Check if OPTION is a valid :sort option."
  (memq option '(?i ?u)))

(defun evil-traces--update-sort (range bang arg buffer)
  "Preview :sort over RANGE in BUFFER if RANGE is non-nil.
BANG is :sort's ex bang, and ARG is :sort's ex argument."
  (with-current-buffer buffer
    (cond
     ((and arg (cl-notevery #'evil-traces--sort-option-p (string-to-list arg)))
      (evil-ex-echo "Invalid option"))
     ((not range)
      (evil-traces--set-hl 'evil-traces-sort nil))
     (t
      (let* ((beg (evil-range-beginning range))
             (end (evil-range-end range))
             (lines (buffer-substring beg end))
             (sorted-lines (with-temp-buffer
                             (insert lines)
                             ;; hide "Reordering buffer... Done"
                             (let ((inhibit-message t))
                               (evil-ex-sort (point-min) (point-max) arg bang))
                             (buffer-string))))
        (evil-traces--set-hl 'evil-traces-sort
                             (cons beg end)
                             'face
                             'evil-traces-sort
                             'display
                             sorted-lines))))))

(defun evil-traces--hl-sort (flag &optional arg)
  "Preview the results of :sort.
FLAG indicates whether to update or stop highlights, and ARG is
:sort's ex argument."
  (cl-case flag
    (update
     (evil-traces--run-timer #'evil-traces--update-sort
                             evil-ex-range
                             evil-ex-bang
                             arg
                             evil-ex-current-buffer))
    (stop
     (evil-traces--cancel-timer)
     (evil-traces--delete-hl 'evil-traces-sort))))

(evil-ex-define-argument-type evil-traces-sort
  :runner evil-traces--hl-sort)

;; ** Substitute
(defun evil-traces--evil-substitute-runner (flag &optional arg)
  "Call evil's :substitute runner with FLAG and ARG."
  (when-let ((runner (evil-ex-argument-handler-runner
                      (alist-get 'substitution evil-ex-argument-types))))
    (funcall runner flag arg)))

(defun evil-traces--update-substitute (range arg buffer)
  "Preview :substitute in BUFFER according to RANGE and ARG."
  (with-current-buffer buffer
    (let ((range (or range (evil-ex-range (evil-ex-current-line)))))
      (evil-traces--set-hl 'evil-traces-substitute-range
                           (cons (evil-range-beginning range)
                                 (evil-range-end range))
                           'face
                           'evil-traces-substitute-range))
    (let ((evil-ex-hl-update-delay 0))
      (evil-traces--evil-substitute-runner 'update arg))))

(defun evil-traces--hl-substitute (flag &optional arg)
  "Preview the :substitute command.
FLAG indicates whether to start, update, or stop previews, and ARG is
:substitute's ex argument."
  (cl-case flag
    (start
     (evil-traces--evil-substitute-runner 'start arg))
    (update
     (evil-traces--update-substitute evil-ex-range arg evil-ex-current-buffer))
    (stop
     (evil-traces--cancel-timer)
     (evil-traces--evil-substitute-runner 'stop arg)
     (evil-traces--delete-hl 'evil-traces-substitute-range))))

(evil-ex-define-argument-type evil-traces-substitute
  :runner evil-traces--hl-substitute)

;; * Minor Mode
(defvar evil-traces--old-argument-types-alist nil
  "An alist mapping `evil-ex' function to their old argument types.
This is used to restore the appropriate argument types when
evil-traces is turned off.")

(defun evil-traces--register-argument-types ()
  "Register evil-traces' argument types with `evil-ex'."
  ;; `add-to-list' adds elements to the front of the list by default.
  ;; We iterate in reverse order so those first elements take precedence.
  (dolist (type-desc (reverse evil-traces-argument-type-alist))
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

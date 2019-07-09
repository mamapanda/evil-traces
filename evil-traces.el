;;; evil-traces.el --- Visual hints for `evil-ex' -*- lexical-binding: t -*-

;; Author: Daniel Phan <daniel.phan36@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (evil "1.2.13") (cl-lib "0.6"))
;; Homepage: https://github.com/mamapanda/evil-traces
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
(defcustom evil-traces-idle-delay 0.05
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
  (when evil-traces--timer
    (cancel-timer evil-traces--timer)
    (setq evil-traces--timer nil)))

;; * Overlay Management
(defvar evil-traces--highlights (make-hash-table)
  "A table where the keys are names and values are lists of overlays.")

(defun evil-traces--set-hl (name range &optional props buffer)
  "Highlight RANGE with PROPS in BUFFER.
NAME is the name for the highlight.  RANGE may be a (beg . end) pair or
a list of such pairs, and PROPS is an overlay property list."
  (let ((ranges (if (numberp (cl-first range)) (list range) range))
        (buffer (or buffer (current-buffer)))
        (overlays (gethash name evil-traces--highlights))
        new-overlays)
    (dolist (range ranges)
      (let ((ov (or (pop overlays) (make-overlay 0 0 buffer)))
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

;; * Visual Previews
;; ** General Options
(defface evil-traces-default-face '((t (:inherit region)))
  "The default face for evil-traces overlays.")

;; ** Helper Functions
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

;; ** Simple
(cl-defmacro evil-traces--define-simple (arg-type
                                         &optional
                                         doc
                                         &key
                                         runner-name
                                         face-name
                                         define-face
                                         default-range)
  "Define ARG-TYPE as a simple ex argument type.

DOC is a docstring describing ARG-TYPE.

RUNNER-NAME is the name of the runner function for ARG-TYPE. It will
be called by evil to provide visual feedback for an ex command of type
ARG-TYPE.

FACE-NAME defines the face to highlight with. If DEFINE-FACE is
non-nil, define FACE-NAME with a default value of inheriting from
`evil-traces-default-face'.

DEFAULT-RANGE is one of line, buffer, or nil and describes the range
ARG-TYPE commands take if an explicit range is not provided."
  (declare (indent defun) (doc-string 2))
  (cl-assert (symbolp arg-type))
  (cl-assert (symbolp runner-name))
  (cl-assert (symbolp face-name))
  (cl-assert (memq default-range '(line buffer nil)))
  `(progn
     ,(when define-face
        `(defface ,face-name '((t (:inherit evil-traces-default-face)))
           ,(format "Face for the %s ex argument type." arg-type)))
     ;; I don't think we need to `gensym' the parameters because none
     ;; of the macro arguments are variable names.
     (defun ,runner-name (flag &optional _arg)
       ,(format "Highlight the range of an ex command with type %s." arg-type)
       (cl-case flag
         (update
          (with-current-buffer evil-ex-current-buffer
            (if evil-ex-range
                (evil-traces--run-timer #'evil-traces--set-hl
                                        ',arg-type
                                        (cons (evil-range-beginning evil-ex-range)
                                              (evil-range-end evil-ex-range))
                                        '(face ,face-name)
                                        (current-buffer))
              ,(cl-case default-range
                 (line `(evil-traces--run-timer #'evil-traces--set-hl
                                                ',arg-type
                                                (cons (point-at-bol) (point-at-bol 2))
                                                '(face ,face-name)
                                                (current-buffer)))
                 (buffer `(evil-traces--run-timer #'evil-traces--set-hl
                                                  ',arg-type
                                                  (cons (point-min) (point-max))
                                                  '(face ,face-name)
                                                  (current-buffer)))
                 ((nil) `(evil-traces--run-timer #'evil-traces--delete-hl ',arg-type))))))
         (stop
          (evil-traces--cancel-timer)
          (evil-traces--delete-hl ',arg-type))))
     (evil-ex-define-argument-type ,arg-type
       ,doc ; DOC isn't even used by `evil-ex-define-argument-type'
       :runner ,runner-name)))

(evil-traces--define-simple evil-traces-default
  "Default argument type for commands that only perform on explicit ranges."
  :runner-name evil-traces--hl-range
  :face-name evil-traces-default-face)

(evil-traces--define-simple evil-traces-default-line
  "Default argument type for commands that default the range to the current line."
  :runner-name evil-traces--hl-range-or-line
  :face-name evil-traces-default-face
  :default-range line)

(evil-traces--define-simple evil-traces-default-buffer
  "Default argument type for commands that default the range to the whole buffer."
  :runner-name evil-traces--hl-range-or-buffer
  :face-name evil-traces-default-face
  :default-range buffer)

(evil-traces--define-simple evil-traces-change
  "Argument type for :change commands."
  :runner-name evil-traces--hl-change
  :face-name evil-traces-change-face
  :define-face t
  :default-range line)

(evil-traces--define-simple evil-traces-delete
  "Argument type for :delete commands."
  :runner-name evil-traces--hl-delete
  :face-name evil-traces-delete-face
  :define-face t
  :default-range line)

(evil-traces--define-simple evil-traces-normal
  "Argument type for :normal commands."
  :runner-name evil-traces--hl-normal
  :face-name evil-traces-normal-face
  :define-face t
  :default-range line)

(evil-traces--define-simple evil-traces-shell-command
  "Argument type for :shell commands."
  :runner-name evil-traces--hl-shell-command
  :face-name evil-traces-shell-command-face
  :define-face t)

(evil-traces--define-simple evil-traces-yank
  "Argument type for :yank commands."
  :runner-name evil-traces--hl-yank
  :face-name evil-traces-yank-face
  :define-face t
  :default-range line)

;; ** Movers
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

(cl-defmacro evil-traces--define-mover (arg-type
                                        &optional
                                        doc
                                        &key
                                        runner-name
                                        updater-name
                                        range-face-name
                                        preview-face-name
                                        define-faces)
  "Define an ex argument type of ARG-TYPE with behavior similar to :move.

DOC is a docstring describing ARG-TYPE.

RUNNER-NAME is the name to use for ARG-TYPE's runner function. The function
will be used by evil to provide ARG-TYPE's visual previews.

UPDATER-NAME is the name to use for the function that actually updates
the visual previews.

RANGE-FACE-NAME will be used to highlight the ex command's range.

PREVIEW-FACE-NAME will be used for the moved text's preview.

DEFINE-FACES, if non-nil, means to define both RANGE-FACE-NAME and
PREVIEW-FACE-NAME with default values of inheriting from
`evil-traces-default-face'."
  (declare (indent defun) (doc-string 2))
  (cl-assert (symbolp arg-type))
  (cl-assert (symbolp runner-name))
  (cl-assert (symbolp updater-name))
  (cl-assert (symbolp range-face-name))
  (cl-assert (symbolp preview-face-name))
  ;; I don't think variable names need to be `gensym'ed because none
  ;; of the macro parameters are variable names.
  (let ((range-hl-name (intern (format "%s-range" arg-type)))
        (preview-hl-name (intern (format "%s-preview" arg-type))))
    `(progn
       ,(when define-faces
          `(progn
             (defface ,range-face-name '((t (:inherit evil-traces-default-face)))
               ,(format "Face for the range of an ex command of type %s." arg-type))
             (defface ,preview-face-name '((t (:inherit evil-traces-default-face)))
               ,(format "Face for the preview text of an ex command of type %s." arg-type))))
       (defun ,updater-name (range arg buffer)
         ,(format "Preview the results of an ex command of type %s in BUFFER.
RANGE is the command's range, and ARG is its ex argument." arg-type)
         (with-current-buffer buffer
           (let* ((range (or range (evil-ex-range (evil-ex-current-line))))
                  (beg (evil-range-beginning range))
                  (end (evil-range-end range)))
             (evil-traces--set-hl ',range-hl-name (cons beg end) '(face ,range-face-name))
             (if-let ((insert-pos (evil-traces--parse-move arg)))
                 (let ((move-text (evil-traces--get-move-text beg end insert-pos)))
                   (evil-traces--set-hl ',preview-hl-name
                                        (cons insert-pos insert-pos)
                                        (list 'before-string
                                              (propertize move-text 'face ',preview-face-name))))
               (evil-ex-echo "Invalid address")))))
       (defun ,runner-name (flag &optional arg)
         (cl-case flag
           (update
            (evil-traces--run-timer #',updater-name evil-ex-range arg evil-ex-current-buffer))
           (stop
            (evil-traces--cancel-timer)
            (evil-traces--delete-hl ',range-hl-name)
            (evil-traces--delete-hl ',preview-hl-name))))
       (evil-ex-define-argument-type ,arg-type
         ,doc
         :runner ,runner-name))))

(evil-traces--define-mover evil-traces-move
  "Argument type for :move commands."
  :runner-name evil-traces--hl-move
  :updater-name evil-traces--update-move
  :range-face-name evil-traces-move-range-face
  :preview-face-name evil-traces-move-preview-face
  :define-faces t)

(evil-traces--define-mover evil-traces-copy
  "Argument type for :copy commands."
  :runner-name evil-traces--hl-copy
  :updater-name evil-traces--update-copy
  :range-face-name evil-traces-copy-range-face
  :preview-face-name evil-traces-copy-preview-face
  :define-faces t)

;; ** Global
(defface evil-traces-global-range-face '((t :inherit evil-traces-default-face))
  "The face for :global's range.")

(defface evil-traces-global-match-face '((t :inherit evil-traces-default-face))
  "The face for matched :global terms.")

(defvar evil-traces--last-global-params nil
  "The last parameters passed to :global.")

(defun evil-traces--global-matches (pattern beg end)
  "Return the bounds of each match of PATTERN between BEG and END."
  (when pattern
    (let ((case-fold-search (eq (evil-ex-regex-case pattern evil-ex-search-case) 'insensitive)))
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
                                 '(face evil-traces-global-range-face))
            (evil-traces--set-hl 'evil-traces-global-matches
                                 (evil-traces--global-matches pattern beg end)
                                 '(face evil-traces-global-match-face))
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
     (evil-traces--run-timer #'evil-traces--update-global evil-ex-range arg evil-ex-current-buffer))
    (stop
     (evil-traces--cancel-timer)
     (evil-traces--delete-hl 'evil-traces-global-range)
     (evil-traces--delete-hl 'evil-traces-global-matches))))

(evil-ex-define-argument-type evil-traces-global
  :runner evil-traces--hl-global)

;; ** Join
(defface evil-traces-join-range-face '((t (:inherit evil-traces-default-face)))
  "The face for :join's range.")

(defface evil-traces-join-indicator-face nil
  "The face for :join's indicator.")

(defcustom evil-traces-join-indicator "  <<<"
  "The indicator for :join."
  :type 'string)

(defun evil-traces--join-indicator-positions (beg end)
  "Find where to place :join's indicators.
BEG and END define the range of the lines to join."
  (let* ((positions (cl-loop for pos in (evil-traces--points-at-visible-bol beg end)
                             collect (save-excursion
                                       (goto-char pos)
                                       (point-at-eol))))
         (visual-end (save-excursion
                       (goto-char (cl-first (last positions)))
                       (point-at-bol 2))))
    (when (and (> (length positions) 1) (= visual-end end)) ; ignore the last :join line
      (setq positions (butlast positions)))
    positions))

(defun evil-traces--update-join-indicators (beg end)
  "Place :join line indicators in the range from BEG to END."
  (evil-traces--set-hl 'evil-traces-join-indicators
                       (cl-loop for pos in (evil-traces--join-indicator-positions beg end)
                                collect (cons pos pos))
                       (list 'after-string
                             (propertize evil-traces-join-indicator
                                         'face
                                         'evil-traces-join-indicator-face))))

(defun evil-traces--update-join (range arg buffer)
  "Highlight RANGE and add indicators for :join lines in BUFFER.
ARG is :join's ex argument."
  (with-current-buffer buffer
    (let* ((range (or range (evil-ex-range (evil-ex-current-line))))
           (beg (evil-range-beginning range))
           (end (evil-range-end range)))
      (evil-traces--set-hl
       'evil-traces-join-range (cons beg end) '(face evil-traces-join-range-face))
      (cond
       ((not arg)
        (evil-traces--update-join-indicators beg end))
       ((string-match-p "^[1-9][0-9]*$" arg)
        (let ((join-beg (save-excursion
                          (goto-char end)
                          (point-at-bol 0)))
              (join-end (save-excursion
                          (goto-char end)
                          (point-at-bol (string-to-number arg)))))
          (evil-traces--update-join-indicators join-beg join-end)))
       (t
        (evil-ex-echo "Invalid count"))))))

(defun evil-traces--hl-join (flag &optional arg)
  "Highlight the range covered by a :join command.
FLAG indicates whether to update or stop highlights, and ARG is a
string representing the count argument to :join."
  (cl-case flag
    (update
     (evil-traces--run-timer #'evil-traces--update-join evil-ex-range arg evil-ex-current-buffer))
    (stop
     (evil-traces--cancel-timer)
     (evil-traces--delete-hl 'evil-traces-join-range)
     (evil-traces--delete-hl 'evil-traces-join-indicators))))

(evil-ex-define-argument-type evil-traces-join
  :runner evil-traces--hl-join)

;; ** Sort
(defface evil-traces-sort-face '((t (:inherit evil-traces-default-face)))
  "The face for :sort.")

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
      (evil-traces--delete-hl 'evil-traces-sort))
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
                             (list 'face 'evil-traces-sort-face 'display sorted-lines)))))))

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
(defface evil-traces-substitute-range-face '((t (:inherit evil-traces-default-face)))
  "The face for :substitute's range.")

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
                           '(face evil-traces-substitute-range-face)))
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

;; ** Changing Faces
(defun evil-traces-use-diff-faces ()
  "Use `diff-mode' faces for evil-traces."
  ;; TODO: explore more faces. might be better to have more variety
  (require 'diff-mode)
  (custom-set-faces
   '(evil-traces-change-face         ((t (:inherit diff-removed))))
   '(evil-traces-delete-face         ((t (:inherit diff-removed))))
   '(evil-traces-global-match-face   ((t (:inherit diff-added))))
   '(evil-traces-global-range-face   ((t (:inherit diff-changed))))
   '(evil-traces-join-indicator-face ((t (:inherit diff-added))))
   '(evil-traces-join-range-face     ((t (:inherit diff-changed))))
   '(evil-traces-normal-face         ((t (:inherit diff-changed))))
   '(evil-traces-shell-command-face  ((t (:inherit diff-changed))))
   '(evil-traces-yank-face           ((t (:inherit diff-changed))))))

;; * Minor Mode
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

(require 'cl-lib)
(require 'ert)
(require 'evil)
(require 'evil-test-helpers)
(require 'evil-traces)

;; `seq-set-equal-p' is new with Emacs 26.
(defun evil-traces--set-equal-p (set1 set2 &optional testfn)
  "Check if SET1 and SET2 contain the same elements, regardless of order.
TESTFN will be used to test equality if non-nil."
  (null (cl-set-exclusive-or set1 set2 :test (or testfn #'equal))))

(defun evil-traces--hl-alist ()
  "Obtain an alist describing evil-traces' highlights.
The alist has highlight names as keys and lists of overlay
descriptions as values, where the overlay descriptions take the form
(beg end prop-list...)."
  (cl-loop for name being the hash-key of evil-traces--highlights
           using (hash-value ovs)
           for ov-descs = (cl-loop for ov in ovs
                                   for beg = (overlay-start ov)
                                   for end = (overlay-end ov)
                                   for props = (overlay-properties ov)
                                   collect (append (list beg end) props))
           collect (cons name ov-descs)))

(defun evil-traces--plist-equal-p (plist1 plist2)
  "Check whether PLIST1 and PLIST2 contain the same properties and values."
  (let ((alist1 (seq-partition plist1 2))
        (alist2 (seq-partition plist2 2)))
    (evil-traces--set-equal-p alist1 alist2)))

(defun evil-traces--hl-alist-equal-p (alist1 alist2)
  "Check whether ALIST1 and ALIST2 represent the same set of highlights.
The alists should have the same specification as
`evil-traces--hl-alist''s return value."
  (evil-traces--set-equal-p
   alist1 alist2
   (lambda (highlight1 highlight2)
     (and (eq (car highlight1) (car highlight2))
          (evil-traces--set-equal-p
           (cdr highlight1) (cdr highlight2)
           (lambda (ov-desc-1 ov-desc-2)
             (and (= (cl-first ov-desc-1) (cl-first ov-desc-2))
                  (= (cl-second ov-desc-1) (cl-second ov-desc-2))
                  (evil-traces--plist-equal-p (nthcdr 2 ov-desc-1) (nthcdr 2 ov-desc-2)))))))))

(defun evil-traces--should-have-hls (hl-alist)
  "Checks if the highlights in HL-ALIST are the same as evil-traces'."
  (should (evil-traces--hl-alist-equal-p (evil-traces--hl-alist) hl-alist)))

(defmacro evil-traces--without-timer (&rest body)
  "Execute BODY without evil-traces' timer."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'evil-traces--run-timer) #'funcall))
     ,@body))

(defun evil-traces--buffer-limits (&rest _)
  "A drop-in for `evil-traces--window-ranges' during testing.
This function returns point-min and point-max as the sole window range."
  (list (cons (point-min) (point-max))))

;; NOTE: Apparently (execute-kbd-macro ":<input>") executes the ex
;; command even if the input is incomplete, so we have to check
;; highlights in the middle of the kbd macro execution.
;; `enable-recursive-minibuffers' is set to t so that we can use "M-:"
;; (`eval-expression') in the middle of an ex command.
(setq enable-recursive-minibuffers t)

(evil-traces-mode)

(ert-deftest evil-traces-test-simple ()
  "Test simple highlighting."
  (ert-info ("No default range with no range given")
    (evil-traces--without-timer
      (evil-test-buffer
        "[t]est\nabcde\npotato\nteletubby\n"
        (":!echo"
         "\M-:" (format "%S" '(evil-traces--should-have-hls
                               '((evil-traces-shell-command . nil))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "[t]est\nabcde\npotato\nteletubby\n")))
  (ert-info ("No default range with range given")
    (evil-traces--without-timer
      (evil-test-buffer
        "[t]est\nabcde\npotato\nteletubby\n"
        (":2,3!echo"
         "\M-:" (format "%S" '(evil-traces--should-have-hls
                               '((evil-traces-shell-command
                                  . ((6 19 face evil-traces-shell-command))))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "test\n[\n]teletubby\n")))
  (ert-info ("Default range of current line with no range given")
    (evil-traces--without-timer
      (evil-test-buffer
        "[t]est\nabcde\npotato\nteletubby\n"
        (":d"
         "\M-:" (format "%S" '(evil-traces--should-have-hls
                               '((evil-traces-delete
                                  . ((1 6 face evil-traces-delete))))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "[a]bcde\npotato\nteletubby\n")))
  (ert-info ("Default range of current line with range given")
    (evil-traces--without-timer
      (evil-test-buffer
        "[t]est\nabcde\npotato\nteletubby\n"
        (":2,3d"
         "\M-:" (format "%S" '(evil-traces--should-have-hls
                               '((evil-traces-delete
                                  . ((6 19 face evil-traces-delete))))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "test\n[t]eletubby\n")))
  ;; At the moment, there are no simple highlighters with the whole
  ;; buffer as the default range.
  )

(ert-deftest evil-traces-test-move-and-copy ()
  "Test highlighting :move and :copy."
  (ert-info ("No range given")
    (evil-traces--without-timer
      (evil-test-buffer
        "test\nabcde\n[p]otato\nteletubby\n"
        (":m -2"
         "\M-:" (replace-regexp-in-string
                 ;; The literal newline inside "potato\n" isn't sent
                 ;; by `execute-kbd-macro', but we can substitute it
                 ;; with \ + n just fine here.
                 "\n" "\\\\n"
                 (format "%S" '(evil-traces--should-have-hls
                                '((evil-traces-move-range
                                   . ((12 19 face evil-traces-move-range)))
                                  (evil-traces-move-preview
                                   .  ((6 6 before-string "potato\n")))))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "test\n[p]otato\nabcde\nteletubby\n")))
  (ert-info ("Range given")
    (evil-traces--without-timer
      (evil-test-buffer
        "[t]est\nabcde\npotato\nteletubby\n"
        (":3t 0"
         "\M-:" (replace-regexp-in-string
                 "\n" "\\\\n"
                 (format "%S" '(evil-traces--should-have-hls
                                '((evil-traces-copy-range
                                   . ((12 19 face evil-traces-copy-range)))
                                  (evil-traces-copy-preview
                                   .  ((1 1 before-string "potato\n")))))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "[p]otato\ntest\nabcde\npotato\nteletubby\n"))))

(ert-deftest evil-traces-test-global ()
  "Test highlighting :global and :vglobal."
  (ert-info ("No pattern input")
    (evil-traces--without-timer
      (evil-test-buffer
        "[t]est\nabcde\npotato\nteletubby\n"
        (":g/"
         "\M-:" (format "%S" '(evil-traces--should-have-hls
                               '((evil-traces-global-range
                                  . ((1 29 face evil-traces-global-range)))
                                 (evil-traces-global-matches
                                  . nil))))
         [return]
         ;; finish feeding :global valid input, or we get a user-error
         "pattern/normal" [return])
        (evil-traces--should-have-hls nil)
        "test\nabcde\npotato\nteletubby\n[]")))
  (ert-info ("No range given")
    (evil-traces--without-timer
      (cl-letf (((symbol-function 'evil-traces--window-ranges) #'evil-traces--buffer-limits))
        (evil-test-buffer
          "[t]est\nabcde\npotato\nteletubby\n"
          (":g/e[slt]/normal"
           "\M-:" (format "%S" '(evil-traces--should-have-hls
                                 '((evil-traces-global-range
                                    . ((1 29 face evil-traces-global-range)))
                                   (evil-traces-global-matches
                                    . ((2 4 face evil-traces-global-match)
                                       (20 22 face evil-traces-global-match))))))
           [return] [return])
          (evil-traces--should-have-hls nil)
          "test\nabcde\npotato\n[t]eletubby\n"))))
  (ert-info ("Range given")
    (evil-traces--without-timer
      (cl-letf (((symbol-function 'evil-traces--window-ranges) #'evil-traces--buffer-limits))
        (evil-test-buffer
          "[t]est\nabcde\npotato\nteletubby\nwifi\n"
          (":2,4g/e/normal"
           "\M-:" (format "%S" '(evil-traces--should-have-hls
                                 '((evil-traces-global-range
                                    . ((6 29 face evil-traces-global-range)))
                                   (evil-traces-global-matches
                                    . ((10 11 face evil-traces-global-match)
                                       (20 21 face evil-traces-global-match))))))
           [return] [return])
          (evil-traces--should-have-hls nil)
          "test\nabcde\npotato\n[t]eletubby\nwifi\n"))))
  (ert-info ("Reuse last pattern")
    (evil-traces--without-timer
      (cl-letf (((symbol-function 'evil-traces--window-ranges) #'evil-traces--buffer-limits))
        (evil-test-buffer
          "[t]est\nabcde\npotato\nteletubby\nwifi\n"
          (":2,3g/e/normal" [return])
          (":g//normal"
           "\M-:" (format "%S" '(evil-traces--should-have-hls
                                 '((evil-traces-global-range
                                    . ((1 34 face evil-traces-global-range)))
                                   (evil-traces-global-matches
                                    . ((2 3 face evil-traces-global-match)
                                       (10 11 face evil-traces-global-match)
                                       (20 21 face evil-traces-global-match))))))
           [return] [return])
          (evil-traces--should-have-hls nil)
          "test\nabcde\npotato\n[t]eletubby\nwifi\n")))))

(ert-deftest evil-traces-test-join ()
  "Test highlighting :join."
  (ert-info ("No range or count given")
    (evil-traces--without-timer
      (cl-letf (((symbol-function 'evil-traces--window-ranges) #'evil-traces--buffer-limits))
        (evil-test-buffer
          "no\nno\n[n]o\nno\nno\nno\n"
          (":j"
           "\M-:" (format "%S" '(evil-traces--should-have-hls
                                 '((evil-traces-join-range
                                    . ((7 10 face evil-traces-join-range)))
                                   (evil-traces-join-in-indicators
                                    . ((9 9 after-string "  <<<")))
                                   (evil-traces-join-out-indicators
                                    . nil))))
           [return] [return])
          (evil-traces--should-have-hls nil)
          "no\nno\nno[ ]no\nno\nno\n"))))
  (ert-info ("Range given")
    (evil-traces--without-timer
      (cl-letf (((symbol-function 'evil-traces--window-ranges) #'evil-traces--buffer-limits))
        (evil-test-buffer
          "no\nno\n[n]o\nno\nno\nno\n"
          (":-1,+2j"
           "\M-:" (format "%S" '(evil-traces--should-have-hls
                                 '((evil-traces-join-range
                                    . ((4 16 face evil-traces-join-range)))
                                   (evil-traces-join-in-indicators
                                    . ((6 6 after-string "  <<<")
                                       (9 9 after-string "  <<<")
                                       (12 12 after-string "  <<<")))
                                   (evil-traces-join-out-indicators
                                    . nil))))
           [return] [return])
          (evil-traces--should-have-hls nil)
          "no\nno no no[ ]no\nno\n"))))
  (ert-info ("Count given")
    (evil-traces--without-timer
      (cl-letf (((symbol-function 'evil-traces--window-ranges) #'evil-traces--buffer-limits))
        (evil-test-buffer
          "no\nno\n[n]o\nno\nno\nno\n"
          (":j3"
           "\M-:" (format "%S" '(evil-traces--should-have-hls
                                 '((evil-traces-join-range
                                    . ((7 10 face evil-traces-join-range)))
                                   (evil-traces-join-in-indicators
                                    . ((9 9 after-string "  <<<")))
                                   (evil-traces-join-out-indicators
                                    . ((12 12 after-string "  <<<"))))))
           [return] [return])
          (evil-traces--should-have-hls nil)
          "no\nno\nno no[ ]no\nno\n"))))
  (ert-info ("Range and count given")
    (evil-traces--without-timer
      (cl-letf (((symbol-function 'evil-traces--window-ranges) #'evil-traces--buffer-limits))
        (evil-test-buffer
          "[n]o\nno\nno\nno\nno\nno\n"
          (":1,3j3"
           "\M-:" (format "%S" '(evil-traces--should-have-hls
                                 '((evil-traces-join-range
                                    . ((1 10 face evil-traces-join-range)))
                                   (evil-traces-join-in-indicators
                                    . ((9 9 after-string "  <<<")))
                                   (evil-traces-join-out-indicators
                                    . ((12 12 after-string "  <<<"))))))
           [return] [return])
          (evil-traces--should-have-hls nil)
          "no\nno\nno no[ ]no\nno\n")))))

(ert-deftest evil-traces-test-sort ()
  "Test highlighting :sort."
  (ert-info ("No range, bang, or options given")
    (evil-traces--without-timer
      (evil-test-buffer
        "[e]nsure\nq\nasdf\nline\nemacs\n"
        (":sort"
         "\M-:" (format "%S" '(evil-traces--should-have-hls
                               '((evil-traces-sort . nil))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "[a]sdf\nemacs\nensure\nline\nq\n")))
  (ert-info ("Range and bang given")
    (evil-traces--without-timer
      (evil-test-buffer
        "[e]nsure\nq\nasdf\nline\nemacs\n"
        (":%sort!"
         "\M-:" (replace-regexp-in-string
                 ;; same issue mentioned in :move and :copy's test
                 "\n" "\\\\n"
                 (format "%S" '(evil-traces--should-have-hls
                                '((evil-traces-sort
                                   . ((1 26 face evil-traces-sort
                                         display "q\nline\nensure\nemacs\nasdf\n")))))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "[q]\nline\nensure\nemacs\nasdf\n")))
  (ert-info ("Range and option given")
    (evil-traces--without-timer
      (evil-test-buffer
        "[e]nsure\nq\nasdf\nLine\nEmacs\n"
        (":%sort i"
         "\M-:" (replace-regexp-in-string
                 "\n" "\\\\n"
                 (format "%S" '(evil-traces--should-have-hls
                                '((evil-traces-sort
                                   . ((1 26 face evil-traces-sort
                                         display "asdf\nEmacs\nensure\nLine\nq\n")))))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "[a]sdf\nEmacs\nensure\nLine\nq\n")))
  (ert-info ("Range, bang, and option given")
    (evil-traces--without-timer
      (evil-test-buffer
        "[e]nsure\nasdf\nq\nLine\nq\nq\nq\nasdf\nEmacs\n"
        (":2,$-sort! u"
         "\M-:" (replace-regexp-in-string
                 "\n" "\\\\n"
                 (format "%S" '(evil-traces--should-have-hls
                                '((evil-traces-sort
                                   . ((8 31 face evil-traces-sort
                                         display "q\nasdf\nLine\n")))))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "ensure\n[q]\nasdf\nLine\nEmacs\n"))))

(ert-deftest evil-traces-test-substitute ()
  "Test highlighting :substitute."
  (ert-info ("No range given")
    (evil-traces--without-timer
      (evil-test-buffer
        "ert\ndeftest\nevil\n[t]races\ntest\nsubstitute\n()\n"
        (":s/ace/replace"
         "\M-:" (format "%S" '(evil-traces--should-have-hls
                               '((evil-traces-substitute-range
                                  . ((18 25 face evil-traces-substitute-range))))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "ert\ndeftest\nevil\n[t]rreplaces\ntest\nsubstitute\n()\n")))
  (ert-info ("Range given")
    (evil-traces--without-timer
      (evil-test-buffer
        "ert\ndeftest\nevil\n[t]races\ntest\nsubstitute\n()\n"
        (":-2,+2s/es/evil"
         "\M-:" (format "%S" '(evil-traces--should-have-hls
                               '((evil-traces-substitute-range
                                  . ((5 41 face evil-traces-substitute-range))))))
         [return] [return])
        (evil-traces--should-have-hls nil)
        "ert\ndeftevilt\nevil\ntracevil\n[t]evilt\nsubstitute\n()\n"))))

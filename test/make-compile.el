;;  bail out on compilation warnings and errors
(setq byte-compile-error-on-warn t)
(setq byte-compile--use-old-handlers nil)

;; The new variable was added fairly recently as of writing, so use the original
;; one for better compatibility for now.
(with-eval-after-load "evil"
  (push 'evil-ex-current-buffer byte-compile-not-obsolete-vars))

;; compile *.el files
(dolist (file (file-expand-wildcards "evil-traces*.el"))
  (unless (byte-compile-file file)
    (kill-emacs 1)))

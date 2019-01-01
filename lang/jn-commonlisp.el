;;;; -*- lexical-binding: t; -*-
(use-package lisp-mode
  :ensure nil
  :mode
  "\\.lisp\\'"
  "\\.stumpwmrc\\'"
  "\\.stumpish\\'"
  "\\.sbclrc\\'")

(use-package slime
  :ensure t
  :init
  (cond
   ((eq system-type 'gnu/linux)
    (setq inferior-lisp-program "/usr/bin/sbcl"))
   ((eq system-type 'windows-nt)
    (setq inferior-lisp-program "sbcl.exe"))
   (t
    (setq inferior-lisp-program "/usr/local/bin/sbcl")))
  :config
  (use-package slime-company
    :ensure t
    :commands (company-slime)
    :init
    (add-hook 'lisp-mode-hook
              (lambda ()
                (j|company-push-backend 'company-slime t)))
    :config
    (setq slime-company-completion 'fuzzy))
  (setq slime-contribs '(slime-asdf
                         slime-fancy
                         slime-indentation
                         slime-sbcl-exts
                         slime-scratch
                         slime-company))
  ;; Enable fuzzy matching in code buffer and SLIME REPL.
  (setq slime-complete-symbol*-fancy t)

  (slime-setup)

  (with-eval-after-load 'slime
    (evil-define-key 'normal slime-mode-map
      "\C-j" #'slime-eval-print-last-expression)))

(use-package stumpwm-mode
  :ensure t
  :commands (stumpwm-mode)
  :init
  ;; We expect stumpwm to launch a swank server.
  (add-hook 'slime-mode-hook
            (lambda ()
              (when (bound-and-true-p stumpwm-mode)
                (slime-connect "127.0.0.1" slime-port))))

  (setq stumpwm-shell-program
        (expand-file-name (concat user-emacs-directory "script/stumpish")))
  (add-hook 'lisp-mode-hook
            (lambda ()
              (when (or
                     (string-match-p "init.lisp" (buffer-name))
                     (string-match-p ".stumpwmrc" (buffer-name)))
                (stumpwm-mode)))))

;;;###autoload
(defun j|commonlisp-mode ()
  "Bootstrap `jn-commonlisp'."
  (setq auto-mode-alist (rassq-delete-all #'j|commonlisp-mode auto-mode-alist))
  (lisp-mode))

(provide 'jn-commonlisp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-commonlisp.el ends here

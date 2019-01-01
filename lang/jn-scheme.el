;;;; -*- lexical-binding: t; -*-

(use-package geiser
  :ensure t
  :commands (geiser-mode)
  :init
  (defun j|scheme-mode-setup-linting ()
    "Use `flymake' instead of `flycheck' in Emacs 26."
    (when (>= emacs-major-version 26)
      (flymake-mode)
      (flycheck-mode -1)))

  (add-hook 'scheme-mode-hook #'j|scheme-mode-setup-linting)
  (add-hook 'scheme-mode-hook #'geiser-mode)
  :config
  (setq geiser-mode-start-repl-p t)
  (setq geiser-repl-query-on-kill-p nil))

(use-package flymake-racket
  :ensure nil
  :commands (flymake-racket-add-hook)
  :load-path "~/.emacs.d/fork/flymake-racket/"
  :init
  (add-hook 'scheme-mode-hook #'flymake-racket-add-hook)
  (add-hook 'racket-mode-hook #'flymake-racket-add-hook))

;;;###autoload
(defun j|scheme-mode ()
  "Bootstrap `jn-commonlisp'."
  (setq auto-mode-alist (rassq-delete-all #'j|scheme-mode auto-mode-alist))
  (scheme-mode))

(provide 'jn-scheme)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-scheme.el ends here

;;;; -*- lexical-binding: t; -*-

(use-package geiser
  :ensure t
  :commands (geiser-mode)
  :init
  (add-hook 'scheme-mode-hook #'geiser-mode)
  :config
  (setq geiser-mode-start-repl-p t)
  (setq geiser-repl-query-on-kill-p nil))

;;;###autoload
(defun +scheme-mode ()
  "Bootstrap `jn-commonlisp'."
  (setq auto-mode-alist (rassq-delete-all #'+scheme-mode auto-mode-alist))
  (scheme-mode))

(provide 'jn-scheme)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-scheme.el ends here

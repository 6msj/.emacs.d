;;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'completion-ignored-extensions ".hi"))

;;;###autoload
(defun j|haskell-mode ()
  "Bootstrap `jn-haskell'."
  (setq auto-mode-alist (rassq-delete-all #'j|haskell-mode auto-mode-alist))
  (haskell-mode))

(provide 'jn-haskell)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-haskell.el ends here

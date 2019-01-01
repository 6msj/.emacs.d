;;;; -*- lexical-binding: t; -*-

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package racer
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'eldoc-mode)
  :config
  (setq racer-cmd (executable-find "racer")))

(use-package company-racer
  :ensure t
  :after company
  :config
  (push '(company-racer) company-backends))

(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :ensure t)

;;;###autoload
(defun j|rust-mode ()
  "Bootstrap `jn-rust'."
  (setq auto-mode-alist (rassq-delete-all #'j|rust-mode auto-mode-alist))
  (rust-mode))

(provide 'jn-rust)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-rust.el ends here

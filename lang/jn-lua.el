;;;; -*- lexical-binding: t; -*-

(use-package lua-mode
  :ensure t
  :mode
  ("\\.luacheckrc\\'" . lua-mode)
  ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (setq lua-indent-level 2))

(use-package company-lua
  :ensure t
  :commands (company-lua)
  :init
  (add-hook 'lua-mode-hook
            (lambda ()
              (setq company-lua-interpreter 'love)
              (j|company-push-backend 'company-lua t))))

(use-package love-minor-mode
  :ensure t
  :commands (love/possibly-enable-mode)
  :init
  (add-hook 'lua-mode-hook
            (lambda ()
              (love/possibly-enable-mode))))

;;;###autoload
(defun j|lua-mode ()
  "Bootstrap `jn-lua'."
  (setq auto-mode-alist (rassq-delete-all #'j|lua-mode auto-mode-alist))
  (lua-mode))

(provide 'jn-lua)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-lua.el ends here

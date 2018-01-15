;;;; -*- lexical-binding: t; -*-

(use-package lua-mode
  :ensure t
  :mode
  ("\\.luacheckrc\\'" . lua-mode)
  ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (setq lua-indent-level 2)
  (defun pd/love-run ()
    "Run pdrun script in root of project."
    (interactive)
    (let ((default-directory
            (locate-dominating-file default-directory "makefile"))
          (run-command "make"))
      (compilation-start run-command 'compilation-mode
                         (lambda (_mode-name)
                           "*pdrun make*") t))))

(use-package company-lua
  :ensure t
  :commands (company-lua)
  :init
  (add-hook 'lua-mode-hook
            (lambda ()
              (setq company-lua-interpreter 'love)
              (+company-push-backend 'company-lua t))))

(use-package love-minor-mode
  :ensure t
  :commands (love/possibly-enable-mode)
  :init
  (add-hook 'lua-mode-hook
            (lambda ()
              (love/possibly-enable-mode))))

;;;###autoload
(defun +lua-mode ()
  "Bootstrap `jn-lua'."
  (setq auto-mode-alist (rassq-delete-all #'+lua-mode auto-mode-alist))
  (lua-mode))

(provide 'jn-lua)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-lua.el ends here

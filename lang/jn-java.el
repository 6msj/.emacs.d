;;;; -*- lexical-binding: t; -*-
(require 'jn-functions)

(use-package cc-mode
  :ensure nil
  :mode ("\\.java\\'" . java-mode)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal java-mode-map
      (kbd "K") 'javadoc-lookup
      (kbd "gf") 'ggtags-find-file)))

(use-package javadoc-lookup
  :ensure t
  :commands (javadoc-lookup))

(use-package android-mode
  :ensure t
  :commands (android-root android-mode)
  :init
  (dolist (hook '(java-mode-hook groovy-mode-hook))
    (add-hook hook (lambda ()
                     (when (android-root)
                       (android-mode)
                       (set
                        (make-local-variable 'compilation-scroll-output) t)))))
  :config
  (setq android-mode-sdk-dir
        "/Applications/adt-bundle-mac-x86_64-20140702/sdk")
  (setq android-mode-builder 'gradle))

(use-package meghanada
  :ensure t
  :commands (meghanda-mode)
  :init
  (add-hook 'java-mode-hook (lambda ()
                              (when (projectile-project-p)
                                (j|company-push-backend 'company-meghanada t)
                                (meghanada-mode))))
  :config
  (defun j|meghanada-company-enable (command &rest args)
    "Wrap `meghanada-company-enable' to better tweak completion settings."
    (let ((save-company-backends company-backends)
          (save-company-transformers company-transformers))
      (apply command args)
      (setq-local company-backends save-company-backends)
      (setq-local company-transformers save-company-transformers)))

  (advice-add 'meghanada-company-enable :around 'j|meghanada-company-enable)

  (with-eval-after-load 'evil
    (evil-define-key 'normal meghanada-mode-map
      (kbd "go") 'meghanada-switch-testcase)))

;;;###autoload
(defun j|java-mode ()
  "Bootstrap `jn-java'."
  (setq auto-mode-alist (rassq-delete-all #'j|java-mode auto-mode-alist))
  (java-mode))

;; http://emacs.stackexchange.com/questions/508/how-to-configure-specific-java-indentation
(c-add-style
 "intellij"
 '("Java"
   (c-basic-offset . 4)
   (c-offsets-alist
    (inline-open . 0)
    (topmost-intro-cont    . +)
    (statement-block-intro . +)
    (knr-argdecl-intro     . 5)
    (substatement-open     . +)
    (substatement-label    . +)
    (label                 . +)
    (statement-case-open   . +)
    (statement-cont        . ++)
    (arglist-intro  . +)
    (arglist-close . c-lineup-arglist)
    (access-label   . 0)
    (inher-cont     . ++)
    (func-decl-cont . ++))))

(j|c-set-c-style '(java-mode . "intellij"))

(provide 'jn-java)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-java.el ends here

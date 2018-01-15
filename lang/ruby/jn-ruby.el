;;;; -*- lexical-binding: t; -*-

(use-package ruby-mode
  :ensure t
  :mode
  "\\.rb$\\'"
  "\\Rakefile$\\'"
  "\\.gemspec$\\'"
  "\\.ru$\\'"
  "\\Gemfile$\\'"
  "\\.rake$\\'"
  :interpreter "ruby"
  :init
  (defun +check-if-cocoapods ()
    "Check if the ruby file is cocoapods.
   - Disable flycheck for CocoaPods"
    (when (string-match-p "^.*Podfile$" (buffer-file-name))
      (when (bound-and-true-p flycheck-mode)
        (flycheck-mode 0))))
  (add-hook 'ruby-mode-hook #'+check-if-cocoapods))

(use-package robe
  :ensure t
  :commands (robe-mode)
  :init
  (add-hook 'ruby-mode-hook #'+robe-mode-hook)
  (defun +robe-mode-hook ()
    "`robe-mode' hook."
    (if (derived-mode-p 'motion-mode)
        (robe-mode 0)
      (progn
        (robe-mode 1)
        (+company-robe-setup))))
  (defun +company-robe-setup ()
    "Sets up ruby completion with company."
    (+company-push-backend 'company-robe t)))

(use-package projectile-rails
  :ensure t
  :commands (projectile-rails-on)
  :init
  (add-hook 'web-mode-hook #'+init-projectile-rails)
  (add-hook 'ruby-mode-hook #'+init-projectile-rails)
  (defun +init-projectile-rails ()
    "Do some startup initialization."
    (projectile-rails-on))
  :config
  (evil-define-key 'normal projectile-rails-mode-map
    (kbd "gf") 'projectile-rails-goto-file-at-point))

(use-package motion-mode
  :ensure t
  :commands (motion-recognize-project)
  :init
  (defun +motion-mode-hook ()
    "Motion mode hook."
    (setq motion-flymake nil) ;; Disable `flymake'.
    (motion-recognize-project)
    (+company-push-backend 'company-dict t)
    (setq flycheck-checker 'ruby-rubocop))
  (add-hook 'ruby-mode-hook #'+motion-mode-hook)
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'ruby-rubocop 'motion-mode))

  (evil-define-key 'normal motion-mode-map (kbd "K") 'motion-dash-at-point))

;;;###autoload
(defun +ruby-mode ()
  "Bootstrap `jn-ruby'."
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) '+ruby-mode)
      (setf (cdr alist) 'ruby-mode)))
  (ruby-mode))

(provide 'jn-ruby)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-ruby.el ends here

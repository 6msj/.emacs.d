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
  (defun j-ruby-check-for-cocoapods ()
    "Check if the ruby file is cocoapods.
   - Disable flycheck for CocoaPods"
    (when (string-match-p "^.*Podfile$" (buffer-file-name))
      (when (bound-and-true-p flycheck-mode)
        (flycheck-mode 0))))
  (add-hook 'ruby-mode-hook #'j-ruby-check-for-cocoapods))

(use-package robe
  :ensure t
  :commands (robe-mode)
  :init
  (j-company-push-backend 'company-robe)
  (add-hook 'ruby-mode-hook #'robe-mode))

(use-package projectile-rails
  :ensure t
  :commands (projectile-rails-on)
  :init
  (add-hook 'web-mode-hook #'projectile-rails-on)
  (add-hook 'ruby-mode-hook #'projectile-rails-on)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal projectile-rails-mode-map
      (kbd "gf") 'projectile-rails-goto-file-at-point)))

;;;###autoload
(defun j-ruby-mode ()
  "Bootstrap `jn-ruby'."
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) 'j-ruby-mode)
      (setf (cdr alist) 'ruby-mode)))
  (ruby-mode))

(provide 'jn-ruby)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-ruby.el ends here

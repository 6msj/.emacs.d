;;;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :ensure t
  :config
  :init
  (defun j|kotlin-mode-setup-linting ()
    "Use `flymake' instead of `flycheck' in Emacs 26."
    (when (>= emacs-major-version 26)
      (flymake-mode)
      (flycheck-mode -1)))

  (add-hook 'kotlin-mode-hook #'j|kotlin-mode-setup-linting)
  (add-hook 'kotlin-mode-hook #'j|kotlin-mode-setup-indentation)
  (defun j|kotlin-mode-setup-indentation ()
    "Use `swift-mode''s indentation instead."
    (autoload 'swift-mode:indent-line "swift-mode-indent")
    (setq-local swift-mode:parenthesized-expression-offset 8)
    (setq-local swift-mode:multiline-statement-offset 8)
    (setq-local indent-line-function #'swift-mode:indent-line))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal kotlin-mode-map
      (kbd "<tab>") 'j|hs-toggle-node)
    (evil-define-key 'insert kotlin-mode-map
      (kbd "<tab>") 'c-indent-line-or-region)))

(use-package flymake-ktlint
  :ensure nil
  :load-path "~/.emacs.d/fork/flymake-ktlint/"
  :config
  (setq ktlint-flymake-args '("--android"))
  (flymake-ktlint-setup))

(use-package flycheck-kotlin
  ;; brew install shyiko/ktlint/ktlint
  :ensure t
  :config
  (defun ktlint-fix-buffer ()
    "Fix Kotlin buffer using ktlint."
    (interactive)
    (if (not buffer-file-name)
        (user-error "No file for buffer!")
      (compile (format "ktlint -F %s" buffer-file-name))))

  (defun ktlint-fix-files-in-project ()
    "Fix Kotlin files in project with ktlint."
    (interactive)
    (let ((default-directory
            (or (and (fboundp 'projectile-project-root)
                     (projectile-project-root))
                (locate-dominating-file buffer-file-name ".git"))))
      (start-process-shell-command "*ktlint-format*" nil "ktlint -F '*.kt'")))

  (flycheck-kotlin-setup))

(defun j|kotlin-mode ()
  "Bootstrap `jn-kotlin'."
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) 'j|kotlin-mode)
      (setf (cdr alist) 'kotlin-mode)))
  (kotlin-mode))

(provide 'jn-kotlin)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-kotlin.el ends here

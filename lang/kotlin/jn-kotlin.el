;;;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :ensure t)

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

(defun +kotlin-mode ()
  "Bootstrap `jn-kotlin'."
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) '+kotlin-mode)
      (setf (cdr alist) 'kotlin-mode)))
  (kotlin-mode))

(provide 'jn-kotlin)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-kotlin.el ends here

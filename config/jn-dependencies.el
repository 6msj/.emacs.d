;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

(with-eval-after-load 'magit
  (when (file-exists-p "~/.yt-emacs/yt.el")
    (add-to-list 'load-path "~/.yt-emacs/")
    (require 'yt)))

(use-package diminish
  :ensure t
  :config
  (diminish 'subword-mode)
  (diminish 'visual-line-mode)
  (diminish 'abbrev-mode)
  (diminish 'buffer-face-mode)
  (diminish 'eldoc-mode)
  (with-eval-after-load 'autorevert
    (diminish 'auto-revert-mode)))

(use-package async
  :ensure t
  :init
  (defun j-async-dired-on ()
    "Turn on `dired-async-mode' if remote."
    (when (file-remote-p default-directory)
      (dired-async-mode)))
  (add-hook 'dired-sidebar-mode-hook #'j-async-dired-on)
  (add-hook 'dired-mode-hook #'j-async-dired-on))

(use-package dumb-jump
  :ensure t
  :commands (dumb-jump-xref-activate)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package smart-jump
  :load-path "~/.emacs.d/fork/smart-jump"
  :ensure nil
  :commands (smart-jump-go smart-jump-back smart-jump-references)
  :init
  (with-eval-after-load 'general
    (general-define-key
     :states '(normal visual motion)
     :keymaps 'override
     "M-." 'smart-jump-go
     "M-," 'smart-jump-back
     "M-?" 'smart-jump-references)))

(use-package ace-window
  :ensure t
  :commands (ace-delete-window
             ace-swap-window
             ace-delete-other-windows
             ace-window
             aw-select))

(use-package general
  :ensure t
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "SPC" 'matcha-me-space))

(use-package matcha
  :load-path "~/.emacs.d/fork/matcha/"
  :ensure nil
  :config
  (matcha-setup))

(unless WINDOWS-P
  (use-package vterm
    :commands (vterm)
    :ensure t
    :init
    (add-hook 'vterm-mode-hook (lambda ()
                                 (setq-local kill-buffer-query-functions nil)))
    (setq vterm-shell "/bin/zsh")
    (defvar vterm-install t)))

(unless YT-P
  (use-package flycheck-xcode
    :load-path "~/.emacs.d/fork/flycheck-xcode"
    :ensure nil
    :commands (flycheck-xcode-setup)
    :init
    (mapc
     (lambda (x)
       (add-hook x #'flycheck-xcode-setup))
     '(c-mode-hook c++-mode-hook objc-mode-hook swift-mode-hook)))

  (use-package flymake-gradle
    :ensure nil
    :load-path "~/.emacs.d/fork/flymake-gradle/"
    :commands (flymake-gradle-setup flymake-gradle-add-hook)
    :init
    (defun j-flymake-gradle-setup ()
      (flycheck-mode -1)
      (flymake-gradle-add-hook))
    (add-hook 'kotlin-mode-hook #'j-flymake-gradle-setup)
    (add-hook 'java-mode-hook #'j-flymake-gradle-setup))

  (use-package flycheck-gradle
    :load-path "~/.emacs.d/fork/flycheck-gradle"
    :ensure nil
    :commands (flycheck-gradle-setup)
    :init
    (mapc
     (lambda (x)
       (add-hook x #'flycheck-gradle-setup))
     '(java-mode-hook kotlin-mode-hook))
    :config
    (setq flycheck-gradle-java-compile-function
          #'flycheck-gradle-java-compile->android)
    (setq flycheck-gradle-adjust-log-level-automatically t)))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :commands (editorconfig-mode editorconfig-mode-apply)
  :init
  (mapcar
   (lambda (hook)
     (add-hook
      hook
      (lambda ()
        (when (and
               (executable-find "editorconfig")
               (projectile-project-p)
               (file-exists-p
                (expand-file-name ".editorconfig"
                                  (projectile-project-root))))
          (editorconfig-mode-apply)))))
   '(typescript-mode-hook web-mode-hook)))

(use-package flymake-diagnostic-at-point
  :ensure t
  :commands (flymake-diagnostic-at-point-mode)
  :init
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  :config
  (setq flymake-diagnostic-at-point-error-prefix nil))

(unless YT-P
  (use-package eglot
    ;; :load-path "~/Code/eglot/"
    :ensure t
    :commands (eglot eglot-ensure)
    :init
    (defun eglot-disable-other-modes ()
      "Disable other modes that would conflict with `eglot'."
      (mapc (lambda (mode)
              (when (and
                     (boundp mode)
                     (symbol-value mode))
                (funcall mode -1)))
            '(flycheck-mode
              ggtags-mode)))
    (add-hook 'eglot-managed-mode-hook #'eglot-disable-other-modes)

    (defvar eglot-check-modes '((c-mode . "ccls")
                                (c++-mode . "ccls")
                                (go-mode . "gopls")
                                (kotlin-mode . "kotlin-language-server")
                                (python-mode . "pyls")
                                (rust-mode . "rls")))

    (defun eglot-check-mode-and-ensure ()
      "Check and run if `eglot-ensure' should be run in `major-mode'."
      (and
       (assoc major-mode eglot-check-modes)
       (executable-find (cdr (assoc major-mode eglot-check-modes)))
       (eglot-ensure)))

    (dolist (mode-pair eglot-check-modes)
      (add-hook (intern (format "%S-hook" (car mode-pair)))
                #'eglot-check-mode-and-ensure))
    :config
    (setq eglot-events-buffer-size 0)
    (setq eglot-sync-connect 0)))

(provide 'jn-dependencies)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-dependencies.el ends here

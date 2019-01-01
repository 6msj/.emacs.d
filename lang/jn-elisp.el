;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)
(require 'jn-dependencies)

(use-package elisp-mode
  :ensure nil
  :init
  ;; Default *scratch* buffer to lexical binding.
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (when (equal (buffer-name) "*scratch*")
                (setq-local lexical-binding t))))

  (defun j|elisp-recompile-on-save ()
    "If there is a corresponding elc file, recompile after save."
    (when (file-exists-p
           (byte-compile-dest-file buffer-file-name))
      (byte-compile-file buffer-file-name)))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (make-local-variable 'after-save-hook)
              (add-hook 'after-save-hook #'j|elisp-recompile-on-save)))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal emacs-lisp-mode-map
      (kbd "C-j") 'eval-print-last-sexp)
    (evil-define-key 'normal lisp-interaction-mode-map
      (kbd "C-j") 'eval-print-last-sexp))

  (when (boundp 'debugger-stack-frame-as-list)
    (setq debugger-stack-frame-as-list t))

  ;; Prefer newer version of .el vs .elc.
  (setq load-prefer-newer t)

  (setq edebug-trace nil)
  (setq edebug-print-length 80)
  (setq eval-expression-print-length 24)
  (setq eval-expression-print-level 8)
  (with-eval-after-load 'evil
    (setq evil-shift-width lisp-body-indent)))

(use-package elisp-slime-nav
  :ensure t
  :commands (elisp-slime-nav-find-elisp-thing-at-point)
  :diminish elisp-slime-nav-mode
  :init
  ;; TODO: Handle ielm-mode.
  (with-eval-after-load 'evil
    (evil-define-key 'normal lisp-interaction-mode-map
      (kbd "K")  'elisp-slime-nav-describe-elisp-thing-at-point)
    (evil-define-key 'normal emacs-lisp-mode-map
      (kbd "K")  'elisp-slime-nav-describe-elisp-thing-at-point))
  :config
  (defadvice elisp-slime-nav-describe-elisp-thing-at-point (after slime-move-to-doc activate)
    "Move point to the other window after opening up documentation window."
    (pop-to-buffer "*Help*")))

(use-package eval-sexp-fu
  :ensure t
  :commands (eval-sexp-fu-flash-mode)
  :init
  (add-hook 'cider-mode-hook 'eval-sexp-fu-flash-mode)
  (add-hook 'clojure-mode-hook 'eval-sexp-fu-flash-mode)
  (add-hook 'emacs-lisp-mode-hook 'eval-sexp-fu-flash-mode)
  :config
  (defun j|eval-sexp-fu-set-face ()
    "Set `eval-sexp-fu' face."
    (set-face-attribute 'eval-sexp-fu-flash nil
                        :background (face-attribute 'cursor :background)
                        :foreground "black"
                        :bold t
                        :underline t))

  (j|eval-sexp-fu-set-face)
  (add-hook 'after-load-theme-hook #'j|eval-sexp-fu-set-face)

  (eval-sexp-fu-flash-mode 1))

(use-package macrostep
  :ensure t
  :commands (macrostep-mode macrostep-expand))

(use-package elisp-refs
  :ensure t
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-special
             elisp-refs-variable
             elisp-refs-symbol))

(use-package debbugs
  :ensure t
  :commands (debbugs-browse-mode
             debbugs-gnu-search
             debbugs-gnu-patches
             debbugs-gnu
             debbugs-gnu-usertags
             debbugs-gnu-bugs
             debbugs-org-search
             debbugs-org-patches
             debbugs-org
             debbugs-org-mode
             debbugs-org-bugs))

(provide 'jn-elisp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
;;; jn-elisp.el ends here

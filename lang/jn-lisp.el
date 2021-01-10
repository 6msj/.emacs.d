;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)
(require 'jn-dependencies)

(use-package lisp-mode
  :ensure nil
  :mode
  "\\.li?sp\\'"
  "\\.l\\'"
  "\\.lisp\\'"
  "\\.stumpwmrc\\'"
  "\\.stumpish\\'"
  "\\.sbclrc\\'")

;; Common Lisp

(use-package slime
  :ensure t
  :commands slime-lisp-mode-hook
  :init
  (cond
   (LINUX-P
    (setq inferior-lisp-program "/usr/bin/sbcl"))
   (WINDOWS-P
    (setq inferior-lisp-program "sbcl.exe"))
   (t
    (setq inferior-lisp-program "/usr/local/bin/sbcl")))
  (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
  :config
  ;; Part of `slime-setup' function that we don't use to defer `slime' at
  ;; startup.
  (slime--setup-contribs)

  (use-package slime-company
    :ensure t
    :commands (company-slime)
    :init
    (add-hook 'lisp-mode-hook
              (lambda ()
                (j-company-push-backend 'company-slime t)))
    :config
    (setq slime-company-completion 'fuzzy))
  (setq slime-contribs '(slime-asdf
                         slime-fancy
                         slime-indentation
                         slime-sbcl-exts
                         slime-scratch
                         slime-company))
  ;; Enable fuzzy matching in code buffer and SLIME REPL.
  (setq slime-complete-symbol*-fancy t)

  (with-eval-after-load 'slime
    (evil-define-key 'normal slime-mode-map
      "\C-j" #'slime-eval-print-last-expression)))

;; Scheme

(use-package geiser
  :ensure t
  :commands (geiser-mode--maybe-activate geiser-mode)
  :init
  (add-hook 'scheme-mode-hook #'flymake-mode)
  (add-hook 'scheme-mode-hook #'geiser-mode)
  :config
  (setq geiser-mode-start-repl-p t)
  (setq geiser-repl-query-on-kill-p nil))

(use-package flymake-racket
  :ensure nil
  :commands (flymake-racket-add-hook)
  :load-path "~/.emacs.d/fork/flymake-racket/"
  :init
  (add-hook 'scheme-mode-hook #'flymake-racket-add-hook)
  (add-hook 'racket-mode-hook #'flymake-racket-add-hook))

;; Clojure

(use-package clojure-mode
  :ensure t
  :mode
  ("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
  ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)
  ("\\.clj\\'" . clojure-mode)
  ("\\.edn\\'" . clojure-mode)
  ("\\.cljs\\'" . clojurescript-mode)
  ("\\.cljc\\'" . clojurec-mode))

(use-package cider
  :ensure t
  :commands (cider cider-mode cider-jack-in)
  :diminish cider-auto-test-mode
  :init
  (defun j-cider-setup ()
    (cider-mode)
    (clj-refactor-mode)
    (j-company-merge-backends))
  (add-hook 'clojurescript-mode-hook #'j-cider-setup)
  (add-hook 'clojure-mode-hook #'j-cider-setup)
  (add-hook 'cider-repl-mode-hook #'j-cider-setup)
  :config
  (setq cider-mode-line
        '(:eval
          (let ((info (cider--modeline-info)))
            (if (string-equal info "not connected")
                " !Cider"
              (format " Cider[%s]" info)))))

  ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
  (defun cider-figwheel-repl ()
    "Start figwheel and cljs-repl in cider.
Use this in conjunction with `cider-jack-in'.

Unless you remember what you're doing, use `cider-jack-in-clojurescript'
along with `cider-cljs-lein-repl' instead."
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl-buffer)
      (goto-char (point-max))
      (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
      (cider-repl-return)))

  ;; This can be used in conjunction with `cider-jack-in-clojurescript'.
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

  ;; Call cider-run immediately after cider-jack-in connects.
  (add-hook 'cider-connected-hook #'cider-run)

  (evil-define-key 'normal cider-test-report-mode-map
    (kbd "M-.") #'cider-test-jump)

  ;; TODO:
  ;; https://github.com/clojure-emacs/cider/issues/784
  ;; if ns has the string 'test' in it, return the ns
  ;; otherwise do some magic here
  ;; probably need to account for luminus and how they set up the test namespaces
  ;; or maybe just chance luminus namespace
  (defun j-cider-determine-test-ns (ns)
    "Figuring out which namespace to test given the current namespace."
    (if (locate-dominating-file default-directory ".luminus")
        ns
      (cider-test-default-test-ns-fn ns)))
  (setq cider-test-infer-test-ns #'j-cider-determine-test-ns)

  (cider-auto-test-mode)
  (setq cider-test-show-report-on-success nil)
  ;; attempt to automatically look up symbol first
  (setq cider-prompt-for-symbol nil)
  ;; use shift return to get a new line in repl
  (define-key cider-repl-mode-map (kbd "C-j") nil)
  (define-key cider-repl-mode-map [(shift return)] 'cider-repl-newline-and-indent)

  (setq nrepl-log-messages t
        nrepl-prompt-to-kill-server-buffer-on-quit nil
        nrepl-hide-special-buffers t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package flycheck-clojure
  :ensure t
  :after cider
  :config
  (flycheck-clojure-setup))

(use-package clj-refactor
  :ensure t
  :diminish clj-refactor-mode
  :after cider)

;; Elisp

(use-package elisp-mode
  :ensure nil
  :init
  (defun j-elisp-recompile-on-save ()
    "If there is a corresponding elc file, recompile after save."
    (when (file-exists-p
           (byte-compile-dest-file buffer-file-name))
      (byte-compile-file buffer-file-name)))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (make-local-variable 'after-save-hook)
              (add-hook 'after-save-hook #'j-elisp-recompile-on-save)))
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
  (add-hook 'inferior-emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'lisp-interaction-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  :config
  (defadvice elisp-slime-nav-describe-elisp-thing-at-point (after slime-move-to-doc activate)
    "Move point to the other window after opening up documentation window."
    (pop-to-buffer "*Help*")))

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

(provide 'jn-lisp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-commonlisp.el ends here

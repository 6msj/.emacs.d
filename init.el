;;;; -*- lexical-binding: t; -*-

;; (package-initialize)

;; Set up garbage collection and tweak startup settings.
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 402653184
                   gc-cons-percentage 0.6)
             (add-hook
              'focus-out-hook
              (lambda ()
                "Lower `gc-cons-threshold' and then run `garbage-collect'."
                (let ((gc-cons-threshold 800000))
                  (garbage-collect))))) t)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/fork/private_emacs")

;; In Emacs 27, this is handled by `early-init'.
(when (< emacs-major-version 27)
  (load "~/.emacs.d/early-init.el")
  (require 'early-init))

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'jn-private nil t) ;; Private functions.

;; Install package if not existing.
(setq use-package-always-ensure nil)

;; Check loading times with `use-package'.
(setq use-package-verbose t)

(use-package jn-functions :ensure nil)
(use-package jn-dependencies :ensure nil)
(use-package jn-theme :ensure nil)
(use-package jn-defaults :ensure nil)
(use-package jn-platform :ensure nil)
(use-package jn-git :ensure nil)
(use-package jn-misc :ensure nil)
(use-package jn-project :ensure nil)
(use-package jn-editing :ensure nil)
(use-package jn-evil :ensure nil)
(use-package jn-autocomplete :ensure nil)

(use-package jn-java :ensure nil
  :mode ("\\.java\\'" . j|java-mode))

(use-package jn-csharp :ensure nil
  :mode
  ("\\.cs$" . j|csharp-mode)
  ("\\.cs\\'". j|csharp-mode))

(use-package jn-rust :ensure nil
  :mode ("\\.rs\\'" . j|rust-mode))

(use-package jn-swift :ensure nil
  :mode ("\\.swift\\'" . j|swift-mode))

(use-package jn-ruby :ensure nil
  :commands (j|ruby-mode)
  :init
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) 'ruby-mode)
      (setf (cdr alist) 'j|ruby-mode)))
  :interpreter ("ruby" . j|ruby-mode))

(use-package jn-python :ensure nil
  :mode
  ("\\.pyw?\\'" . j|python-mode)
  ("\\.py\\'" . j|python-mode)
  :interpreter ("python" . j|python-mode))

(use-package jn-lua :ensure nil
  :mode
  ("\\.luacheckrc\\'" . j|lua-mode)
  ("\\.lua\\'" . j|lua-mode)
  :interpreter ("lua" . j|lua-mode))

(use-package jn-commonlisp :ensure nil
  :mode
  ("\\.li?sp\\'" . j|commonlisp-mode)
  ("\\.l\\'" . j|commonlisp-mode)
  ("\\.lisp\\'" . j|commonlisp-mode)
  ("\\.stumpwmrc\\'" . j|commonlisp-mode)
  ("\\.stumpish\\'" . j|commonlisp-mode)
  ("\\.sbclrc\\'" . j|commonlisp-mode))

(use-package jn-scheme :ensure nil
  :init
  (remove-hook 'scheme-mode-hook 'geiser-mode--maybe-activate)
  :mode
  ("\\.oak\\'" . j|scheme-mode)
  ("\\.scm\\.[0-9]*\\'" . j|scheme-mode)
  ("\\.\\(scm\\|stk\\|ss\\|sch\\)\\'" . j|scheme-mode)
  ("\\.rkt\\'" . j|scheme-mode)
  ("\\.scm\\'" . j|scheme-mode)
  :interpreter
  ("guile" . j|scheme-mode)
  ("scm" . j|scheme-mode))

(use-package jn-elisp :ensure nil)

(use-package jn-clojure :ensure nil
  :mode
  ("\\.\\(clj\\|dtm\\|edn\\)\\'" . j|clojure-mode)
  ("\\(?:build\\|profile\\)\\.boot\\'" . j|clojure-mode)
  ("\\.clj\\'" . j|clojure-mode)
  ("\\.edn\\'" . j|clojure-mode)
  ("\\.cljs\\'" . j|clojurescript-mode)
  ("\\.cljc\\'" . j|clojurec-mode))

(use-package jn-erlang :ensure nil
  :commands (j|erlang-mode)
  :init
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) 'erlang-mode)
      (setf (cdr alist) 'j|erlang-mode))))

(use-package jn-elixir :ensure nil
  :mode
  ("\\.elixir\\'" . j|elixir-mode)
  ("\\.ex\\'" . j|elixir-mode)
  ("\\.exs\\'" . j|elixir-mode))

(use-package jn-haskell :ensure nil
  :mode
  ("\\.hsc\\'" . j|haskell-mode)
  ("\\.[gh]s\\'" . j|haskell-mode)
  ("\\.hs\\'" . j|haskell-mode))

(use-package jn-groovy :ensure nil
  :mode
  ("\\.gradle\\'" . j|groovy-mode)
  ("\\.groovy\\'" . j|groovy-mode))

(use-package jn-scala :ensure nil
  :commands (j|scala-mode)
  :init
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) 'scala-mode)
      (setf (cdr alist) 'j|scala-mode))))

(use-package jn-kotlin :ensure nil
  :mode
  ("\\.kt$" . j|kotlin-mode)
  ("\\.kts$" . j|kotlin-mode))

(use-package jn-golang :ensure nil
  :mode
  ("\\.go\\'" . j|go-mode))

(use-package jn-web :ensure nil
  :mode
  ("\\.tsx\\'" . j|web-mode)
  ("\\.ts\\'" . j|typescript-mode)
  ("\\.php\\'" . j|php-mode)
  ("\\.js\\'" . j|javascript-mode)
  ("\\.jsx?\\'" . j|javascript-jsx-mode)
  ("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . j|mhtml-mode)
  ("\\.phtml\\'" . j|web-mode)
  ("\\.tpl\\.php\\'" . j|web-mode)
  ("\\.blade\\.php\\'" . j|web-mode)
  ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . j|web-mode)
  ("\\.[agj]sp\\'" . j|web-mode)
  ("\\.as[cp]x\\'" . j|web-mode)
  ("\\.erb\\'" . j|web-mode)
  ("\\.mustache\\'" . j|web-mode)
  ("\\.djhtml\\'" . j|web-mode)
  ("\\.jsp\\'" . j|web-mode)
  ("\\.eex\\'" . j|web-mode))

(use-package jn-c :ensure nil
  :mode
  ("\\.c\\'" . j|c-mode)
  ("\\.lex\\'" . j|c-mode)
  ("\\.y\\(acc\\)?\\'" . j|c-mode)
  ("\\.i\\'" . j|c-mode)
  ("\\.ii\\'" . j|c++-mode)
  ("\\.\\(CC?\\|HH?\\)\\'" . j|c++-mode)
  ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . j|c++-mode)
  ("\\.\\(cc\\|hh\\)\\'" . j|c++-mode)
  ("\\.m\\'" . j|objc-mode)
  ("\\.mm\\'" . j|objc-mode))

(use-package jn-org :ensure nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ac-js2
     ace-jump-mode
     add-node-modules-path
     afternoon-theme
     ag
     alchemist
     alert
     all-the-icons
     all-the-icons-dired
     anaconda-mode
     android-mode
     base16-theme
     bind-map
     cargo
     cider
     cider-eval-sexp-fu
     circe
     circe-notifications
     clang-format
     clj-refactor
     clojure-mode
     cmake-mode
     company
     company-anaconda
     company-go
     company-jedi
     company-lua
     company-quickhelp
     company-racer
     company-shell
     company-statistics
     counsel
     csharp-mode
     deadgrep
     debbugs
     dired-collapse
     dired-hack-utils
     dired-subtree
     doom-themes
     dtrt-indent
     dumb-jump
     dummy-h-mode
     editorconfig
     eglot
     elisp-refs
     elisp-slime-nav
     elixir-mode
     elixir-yasnippets
     emr
     erlang
     esh-autosuggest
     etags-select
     eval-sexp-fu
     evil-commentary
     evil-magit
     evil-matchit
     evil-mc
     evil-numbers
     evil-org
     evil-surround
     evil-visualstar
     exec-path-from-shell
     expand-region
     fd-dired
     flx
     flycheck
     flycheck-clojure
     flycheck-irony
     flycheck-kotlin
     flycheck-mix
     flycheck-pos-tip
     flycheck-posframe
     flycheck-rtags
     flycheck-rust
     flycheck-swift
     flymake-diagnostic-at-point
     focus-autosave-mode
     geiser
     ggtags
     github-theme
     go-dlv
     go-guru
     go-mode
     godoctor
     gotham-theme
     graphviz-dot-mode
     groovy-mode
     haskell-mode
     highlight-symbol
     htmlize
     hungry-delete
     ibuffer-projectile
     indium
     ix
     javadoc-lookup
     js-import
     js2-mode
     js2-refactor
     json-mode
     kotlin-mode
     lispyville
     list-utils
     loop
     love-minor-mode
     lua-mode
     macrostep
     meghanada
     mocha
     modern-cpp-font-lock
     moe-theme
     multi-term
     nlinum
     nlinum-hl
     notmuch
     nov
     omnisharp
     org-plus-contrib
     ox-pandoc
     pass
     password-store
     pbcopy
     powershell
     prettier-js
     prodigy
     projectile
     projectile-rails
     pydoc
     quickrun
     racer
     rainbow-delimiters
     rainbow-mode
     realgud
     restclient
     reveal-in-osx-finder
     rg
     rjsx-mode
     robe
     rust-mode
     scala-mode
     shackle
     slime
     slime-company
     smart-mode-line
     smartparens
     smex
     solarized-theme
     sourcemap
     spacemacs-theme
     super-save
     swift-mode
     theme-changer
     tide
     ts-comint
     typescript-mode
     use-package
     vimrc-mode
     vlf
     web-mode
     wgrep-ag
     which-key
     ws-butler
     xclip
     xref-js2
     xterm-color
     yaml-mode
     yasnippet))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:

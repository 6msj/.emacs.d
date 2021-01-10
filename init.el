;;;; -*- lexical-binding: t; -*-

;; (package-initialize)

(unless (file-exists-p "~/.emacs.d/bootstrapped")
  (message "Bootstrapping up emacs...")
  (shell-command "~/.emacs.d/bootstrap.sh")
  (message "Finished emacs bootstrap."))

;; Set up garbage collection and tweak startup settings.
(defvar file-name-handler-alist-old file-name-handler-alist)

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq message-log-max 16384
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 402653184
                   gc-cons-percentage .3)
             (add-hook
              'focus-out-hook
              (lambda ()
                "Lower `gc-cons-threshold' and then run `garbage-collect'."
                (let ((gc-cons-threshold 800000))
                  (garbage-collect))))) t)

;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this. However, this
;; may cause problems on builds of Emacs where its site lisp files aren't
;; byte-compiled and we're forced to load the *.el.gz files (e.g. on Alpine)
(unless (daemonp)
  (defvar j-initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' later, because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun j-reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because changes to `file-name-handler-alist'
    ;; since startup ought to be preserved.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'j-initial-file-name-handler-alist handler))
    (setq file-name-handler-alist j-initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'j-reset-file-handler-alist-h))

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
(setq use-package-always-ensure t)

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
(use-package jn-lisp :ensure nil)

(use-package jn-csharp :ensure nil
  :mode
  ("\\.cs$" . j-csharp-mode)
  ("\\.cs\\'". j-csharp-mode))

(use-package jn-rust :ensure nil
  :mode ("\\.rs\\'" . j-rust-mode))

(use-package jn-swift :ensure nil
  :mode ("\\.swift\\'" . j-swift-mode))

(use-package jn-ruby :ensure nil
  :commands (j-ruby-mode)
  :init
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) 'ruby-mode)
      (setf (cdr alist) 'j-ruby-mode)))
  :interpreter ("ruby" . j-ruby-mode))

(use-package jn-python :ensure nil
  :mode
  ("\\.pyw?\\'" . j-python-mode)
  ("\\.py\\'" . j-python-mode)
  :interpreter ("python" . j-python-mode))

(use-package jn-lua :ensure nil
  :mode
  ("\\.luacheckrc\\'" . j-lua-mode)
  ("\\.lua\\'" . j-lua-mode)
  :interpreter ("lua" . j-lua-mode))

(use-package jn-erlang :ensure nil
  :commands (j-erlang-mode)
  :init
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) 'erlang-mode)
      (setf (cdr alist) 'j-erlang-mode))))

(use-package jn-elixir :ensure nil
  :mode
  ("\\.elixir\\'" . j-elixir-mode)
  ("\\.ex\\'" . j-elixir-mode)
  ("\\.exs\\'" . j-elixir-mode))

(use-package jn-kotlin :ensure nil
  :mode
  ("\\.kt$" . j-kotlin-mode)
  ("\\.kts$" . j-kotlin-mode))

(use-package jn-golang :ensure nil
  :mode
  ("\\.go\\'" . j-go-mode))

(use-package jn-web :ensure nil
  :mode
  ("\\.tsx\\'" . j-web-mode)
  ("\\.ts\\'" . j-typescript-mode)
  ("\\.php\\'" . j-php-mode)
  ("\\.js\\'" . j-javascript-mode)
  ("\\.jsx?\\'" . j-javascript-jsx-mode)
  ("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . j-mhtml-mode)
  ("\\.phtml\\'" . j-web-mode)
  ("\\.tpl\\.php\\'" . j-web-mode)
  ("\\.blade\\.php\\'" . j-web-mode)
  ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . j-web-mode)
  ("\\.[agj]sp\\'" . j-web-mode)
  ("\\.as[cp]x\\'" . j-web-mode)
  ("\\.erb\\'" . j-web-mode)
  ("\\.mustache\\'" . j-web-mode)
  ("\\.djhtml\\'" . j-web-mode)
  ("\\.jsp\\'" . j-web-mode)
  ("\\.eex\\'" . j-web-mode))

(use-package jn-c :ensure nil
  :mode
  ("\\.c\\'" . j-c-mode)
  ("\\.lex\\'" . j-c-mode)
  ("\\.y\\(acc\\)?\\'" . j-c-mode)
  ("\\.i\\'" . j-c-mode)
  ("\\.ii\\'" . j-c++-mode)
  ("\\.\\(CC?\\|HH?\\)\\'" . j-c++-mode)
  ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . j-c++-mode)
  ("\\.\\(cc\\|hh\\)\\'" . j-c++-mode)
  ("\\.java\\'" . j-java-mode)
  ("\\.m\\'" . j-objc-mode)
  ("\\.mm\\'" . j-objc-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ac-js2
     add-node-modules-path
     afternoon-theme
     alchemist
     annalist
     base16-theme
     bind-map
     cargo
     cider
     circe
     clang-format
     clj-refactor
     clojure-mode
     cmake-mode
     company
     company-go
     company-jedi
     company-lua
     company-quickhelp
     company-racer
     company-shell
     company-statistics
     counsel
     csharp-mode
     csv-mode
     deadgrep
     dired-collapse
     dired-hack-utils
     dired-subtree
     doom-themes
     dtrt-indent
     dumb-jump
     editorconfig
     eglot
     elisp-refs
     elisp-slime-nav
     elixir-mode
     elixir-yasnippets
     erlang
     etags-select
     evil-commentary
     evil-matchit
     evil-numbers
     evil-org
     evil-surround
     evil-visualstar
     exec-path-from-shell
     expand-region
     flx
     flycheck
     flycheck-clojure
     flycheck-kotlin
     flycheck-mix
     flycheck-rust
     flycheck-swift
     flymake-diagnostic-at-point
     focus-autosave-mode
     geiser
     go-dlv
     go-guru
     go-mode
     godoctor
     google-translate
     groovy-mode
     haskell-mode
     highlight-symbol
     hungry-delete
     ibuffer-projectile
     js-import
     js2-mode
     js2-refactor
     json-mode
     kaolin-themes
     kotlin-mode
     lispyville
     list-utils
     loop
     love-minor-mode
     lua-mode
     macrostep
     modern-cpp-font-lock
     multi-term
     nov
     org-plus-contrib
     pass
     password-store
     pbcopy
     php-mode
     powershell
     prettier-js
     prodigy
     projectile
     projectile-rails
     racer
     rainbow-delimiters
     rainbow-mode
     restclient
     reveal-in-osx-finder
     rjsx-mode
     robe
     rust-mode
     scala-mode
     shackle
     slime
     slime-company
     smart-mode-line
     smartparens
     solarized-theme
     sourcemap
     spacemacs-theme
     super-save
     swift-mode
     terminal-here
     tide
     transient
     typescript-mode
     undo-fu
     use-package
     vc-defer
     vc-hgcmd
     vterm
     vimrc-mode
     vlf
     web-mode
     ws-butler
     xclip
     xref-js2
     xterm-color
     yaml-mode
     yasnippet)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:

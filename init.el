;;;; -*- lexical-binding: t; -*-

;; (package-initialize)

;; Set up garbage collection rules.
;; Set a high value so there's no noticeable garbage collection.
(setq gc-cons-threshold 100000000)
(setq garbage-collection-messages t)

(defun +gc-lower-limit-then-collect ()
  "Lower `gc-cons-threshold' and then run `garbage-collect'."
  (let ((gc-cons-threshold 800000))
    (garbage-collect)))

(add-hook 'focus-out-hook '+gc-lower-limit-then-collect)

;; Separate package directories according to Emacs version.
;; Bytecode compiled in different Emacs versions are not
;; guaranteed to work with another.
(setq package-user-dir
      (format "%selpa/%s/" user-emacs-directory emacs-major-version))

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/fork/private_emacs")

;; Disable in favor of `use-package'.
(setq package-enable-at-startup nil)

;; Package Repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(setq package-archive-priorities '(("org" . 15)
                                   ("melpa" . 10)
                                   ("melpa-stable" . 5)
                                   ("gnu" . 1)))

;; Activate all packages (in particular autoloads).
(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key) ;; for :bind
(require 'jn-private nil t) ;; Private functions.

;; Install package if not existing.
(setq use-package-always-ensure nil)

;; Check loading times with `use-package'.
(setq use-package-verbose t)

(let ((file-name-handler-alist nil))
  (use-package jn-functions :ensure nil)
  (use-package jn-dependencies :ensure nil)
  (use-package jn-theme :ensure nil)
  (use-package jn-defaults :ensure nil)
  (use-package jn-platform :ensure nil)
  (use-package jn-git :ensure nil)
  (use-package jn-misc :ensure nil)
  (use-package jn-editing :ensure nil)
  (use-package jn-project :ensure nil)
  (use-package jn-evil :ensure nil)
  (use-package jn-autocomplete :ensure nil)

  (use-package jn-java :ensure nil
    :mode ("\\.java\\'" . +java-mode))

  (use-package jn-csharp :ensure nil
    :mode
    ("\\.cs$" . +csharp-mode)
    ("\\.cs\\'". +csharp-mode))

  (use-package jn-rust :ensure nil
    :mode ("\\.rs\\'" . +rust-mode))

  (use-package jn-swift :ensure nil
    :mode ("\\.swift\\'" . +swift-mode))

  (use-package jn-ruby :ensure nil
    :commands (+ruby-mode)
    :init
    (dolist (alist auto-mode-alist)
      (when (eq (cdr alist) 'ruby-mode)
        (setf (cdr alist) '+ruby-mode)))
    :interpreter ("ruby" . +ruby-mode))

  (use-package jn-python :ensure nil
    :mode
    ("\\.pyw?\\'" . +python-mode)
    ("\\.py\\'" . +python-mode)
    :interpreter ("python" . +python-mode))

  (use-package jn-lua :ensure nil
    :mode
    ("\\.luacheckrc\\'" . +lua-mode)
    ("\\.lua\\'" . +lua-mode)
    :interpreter ("lua" . +lua-mode))

  (use-package jn-commonlisp :ensure nil
    :mode
    ("\\.li?sp\\'" . +commonlisp-mode)
    ("\\.l\\'" . +commonlisp-mode)
    ("\\.lisp\\'" . +commonlisp-mode)
    ("\\.stumpwmrc\\'" . +commonlisp-mode)
    ("\\.stumpish\\'" . +commonlisp-mode)
    ("\\.sbclrc\\'" . +commonlisp-mode))

  (use-package jn-scheme :ensure nil
    :init
    (remove-hook 'scheme-mode-hook 'geiser-mode--maybe-activate)
    :mode
    ("\\.oak\\'" . +scheme-mode)
    ("\\.scm\\.[0-9]*\\'" . +scheme-mode)
    ("\\.\\(scm\\|stk\\|ss\\|sch\\)\\'" . +scheme-mode)
    ("\\.rkt\\'" . +scheme-mode)
    ("\\.scm\\'" . +scheme-mode)
    :interpreter
    ("guile" . +scheme-mode)
    ("scm" . +scheme-mode))

  (use-package jn-elisp :ensure nil)

  (use-package jn-clojure :ensure nil
    :mode
    ("\\.\\(clj\\|dtm\\|edn\\)\\'" . +clojure-mode)
    ("\\(?:build\\|profile\\)\\.boot\\'" . +clojure-mode)
    ("\\.clj\\'" . +clojure-mode)
    ("\\.edn\\'" . +clojure-mode)
    ("\\.cljs\\'" . +clojurescript-mode)
    ("\\.cljx\\'" . +clojurex-mode)
    ("\\.cljc\\'" . +clojurec-mode))

  (use-package jn-erlang :ensure nil
    :commands (+erlang-mode)
    :init
    (dolist (alist auto-mode-alist)
      (when (eq (cdr alist) 'erlang-mode)
        (setf (cdr alist) '+erlang-mode))))

  (use-package jn-elixir :ensure nil
    :mode
    ("\\.elixir\\'" . +elixir-mode)
    ("\\.ex\\'" . +elixir-mode)
    ("\\.exs\\'" . +elixir-mode))

  (use-package jn-haskell :ensure nil
    :mode
    ("\\.hsc\\'" . +haskell-mode)
    ("\\.[gh]s\\'" . +haskell-mode)
    ("\\.hs\\'" . +haskell-mode))

  (use-package jn-groovy :ensure nil
    :mode
    ("\\.gradle\\'" . +groovy-mode)
    ("\\.groovy\\'" . +groovy-mode))

  (use-package jn-scala :ensure nil
    :commands (+scala-mode)
    :init
    (dolist (alist auto-mode-alist)
      (when (eq (cdr alist) 'scala-mode)
        (setf (cdr alist) '+scala-mode))))

  (use-package jn-kotlin :ensure nil
    :mode
    ("\\.kt$" . +kotlin-mode)
    ("\\.kts$" . +kotlin-mode))

  (use-package jn-web :ensure nil
    :mode
    ("\\.tsx\\'" . +web-mode)
    ("\\.ts\\'" . +typescript-mode)
    ("\\.php\\'" . +php-mode)
    ("\\.js\\'" . +javascript-mode)
    ("\\.jsx?\\'" . +javascript-jsx-mode)
    ("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . +mhtml-mode)
    ("\\.phtml\\'" . +web-mode)
    ("\\.tpl\\.php\\'" . +web-mode)
    ("\\.blade\\.php\\'" . +web-mode)
    ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . +web-mode)
    ("\\.[agj]sp\\'" . +web-mode)
    ("\\.as[cp]x\\'" . +web-mode)
    ("\\.erb\\'" . +web-mode)
    ("\\.mustache\\'" . +web-mode)
    ("\\.djhtml\\'" . +web-mode)
    ("\\.jsp\\'" . +web-mode)
    ("\\.eex\\'" . +web-mode))

  (use-package jn-extra-lang :ensure nil)

  (use-package jn-c :ensure nil
    :mode
    ("\\.c\\'" . +c-mode)
    ("\\.lex\\'" . +c-mode)
    ("\\.y\\(acc\\)?\\'" . +c-mode)
    ("\\.i\\'" . +c-mode)
    ("\\.ii\\'" . +c++-mode)
    ("\\.\\(CC?\\|HH?\\)\\'" . +c++-mode)
    ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . +c++-mode)
    ("\\.\\(cc\\|hh\\)\\'" . +c++-mode)
    ("\\.m\\'" . +objc-mode)
    ("\\.mm\\'" . +objc-mode))

  (use-package jn-org :ensure nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (nov realgud farmhouse-theme super-save flycheck-rust company-racer racer flycheck-kotlin kotlin-mode debbugs scala-mode flycheck-swift meghanada company-eshell-autosuggest xterm-color xclip sourcemap solarized-theme org-plus-contrib notmuch nlinum-hl nlinum loop list-utils imenu-anywhere hungry-delete groovy-mode graphviz-dot-mode flycheck-ycmd flycheck-clojure evil-org evil-numbers evil-mc elisp-refs dumb-jump doom-themes dired-subtree dired-hack-utils circe-notifications base16-theme all-the-icons-dired add-node-modules-path p4 expand-region ibuffer-projectile macrostep dired-collapse pass password-store highlight-symbol mocha editorconfig zerodark-theme wgrep-ag rg ac-js2 ace-jump-mode afternoon-theme ag alchemist alert all-the-icons anaconda-mode android-mode bind-map cider cider-eval-sexp-fu circe clang-format clj-refactor clojure-mode cmake-mode color-theme-solarized company company-anaconda company-dict company-irony-c-headers company-jedi company-lua company-quickhelp company-rtags company-shell company-sourcekit company-statistics company-tern company-ycmd counsel csharp-mode dtrt-indent dummy-h-mode edebug-x elisp-slime-nav elixir-mode elixir-yasnippets erlang etags-select eval-sexp-fu evil-commentary evil-ediff evil-magit evil-matchit evil-surround evil-visualstar exec-path-from-shell flx flycheck flycheck-irony flycheck-mix flycheck-pos-tip flycheck-rtags focus-autosave-mode fold-dwim-org fzf geiser ggtags github-theme gotham-theme haskell-mode htmlize indium ivy-rtags ix javadoc-lookup js-import js2-mode js2-refactor json-mode lispyville love-minor-mode lua-mode malinka modern-cpp-font-lock moe-theme motion-mode multi-term neotree omnisharp pbcopy powershell prettier-js prodigy projectile projectile-rails rainbow-delimiters rainbow-mode restclient reveal-in-osx-finder rjsx-mode robe rtags rust-mode shackle slime slime-company smart-mode-line smartparens smex spacemacs-theme speed-type stumpwm-mode swift-mode theme-changer tide ts-comint typescript-mode use-package vimrc-mode vlf web-mode ws-butler xref-js2 yaml-mode yasnippet))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:

;;;; Begin Init

(defun on-windows ()
  "Are we on windows?"
  (eq system-type 'windows-nt))

(defun on-osx ()
  "Are we on osx?"
  (eq system-type 'darwin))

(defun on-linux ()
  "Are we on linux?"
  (eq system-type 'linux))

;; use display pixels to determine device
(setq display-width (display-pixel-width))

(defun on-macbook-retina ()
  "Are we on macbook?"
  (eq display-width 1440))

(defun on-imac ()
  "Are we on imac?"
  (> display-width 4400))

;; increase memory
(setq gc-cons-threshold 100000000) ; 100 mb

;;; loadpath
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(if (on-windows)
    (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin/"))

(add-to-list 'load-path "~/.emacs.d/packages/")

(setq inhibit-startup-screen t) ; disable startup screen
(setq ring-bell-function #'ignore) ; mute system sound

;;; packages

;; M-x list-packages U x to upgrade packages
(setq package-list '(diminish))

(setq package-enable-at-startup nil)

;;; repositories
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; windows seems to hang on marmalade
(unless (on-windows)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/")))

(package-initialize) ; activate all packages (in particular autoloads)

;; bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish) ; for :diminish
(require 'bind-key) ; for :bind

(setq use-package-always-ensure t) ; install package if not existing
(setq use-package-verbose t) ; check loading times

;; fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; set the shell environment properly
(when (on-osx)
  (use-package exec-path-from-shell
    :config
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

(use-package multi-term
  :if (not (on-windows))
  :commands (multi-term)
  :init
  :config
  (add-to-list 'term-unbind-key-list "C-q") ; C-q binds to raw input by default
  (setq multi-term-program "/bin/zsh"))

(use-package dash)
(use-package s)

;;;; End Init

;;;; Begin Theme

;; default frame size
(when (on-macbook-retina)
  (setq initial-frame-alist '((width . 90) (height . 45))))

(when (on-imac)
  (setq initial-frame-alist '((width . 132) (height . 86))))

(use-package color-theme-solarized
  :defer)

(use-package spacemacs-theme
  :defer
  :init
  (setq spacemacs-theme-comment-bg nil))

(setq frame-title-format '("%f")) ; set the title to be the current file

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; https://stackoverflow.com/questions/9840558/why-cant-emacs-24-find-a-custom-theme-i-added
;; adds wildcard matching to themes in elpa folder.
(-each
    (-map
     (lambda (item)
       (format "~/.emacs.d/elpa/%s" item))
     (-filter
      (lambda (item) (s-contains? "theme" item))
      (directory-files "~/.emacs.d/elpa/")))
  (lambda (item)
    (add-to-list 'custom-theme-load-path item)))

(use-package smart-mode-line
  :config
  (when (on-windows)
    (add-to-list 'sml/replacer-regexp-list '("C:/Users/james/Developer/" ":DEV:"))
    (add-to-list 'sml/replacer-regexp-list '("C:/Users/james/Dropbox/Notes/" ":NOTES:"))
    (add-to-list 'sml/replacer-regexp-list '("C:/Users/james/Dropbox/" ":DB:")))
  (unless (on-windows)
    (add-to-list 'sml/replacer-regexp-list '("^~/Developer/" ":DEV:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/Notes/" ":NOTES:")))
  (setq sml/mode-width 'full)
  (setq sml/name-width 30)
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (smart-mode-line-enable))

(use-package theme-changer
  :after (spaceline-config smart-mode-line)
  :init
  (setq calendar-location-name "Dallas, TX")
  (setq calendar-latitude 32.85)
  (setq calendar-longitude -96.85)
  :config
  (defvar using-solarized-theme t "flag to switch between spacemacs and solarized")
  (defun reset-line--change-theme (&rest args)
    (when using-solarized-theme
      (update-solarized-background))
    (when (fboundp 'powerline-reset)
      (powerline-reset)))
  (advice-add 'change-theme :after #'reset-line--change-theme)

  (defun is-daytime()
    "Figuring out day or night."
    (let*
        ((now (current-time))
         (today-times    (sunrise-sunset-times (today)))
         (tomorrow-times (sunrise-sunset-times (tomorrow)))
         (sunrise-today (first today-times))
         (sunset-today (second today-times))
         (sunrise-tomorrow (first tomorrow-times)))
      (daytime-p sunrise-today sunset-today)))

  (defun use-dark-theme()
    (is-daytime))

  (defun update-solarized-background (&optional frame)
    "Update a few ui elements related to the solarized background.
This should be called after (change-theme) when used with the theme-changer package.
Otherwise the symbol 'solarized won't yet be defined."
    (when (bound-and-true-p org-mode)
      (when using-solarized-theme
        (customize-org-mode-solarized))
      (org-reload)
      (message "org-mode reloaded"))
    (if (use-dark-theme)
        (set-frame-parameter frame 'background-mode 'dark)
      (set-frame-parameter frame 'background-mode 'light))
    (enable-theme 'solarized))

  ;; set up solarized background for new frames created
  (add-hook 'after-make-frame-functions (lambda (frame)
                                          (update-solarized-background frame)))

  (if (not using-solarized-theme)
      (change-theme 'spacemacs-light 'spacemacs-dark)
    (progn
      (change-theme 'solarized 'solarized)
      (update-solarized-background))))

;; disable ui fluff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; save window configurations
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; colorful delimiters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; diminish modeline clutter
(when (require 'diminish nil 'noerror)
  (diminish 'subword-mode)
  (diminish 'visual-line-mode)
  (diminish 'abbrev-mode)
  (eval-after-load "hideshow"
    '(diminish 'hs-minor-mode))
  (eval-after-load "autorevert"
    '(diminish 'auto-revert-mode)))

;;;; End Theme

;;;; Begin Platform

;;; Windows Specifc
(when (on-windows)
  (set-face-attribute 'default nil :font "Consolas-10")
  (defun explorer ()
    (interactive)
    (cond
     ;; in buffers with file name
     ((buffer-file-name)
      (shell-command
       (concat "start explorer /e,/select,\""
               (replace-regexp-in-string "/" "\\\\" (buffer-file-name)) "\"")))
     ;; in dired mode
     ((eq major-mode 'dired-mode)
      (shell-command
       (concat "start explorer /e,\""
               (replace-regexp-in-string "/" "\\\\" (dired-current-directory)) "\"")))
     ;; in eshell mode
     ((eq major-mode 'eshell-mode)
      (shell-command
       (concat "start explorer /e,\""
               (replace-regexp-in-string "/" "\\\\" (eshell/pwd)) "\"")))
     ;; use default-directory as last resource
     (t
      (shell-command
       (concat "start explorer /e,\""
               (replace-regexp-in-string "/" "\\\\" default-directory) "\"")))))
  (global-set-key (kbd "s-j") 'explorer))

;;; Mac Specific
;; https://github.com/adobe-fonts/source-code-pro
(when (on-osx)
  (defun find-and-set-font (&rest candidates)
    "Set the first font found in CANDIDATES."
    (let ((font (cl-find-if (lambda (f) (find-font (font-spec :name f)))
                            candidates)))
      (when font
        (set-face-attribute 'default nil :font font))
      font))
  (if (on-macbook-retina)
      (find-and-set-font
       "Source Code Pro-12"
       "Monaco-11"
       "Menlo-11")
    (find-and-set-font
     "Menlo-11"
     "Consolas-12"
     "DejaVu Sans Mono-11"
     "Source Code Pro-12"
     "Envy Code R-12"))

  ;; use the osx emoji font for emoticons
  (when (fboundp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      '(#x1F600 . #x1F64F)
                      (font-spec :name "Apple Color Emoji") nil 'prepend))

  (defvar osx-use-option-as-meta t) ; flag to disable meta

  ;; command key is super
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)

  ;; treat option key as meta
  (when osx-use-option-as-meta
    (setq mac-option-key-is-meta t)
    (setq mac-option-modifier 'meta))

  ;; some key bindings to match osx
  (global-set-key (kbd "s-f") 'evil-search-forward)
  (global-set-key (kbd "s-F") 'evil-search-backward)
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-c") 'evil-yank)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-n") 'make-frame)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "s-s")
                  (lambda ()
                    (interactive)
                    (call-interactively (key-binding "\C-x\C-s"))))
  (if window-system
      (menu-bar-mode 1)) ; mac needs a menu bar

  ;; reveal in finder
  (use-package reveal-in-osx-finder
    :commands (reveal-in-osx-finder)
    :config
    (global-set-key (kbd "s-r") 'reveal-in-osx-finder)))

(when (on-linux)
  (set-face-attribute 'default nil :family "Inconsolata For Powerline")
  (set-face-attribute 'default nil :height 130)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding")))

;;;; End Platform

;;;; Begin Experience

(use-package magit
  :commands (magit-autoload-commands)
  :config
  (setq magit-repository-directories '("~/Developer"
                                       "~/.emacs.d"
                                       "~/.vim"
                                       "~/.dotfiles"
                                       "~/.zsh"))
  (setq magit-refresh-status-buffer nil)
  (setq magit-completing-read-function 'ivy-completing-read)

  ;; https://github.com/magit/magit/issues/2541 (tweaked)
  ;; single window or special magit modes -> open in other window
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer
           (cond
            ((eq (count-windows) 1)
             nil)
            ((and (derived-mode-p 'magit-mode)
                  (eq (with-current-buffer buffer major-mode)
                      'magit-status-mode))
             nil)
            ((memq (with-current-buffer buffer major-mode)
                   '(magit-process-mode
                     magit-revision-mode
                     magit-diff-mode
                     magit-stash-mode))
             nil)
            (t
             '(display-buffer-same-window)))))))

;; prefer vertical splits
;; https://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 150)

;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
      scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ; wraps line when it reaches end

(use-package git-gutter+
  :diminish git-gutter+-mode
  :commands (global-git-gutter+-mode)
  :init
  (add-hook 'after-save-hook (lambda ()
                               (unless global-git-gutter+-mode
                                 (global-git-gutter+-mode)))))

(use-package git-gutter-fringe+
  :after git-gutter+)

;; hide the comments too when you do a 'hs-hide-all'
(setq hs-hide-comments nil)
;; Set whether isearch opens folded comments, code, or both
;; where x is code, comments, t (both), or nil (neither)
(setq hs-isearch-open 'x)

;;; line numbers
(use-package linum
  :config
  (global-linum-mode 1))

;;; highlight parentheses
(use-package paren
  :config
  (show-paren-mode t))

;; syntax check
(use-package flycheck
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp))
  :commands flycheck-mode)

;;;; End Experience

;;;; Begin Completion

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode 1))

;;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-expand
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file)
  :after company
  :config
  ;; yas messages stretches the status buffer when it starts up
  (setq yas-verbosity 2)
  ;; using yasnippet through company mode, so disable all the binds
  (define-key yas-minor-mode-map (kbd "C-i") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map [(tab)] 'nil)
  (define-key yas-minor-mode-map "\C-c&\C-s" nil)
  (define-key yas-minor-mode-map "\C-c&\C-n" nil)
  (define-key yas-minor-mode-map "\C-c&\C-v" nil)
  (yas-global-mode 1))

;;; company
(use-package company
  :defer 1
  :diminish company-mode
  :commands (global-company-mode) ; important so other packages can start company on demand
  :init

  ;; changing prefix lengths depending on mode
  (defun set-company-min-prefix-length (len)
    (make-local-variable 'company-minimum-prefix-length)
    (setq company-minimum-prefix-length len))
  (add-hook 'eshell-mode-hook (apply-partially #'set-company-min-prefix-length 5))
  (add-hook 'term-mode-hook (apply-partially #'set-company-min-prefix-length 5))
  (add-hook 'prog-mode-hook (apply-partially #'set-company-min-prefix-length 1))

  ;; ios
  (add-hook 'objc-mode-hook
            (lambda ()
              (if company-backends
                  (add-to-list 'company-backends 'company-xcode)
                (set (make-local-variable 'company-backends) '(company-xcode)))
              (company/merge-backends)))
  :config
  ;; add additional backend support for all company backends
  ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  ;; https://stackoverflow.com/questions/134887/when-to-use-quote-in-lisp
  (defun merge-backend-with-company-backends (backend-to-merge)
    "Merges a backend with every backend in company-backends.
The backend will only be merged if it's not already being used in the current backend.
We do this because so that the backend we're merging will always be part of the completion candidates.
For example, merging company-yasnippet to company-capf will yield (company-capf :with company-yasnippet)."

    ;; create a list of backend-to-merge with a count equal to company-backends
    ;; this is so mapcar* can iterate over both lists equally
    ;; ex. if we have (company-capf company-xcode), then the list is (company-yasnippet company-yasnippet)
    (setq blist (make-list (list-length company-backends) backend-to-merge))
    ;; b will be backend-to-merge
    ;; backend will be a backend from company-backends
    (setq company-backends (mapcar* (lambda (backend b)
                                      (if (and (listp backend) (member b backend))
                                          backend
                                        (append (if (consp backend)
                                                    backend
                                                  (list backend))
                                                (if (member :with backend)
                                                    `(,b)
                                                  `(:with ,b))))) company-backends blist)))

  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defvar company-mode/enable-dabbrev nil
    "Enable dabbrev for all backends.")
  (defvar company-mode/enable-dabbrev-code t
    "Enable dabbrev-code for all backends.")
  (defun company/merge-backends ()
    (when company-mode/enable-yas
      (merge-backend-with-company-backends 'company-yasnippet))
    (when company-mode/enable-dabbrev
      (merge-backend-with-company-backends 'company-dabbrev))
    (when company-mode/enable-dabbrev-code
      (merge-backend-with-company-backends 'company-dabbrev-code)))
  (company/merge-backends)

  ;; if the completion is JoJo, typing jojo will get to it
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t) ; default is keep-prefix

  ;; use tab to cycle selection
  ;; https://github.com/company-mode/company-mode/issues/216
  ;; https://github.com/company-mode/company-mode/issues/75
  ;; https://github.com/company-mode/company-mode/issues/246#issuecomment-68538735
  (setq company-auto-complete nil)
  (define-key company-active-map [backtab] 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map [S-tab] 'company-select-previous)
  (define-key company-active-map [S-iso-lefttab] 'company-select-previous)
  (define-key company-active-map [(shift tab)] 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)

  ;; loop completion selections
  (setq company-selection-wrap-around t)

  (global-company-mode)
  (setq company-idle-delay .1)
  (setq company-minimum-prefix-length 1))

;; documentation popup for company
(use-package company-quickhelp
  :after company
  :config
  (setq company-quickhelp-delay 3)
  (company-quickhelp-mode 1))

;;; python - jedi
(use-package company-jedi
  :commands (my/python-mode-hook)
  :init
  (defun my/python-mode-hook ()
    (unless (global-company-mode)
      (global-company-mode))
    (add-to-list 'company-backends 'company-jedi)
    (company/merge-backends))
  (add-hook 'python-mode-hook #'my/python-mode-hook))

;;;  c# - omnisharp
(use-package omnisharp
  :commands (omnisharp-mode)
  :init
  (defun my/csharp-mode-hook ()
    (unless (global-company-mode)
      (global-company-mode))
    (omnisharp-mode)
    (add-to-list 'company-backends 'company-omnisharp)
    (company/merge-backends))
  (add-hook 'csharp-mode-hook #'my/csharp-mode-hook)
  :config
  ;; https://stackoverflow.com/questions/29382137/omnisharp-on-emacs-speed
  ;; disable for speed
  (setq omnisharp-eldoc-support nil))

;;;; End Completion

;;;; Begin Mappings

;; another way to use meta
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "C-c o") 'occur) ;; occur!!
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file) ;; rename

;;;; End Mappings

;;;; Begin Editing

(transient-mark-mode 1) ; enable transient mark mode

(setq kill-whole-line t) ; kills entire line if at the beginning
(fset 'yes-or-no-p 'y-or-n-p) ; yes or no to y or n
(column-number-mode 1) ; makes the column number show up

;; add auto indent to all programming modes
;; doing this after init for the first install
(add-hook 'after-init-hook
          (function (lambda ()
                      (add-hook 'prog-mode-hook 'set-newline-and-indent))))

;;; indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-default-style "k&r"
      c-basic-offset 4)

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;;; folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

(use-package fold-dwim-org
  ;; package only used for shift-tab folding
  :init
  (setq fold-dwim-org-strict nil)
  (add-hook 'prog-mode-hook 'fold-dwim-org/minor-mode))

(global-auto-revert-mode t) ; automatically reload buffers on change

;; automatic pairs
(electric-pair-mode 1)

;;; clipboards
;; for linux
(use-package xclip
  :if (on-linux)
  :config
  (xclip-mode 1))

;; for mac
(use-package pbcopy
  :if (and (not window-system) (on-osx))
  :config
  (turn-on-pbcopy))

;;;; End Editing

;;;; Begin Navigation

;; windmove & framemove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

(use-package framemove
  :config
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

;; navigating splits similar to tmux config
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q -") 'split-window-below)
(global-set-key (kbd "C-q C--") 'split-window-below)
(global-set-key (kbd "C-q |") 'split-window-right)
(global-set-key (kbd "C-q \\") 'split-window-right)
(global-set-key (kbd "C-q C-\\") 'split-window-right)
(global-set-key (kbd "C-q h") 'windmove-left)
(global-set-key (kbd "C-q l") 'windmove-right)
(global-set-key (kbd "C-q k") 'windmove-up)
(global-set-key (kbd "C-q j") 'windmove-down)
(global-set-key (kbd "C-q C-h") 'windmove-left)
(global-set-key (kbd "C-q C-l") 'windmove-right)
(global-set-key (kbd "C-q C-k") 'windmove-up)
(global-set-key (kbd "C-q C-j") 'windmove-down)
(global-set-key (kbd "C-q x") 'delete-window)
(global-set-key (kbd "C-q C-x") 'delete-window)
(global-set-key (kbd "C-q 0") 'kill-this-buffer)

(use-package perspective
  :defer 2
  :init
  (setq persp-mode-prefix-key (kbd "C-q"))
  :config
  ;; default blue doesn't look right with smartline
  ;; use default font color instead
  (custom-theme-set-faces
   'user
   (let ((base-font-color (face-foreground 'default nil 'default)))
     `(persp-selected-face ((t (:foreground ,base-font-color))))))

  (defun delete-perspective-or-window()
    "Delete perspective if last window left but delete window if more than one."
    (interactive)
    (if (and (eq (count-windows) 1) (> (hash-table-count perspectives-hash) 1))
        (persp-kill (persp-name persp-curr))
      (delete-window)))

  (define-key perspective-map (kbd "k") nil) ; keep up windmove-up instead
  (define-key perspective-map (kbd "s") 'nil) ; this was persp-switch before
  (define-key perspective-map (kbd "c") 'persp-switch) ; mirroring tmux
  (define-key perspective-map (kbd "x") 'delete-perspective-or-window)
  (define-key perspective-map (kbd ",") 'persp-rename)
  (persp-mode 1))

;;;; End Navigation

;;;; Begin File Management

(use-package swiper
  :ensure counsel
  :diminish ivy-mode
  :config
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- .") ; default: "ag --nocolor --nogroup %s -- ."
  (setq ivy-count-format "")
  (setq ivy-height 15))

(use-package smex
  :bind (("M-x" . smex))
  :config
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(use-package ag
  :config
  (add-to-list 'ag-arguments "-U") ;; ignore .ignore files by default
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)) ; silver searcher

;;; projectile
(use-package projectile
  :commands (projectile-find-file
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-ag)
  :diminish projectile-mode
  :init
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

;;; saving
(setq auto-save-default nil) ; no autosave

;;; backups
;; write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; don't make backups of files in version control
(setq vc-make-backup-files nil)

;;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-mode 'both) ; for buffers and files

(setq confirm-nonexistent-file-or-buffer nil) ; do not confirm a new file or buffer
(setq ido-enable-tramp-completion t) ; tramp completion

(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-last-directory-history nil)
(setq ido-confirm-unique-completion nil) ; wait for RET, even for unique?
(setq ido-use-filename-at-point t) ; prefer file names near point

;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; flx matching
(use-package flx-ido
  :init
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-use-faces nil))

;; same filenames get the directory name inserted also
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;;; most recently used files
(use-package recentf
  :config
  (setq recentf-max-saved-items 300
        ;; https://www.reddit.com/r/emacs/comments/3g468d/stop_recent_files_showing_elpa_packages/
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; git contents
                              "/elpa/.*\\'"   ; package files
                              ".*\\.gz\\'"
                              "TAGS"
                              ".*-autoloads\\.el\\'"))
  (recentf-mode 1))

;;;; End File Management

;;;; Begin Evil

;;; vim style undo
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t) ; regain scroll up with c-u

  ;; indenting related to evil
  (add-hook 'python-mode-hook
            (function (lambda ()
                        (setq evil-shift-width python-indent))))
  (add-hook 'ruby-mode-hook
            (function (lambda ()
                        (setq evil-shift-width ruby-indent-level))))
  (add-hook 'emacs-lisp-mode-hook
            (function (lambda ()
                        (setq evil-shift-width lisp-body-indent))))
  (add-hook 'org-mode-hook
            (function (lambda ()
                        (setq evil-shift-width 4))))
  :config

  ;; package mode bindings
  (evil-add-hjkl-bindings package-menu-mode 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous)

  (setq evil-flash-delay 8) ;; control the highlight time of searches

  (defun setup-lisp-interaction-leader-keys ()
    "Sets up commands for emacs-lisp."
    (evil-leader/set-key-for-mode 'lisp-interaction-mode
      "er" 'eval-region
      "ee" 'eval-last-sexp
      "ex" 'eval-last-sexp-and-replace
      "eb" 'eval-buffer))

  (defun setup-emacs-lisp-leader-keys ()
    "Sets up commands for emacs-lisp."
    (evil-leader/set-key-for-mode 'emacs-lisp-mode
      "er" 'eval-region
      "ee" 'eval-last-sexp
      "ex" 'eval-last-sexp-and-replace
      "eb" 'eval-buffer))

  (defun setup-clojure-leader-keys ()
    "Sets up commands for cider/clojure."
    ;; http://emacs.stackexchange.com/questions/20779/m-and-m-in-evil-mode
    (evil-leader/set-key-for-mode 'clojure-mode
      "."  'cider-find-dwim
      ","  'cider-pop-back
      "cj" 'cider-jack-in
      "ct" 'cider-test-run-test
      "er" 'cider-eval-region
      "ee" 'cider-eval-last-sexp
      "ex" 'cider-eval-last-sexp-and-replace
      "eb" 'cider-eval-buffer
      "cd" 'cider-doc))

  (defun setup-org-leader-keys ()
    "Sets up commands for org-mode."
    (evil-leader/set-key-for-mode 'org-mode
      "ct" 'org-todo
      "<RET>" 'org-insert-heading-respect-content))

  (defun magit-status-pick-repository ()
    "Calls magit-status with a prefix argument to allow picking the repository."
    (interactive)
    (let ((current-prefix-arg '(4))) ; C-u
      (call-interactively 'magit-status)))

  ;; nesting evil-leader package declaration
  ;; (global-evil-leader-mode) should be before (evil-mode 1)
  ;; this is so evil-leader works in *messages* buffer
  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")

    ;; mode specific leader keys
    (setup-clojure-leader-keys)
    (setup-lisp-interaction-leader-keys)
    (setup-emacs-lisp-leader-keys)
    (setup-org-leader-keys)

    (evil-leader/set-key
      ;; projectile
      "pf"  'projectile-find-file
      "pp"  'projectile-switch-project
      "pa"  'projectile-ag
      "po"  'projectile-find-other-file
      "pO"  'projectile-find-other-file-other-window

      ;; ivy
      "f"  'counsel-find-file
      "ag" 'counsel-ag
      "b"  'ivy-switch-buffer
      "r"  'ivy-recentf
      "ss" 'swiper
      "so" 'occur
      "sb" 'multi-occur
      "sp" 'ag-project
      "sa" 'ag

      ;; random
      "wh" 'split-window-below
      "wv" 'split-window-right
      "wt" 'toggle-window-split
      "wr" 'rotate-windows
      "-"  'split-window-below
      "|"  'split-window-right
      "\\" 'split-window-right
      "="  'indent-region-or-buffer
      "n"  'neotree-toggle
      "v"  (lambda () (interactive)(find-file "~/.emacs.d/init.el"))
      "x"  'smex

      "me"  'explorer-finder

      ;; shell
      "mm" 'open-shell
      "mo" 'find-file-other-window
      "mf" 'find-file

      ;; yasnippet
      "yn" 'yas-new-snippet
      "yv" 'yas-visit-snippet-file

      ;; magit
      "gr" 'magit-status-pick-repository
      "gs" 'magit-status
      "gb" 'magit-blame
      "gl" 'magit-log))

  ;; C-j jumps foward in jumplist, C-o goes the other way
  (setq evil-want-C-i-jump nil)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-jump-forward)

  ;; swap the ` and ' keys
  (define-key evil-motion-state-map "'" 'evil-goto-mark)
  (define-key evil-motion-state-map "`" 'evil-goto-mark-line)

  ;; swapping words
  (defun transpose-words-backwards ()
    "Does the reverse of transpose-words.
Moves the point to the position where we can transpose again for a bubbling effect."
    (interactive)
    (let ((current-prefix-arg '(-1))) ; C-u
      (call-interactively 'transpose-words)
      (evil-backward-word-begin)))
  (define-key evil-normal-state-map "gl" 'transpose-words)
  (define-key evil-normal-state-map "gh" 'transpose-words-backwards)

  (setq evil-normal-state-tag   (propertize " NORMAL ")
        evil-emacs-state-tag    (propertize " EMACS ")
        evil-insert-state-tag   (propertize " INSERT ")
        evil-motion-state-tag   (propertize " MOTION ")
        evil-visual-state-tag   (propertize " VISUAL ")
        evil-operator-state-tag (propertize " OPERATOR "))

  (evil-mode 1)
  ;; keybinds
  (define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
  (define-key evil-normal-state-map (kbd "TAB") 'hs-toggle-hiding)
  (define-key evil-visual-state-map (kbd "TAB") 'hs-toggle-hiding)
  (define-key evil-motion-state-map (kbd "TAB") 'hs-toggle-hiding)
  (define-key evil-insert-state-map (kbd "TAB") 'indent-for-tab-command) ;; default tab command

  ;; reselect text after identing
  ;; https://superuser.com/questions/469327/combining-two-operators-in-evil-mode-emacs
  (define-key evil-visual-state-map "g>" 'evil-shift-right)
  (define-key evil-visual-state-map "g<" 'evil-shift-left)
  (define-key evil-visual-state-map ">" (kbd "g>gv"))
  (define-key evil-visual-state-map "<" (kbd "g<gv"))

  ;;; occur mode
  (evil-set-initial-state 'occur-mode 'motion)
  (evil-define-key 'motion occur-mode-map (kbd "RET") 'occur-mode-goto-occurrence)
  (evil-define-key 'motion occur-mode-map (kbd "q")   'quit-window)

  ;; special mode
  (evil-define-key 'motion special-mode-map (kbd "q") 'quit-window)
  (evil-define-key 'normal special-mode-map (kbd "q") 'quit-window)
  (evil-define-key 'visual special-mode-map (kbd "q") 'quit-window)

  ;; magit mode
  (evil-define-key 'normal magit-status-mode-map (kbd "q") 'delete-window)

  ;; mu4e mode
  (evil-define-key 'normal mu4e-view-mode-map (kbd "q") 'delete-window)
  (evil-define-key 'motion mu4e-view-mode-map (kbd "q") 'delete-window)

  (setq evil-default-cursor t))  ; fix black cursor

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package key-chord
  :config
  ;; max time delay between two key presses to be considered a key chord
  (setq key-chord-two-keys-delay 0.15) ; default 0.1
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode)
  (setq evil-visualstar/persistent t))

;;; magit integration
(use-package evil-magit
  :after magit)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(global-set-key [escape] 'minibuffer-keyboard-quit)

;;; evil surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;; tree like vim
(use-package neotree
  :commands (neotree-enter neotree-hide)
  :init
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
  :config
  (setq neo-smart-open t)
  (setq neo-theme 'nerd)
  (setq neo-mode-line-type 'neotree)
  (setq neo-show-hidden-files t))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-line)
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)
              ("gy" . evil-commentary-yank)
              ("s-/" . evil-commentary-line))
  :init
  :config
  (evil-commentary-mode))

;;;; End Evil

;;;; Begin Languages

;;; Shell
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;;; Haskell
(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'completion-ignored-extensions ".hi"))

;;; C#
(use-package csharp-mode
  :mode "\\.cs\\'"
  :config
  (setq csharp-want-imenu nil)) ; turn off the menu

;;; Python

;; pdb setup
(when (on-osx)
  (setq pdb-path '/usr/lib/python2.7/pdb.py)
  (setq gud-pdb-command-name (symbol-name pdb-path))

  (defadvice pdb (before gud-query-cmdline activate)
    "Provide a better default command line when called interactively."
    (interactive
     (list (gud-query-cmdline pdb-path
                              (file-name-nondirectory buffer-file-name))))))

;; https://github.com/emacsmirror/python-mode - see troubleshooting
;; https://bugs.launchpad.net/python-mode/+bug/963253
;; http://pswinkels.blogspot.com/2010/04/debugging-python-code-from-within-emacs.html
(when (on-windows)
  (setq windows-python-pdb-path "c:/python27/python -i c:/python27/Lib/pdb.py")
  (setq pdb-path 'C:/Python27/Lib/pdb.py)
  (setq gud-pdb-command-name (symbol-name pdb-path))
  (setq gud-pdb-command-name windows-python-pdb-path)

  ;; everytime we enter a new python buffer, set the command path to include the buffer filename
  (add-hook 'python-mode-hook (function
                               (lambda ()
                                 (setq gud-pdb-command-name
                                       (concat windows-python-pdb-path " " buffer-file-name))))))

;;; Erlang
(use-package erlang
  ;; We need to specify erlang-mode explicitely as the package is not called
  ;; erlang-mode.
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)
         ("\\.xrl\\'" . erlang-mode))
  :config
  ;; http://erlang.org/pipermail/erlang-questions/2003-June/009103.html
  (setq hs-special-modes-alist
        (cons '(erlang-mode
                "^\\([a-z][a-zA-Z0-9_]*\\|'[^\n']*[^\\]'\\)\\s *(" nil "%"
                erlang-end-of-clause) hs-special-modes-alist))
  (setq tab-width 4)
  (setq erlang-indent-level 4)
  (setq erlang-font-lock-level-4))

;;; Web
;; http://web-mode.org/
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode)
         ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.jsp\\'" . web-mode))
  :config
  (global-unset-key (kbd "C-d"))
  (global-set-key (kbd "C-d") 'evil-scroll-down))

;;; Php
(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

;; colors for various 'color codes' aka hex strings
(use-package rainbow-mode
  :commands (rainbow-mode)
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  :diminish rainbow-mode
  :config
  (rainbow-mode 1))

;;; Groovy
(use-package groovy-mode
  :mode (("\\.gradle\\'" . groovy-mode)
         ("\\.groovy\\'" . groovy-mode))
  :config
  (groovy-mode))

;; Vimscript
(use-package vimrc-mode
  :mode (("\\.vimrc\\'" . vimrc-mode)))

;; Clojure
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :commands (cider-jack-in)
  :init
  (defun my/cider-mode-hook ()
    (unless (global-company-mode)
      (global-company-mode))
    (clj-refactor-mode)
    (eldoc-mode)
    (company/merge-backends))
  (add-hook 'cider-mode-hook #'my/cider-mode-hook)
  :config
  ;; use shift return to get a new line in repl
  (define-key cider-repl-mode-map (kbd "C-j") nil)
  (define-key cider-repl-mode-map [(shift return)] 'cider-repl-newline-and-indent)

  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package eval-sexp-fu
  :commands (eval-sexp-fu-flash-mode)
  :init
  (add-hook 'cider-mode-hook 'eval-sexp-fu-flash-mode)
  (add-hook 'clojure-mode-hook 'eval-sexp-fu-flash-mode)
  (add-hook 'emacs-lisp-mode-hook 'eval-sexp-fu-flash-mode)
  :config
  (eval-sexp-fu-flash-mode 1))

(use-package cider-eval-sexp-fu
  :after eval-sexp-fu)

(use-package clj-refactor
  :defer t
  :diminish clj-refactor-mode)

;;;; End Languages

;;;; Begin Functions

(defun magit-autoload-commands ()
  "Returns a list of magit commands that will load magit."
  (list 'magit-status 'magit-blame 'magit-log))

(defun set-newline-and-indent ()
  "Binds RET to autoindent after a new line."
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(defun toggle-window-split ()
  "Toggles window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows-helper(x d)
  "Rotates windows."
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))

(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))

(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Helvetica" :height 130 :width semi-condensed))
  (buffer-face-mode))

(defun my-buffer-face-mode-fixed ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Consolas" :height 120))
  (buffer-face-mode))

(defun explorer-finder ()
  "Opens up file explorer based on operating system."
  (interactive)
  (if (on-windows)
      (explorer))
  (if (on-osx)
      (reveal-in-osx-finder)))

(defun open-shell ()
  "Opens up a specific terminal depending on operating system."
  (interactive)
  (if (on-windows)
      (eshell)
    (multi-term)))

(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  "Closes compilation window on successful compile."
  (cond ((string-match "finished" string)
         (message "Build maybe successful: closing window.")
         (run-with-timer 1 nil
                         'delete-window
                         (get-buffer-window buffer t)))
        (t
         (message "Compilation exited abnormally: %s" string))))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
          (mapcar (lambda (x)
                    (cons (file-name-nondirectory x)
                          x))
                  recentf-list))
         (filename-list
          (remove-duplicates (mapcar #'car file-assoc-list)
                             :test #'string=))
         (filename (ido-completing-read "Choose recent file: "
                                        filename-list
                                        nil
                                        t)))
    (when filename
      (find-file (cdr (assoc filename
                             file-assoc-list))))))

;;;; End Functions

;;;; Begin Org Mode

(use-package org
  ;; associate .org files with org-mode inside of emacs
  :commands (org-agenda)
  :mode ("\\.org\\'" . org-mode)
  :init
  (defun customize-org-mode-solarized ()
    ;; http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
    ;; https://github.com/nashamri/spacemacs-theme/blob/master/spacemacs-common.el
    (font-lock-add-keywords 'org-mode
                            '(("^ +\\([-*]\\) "
                               (0 (prog1 ()
                                    (compose-region (match-beginning 1) (match-end 1) "•"))))))

    (let*
        ((light (not (use-dark-theme)))
         (gui window-system)

         (war "#dc752f")
         (suc (if gui "#42ae2c" "#00af00"))
         (green-bg (if light (if gui "#edf2e9" "#ffffff") (if gui "#293235" "262626")))
         (yellow-bg (if light (if gui "#f6f1e1" "#ffffff") (if gui "#32322c" "#262626")))
         (head1 (if light (if gui "#3a81c3" "#268bd2") (if gui "#4f97d7" "#268bd2")))
         (head2 (if light (if gui "#2d9574" "#2aa198") (if gui "#2d9574" "#2aa198")))
         (head3 (if light (if gui "#67b11d" "#5faf00") (if gui "#67b11d" "#67b11d")))
         (head4 (if light (if gui "#b1951d" "#875f00") (if gui "#b1951d" "#875f00")))

         ;; (face-font 'default) -> "-*-Source Code Pro-normal-normal- ..."
         ;; (split-string) -> ("" "*" "Source Code Pro" "normal" "normal" ...)
         (font-used (nth 2 (split-string (face-font 'default) "-")))
         (font `(:font ,font-used))
         (base-font-color     (face-foreground 'default nil 'default))
         (hl           `(:inherit default :weight bold :foreground ,base-font-color)))

      (custom-theme-set-faces
       'user
       `(org-todo ((t (,@hl ,@font :inherit bold :foreground ,war :background ,yellow-bg))))
       `(org-done ((t (,@hl ,@font ::inherit bold :foreground ,suc :background ,green-bg))))
       `(org-level-8 ((t (,@hl ,@font :foreground ,head4))))
       `(org-level-7 ((t (,@hl ,@font :foreground ,head3))))
       `(org-level-6 ((t (,@hl ,@font :foreground ,head2))))
       `(org-level-5 ((t (,@hl ,@font :foreground ,head1))))
       `(org-level-4 ((t (,@hl ,@font :height 1.0 :foreground ,head4))))
       `(org-level-3 ((t (,@hl ,@font :height 1.1 :foreground ,head3))))
       `(org-level-2 ((t (,@hl ,@font :height 1.2 :inherit bold :foreground ,head2))))
       `(org-level-1 ((t (,@hl ,@font :height 1.3 :inherit bold :foreground ,head1))))
       `(org-document-title ((t (,@hl ,@font :height 1.5 :underline nil)))))))

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              ;; evil mappings
              (define-key org-agenda-mode-map "b" 'evil-backward-word-begin)
              (define-key org-agenda-mode-map "w" 'evil-forward-word-begin)
              (define-key org-agenda-mode-map "j" 'evil-next-line)
              (define-key org-agenda-mode-map "k" 'evil-previous-line)
              (define-key org-agenda-mode-map "h" 'evil-backward-char)
              (define-key org-agenda-mode-map "l" 'evil-forward-char)))
  ;; hotkeys for org-mode
  :bind (("\C-cl" . org-store-link)
         ("\C-cc" . org-capture)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-iswitchb))
  :config
  (if (on-osx)
      (setq org-agenda-files '("~/Dropbox/Notes")))
  (if (on-windows)
      (setq org-agenda-files '("C:/Users/james/Dropbox/Notes")))
  (setq org-src-fontify-natively t)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-goto-interface 'outline-path-completion
        org-goto-max-level 10)
  (when using-solarized-theme
    (customize-org-mode-solarized)))

(use-package org-bullets
  :after org
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)))
  :config
  (org-bullets-mode 1))

;;;; End Org Mode

;;;; Begin Mail

;;; sending mail
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;;; receiving mail

;; add the source shipped with mu to load-path
(when (on-macbook-retina)
  (add-to-list 'load-path
               (expand-file-name
                "/usr/local/Cellar/mu/0.9.16/share/emacs/site-lisp/mu/mu4e")))

(when (on-imac)
  (add-to-list 'load-path
               (expand-file-name
                "/usr/local/Cellar/mu/0.9.16/share/emacs/site-lisp/mu/mu4e")))

(use-package mu4e
  :ensure nil
  :commands (mu4e)
  :config
  ;; tell mu4e where my mail is
  (setq mu4e-maildir (expand-file-name "~/Mail"))
  ;; tell mu4e how to sync email
  (setq mu4e-get-mail-command "mbsync -a")
  ;; tell mu4e to use w3m for html rendering
  (setq mu4e-html2text-command "w3m -T text/html")
  ;; download directory
  (setq mu4e-attachment-dir "~/Downloads")
  ;; skip duplicate emails
  (setq mu4e-headers-skip-duplicates t)

  ;; query for mails every 60 seconds
  (setq mu4e-update-interval 60)
  (setq mu4e-headers-auto-update t)

  ;; don't save message to Sent Messages, IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; show images
  (setq mu4e-show-images t)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; use 'fancy' non-ascii characters in various places in mu4e
  (setq mu4e-use-fancy-chars t)

  ;; don't show 'indexing' messages
  (setq mu4e-hide-index-messages t)

  ;; taken from mu4e page to define bookmarks
  (add-to-list 'mu4e-bookmarks '("flag:attach" "Messages with attachment" ?a) t)
  (add-to-list 'mu4e-bookmarks '("size:5M..500M" "Big messages" ?b) t)
  (add-to-list 'mu4e-bookmarks '("flag:flagged" "Flagged messages" ?f) t)

  ;; mu4e requires to specify drafts, sent, and trash dirs
  (setq mu4e-drafts-folder "/mu4e/drafts")
  (setq mu4e-sent-folder "/mu4e/sent")
  (setq mu4e-trash-folder "/mu4e/trash")

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; general emacs mail settings; used when composing e-mail
  ;; the non-mu4e-* stuff is inherited from emacs/message-mode
  (setq mu4e-reply-to-address "me@example.com"
        user-mail-address "ja.nguyen@gmail.com"
        user-full-name  "James Nguyen"
        mu4e-compose-signature
        (concat
         "James Nguyen\n")))

;; (require 'mu4e)
(use-package evil-mu4e
  :if (require 'mu4e nil 'noerror)
  :after mu4e)

(use-package mu4e-maildirs-extension
  :if (require 'mu4e nil 'noerror)
  :after mu4e
  :config
  (mu4e-maildirs-extension))
;;;; End Mail

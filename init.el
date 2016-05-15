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
(add-hook 'focus-out-hook 'garbage-collect)

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
(use-package exec-path-from-shell
  :if (on-osx)
  :defer 2
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package multi-term
  :if (not (on-windows))
  :commands (multi-term)
  :init
  :config
  (evil-define-key 'insert term-mode-map (kbd "TAB") 'term-send-raw) ;; rebinding
  (evil-define-key 'insert term-raw-map (kbd "TAB") 'term-send-raw) ;; rebinding
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

(use-package gotham-theme :defer)
(use-package color-theme-solarized :defer)

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
  (unless (on-windows)
    (add-to-list 'sml/replacer-regexp-list '("^~/Developer/" ":DEV:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/Notes/" ":NOTES:")))
  (when (on-windows)
    (add-to-list 'sml/replacer-regexp-list '("C:/Users/james/Developer/" ":DEV:"))
    (add-to-list 'sml/replacer-regexp-list '("C:/Users/james/Dropbox/Notes/" ":NOTES:"))
    (add-to-list 'sml/replacer-regexp-list '("C:/Users/james/Dropbox/" ":DB:")))
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
  (defun do-additional-theme-changes ()
    "Execute additional theme changes."
    (let* ((daytime (is-daytime))
           (mode-line-active (if daytime "#091f2e" "#eee8d5"))
           (mode-line-inactive (if daytime "#11151c" "#eee8d5")))
      (set-face-attribute 'mode-line nil
                          :box `(:line-width 2 :color ,mode-line-active))
      (set-face-attribute 'modeline-inactive nil
                          :box `(:line-width 2 :color ,mode-line-inactive))))

  (defun reset-line--change-theme (&rest args)
    ;; runs org-reload on current org mode buffers
    (when (fboundp 'org-reload)
      (dolist (b (buffer-list))
        (with-current-buffer b
          (when (eq major-mode 'org-mode)
            (org-reload)))))
    (when (fboundp 'powerline-reset)
      (powerline-reset))
    (do-additional-theme-changes))
  (advice-add 'change-theme :after #'reset-line--change-theme)
  (set-frame-parameter nil 'background-mode 'light)
  (change-theme 'gotham 'solarized))

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
  :commands (magit-status magit-blame magit-log)
  :config
  (evil-define-key 'normal magit-status-mode-map
    (kbd "q") 'delete-window)

  (setq magit-repository-directories '("~/Developer"
                                       "~/.emacs.d"
                                       "~/.vim"
                                       "~/.dotfiles"
                                       "~/.zsh"))
  (setq magit-refresh-status-buffer nil)
  (setq magit-completing-read-function 'ivy-completing-read)

  (defadvice magit-show-commit (around dont-select-commit-window activate)
    "magit-show-commit selects the window it opens unless magit-display-buffer-noselect is set.
Setting magit-display-buffer-noselect changes the selection logic for other parts of magit though.
Instead, advise magit-show-commit by setting magit-show-commit to t
before calling magit-show-commit and set it back to nil afterwards."
    (setq magit-display-buffer-noselect t)
    (setq ad-return-value ad-do-it)
    (setq magit-display-buffer-noselect nil))

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

;; another way to use meta
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

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

(add-hook 'text-mode-hook 'visual-line-mode) ; wraps line when it reaches end

;; hide the comments too when you do a 'hs-hide-all'
(setq hs-hide-comments nil)
;; Set whether isearch opens folded comments, code, or both
;; where x is code, comments, t (both), or nil (neither)
(setq hs-isearch-open 'x)

;;; line numbers
(use-package linum
  :config
  (global-linum-mode 0))

;; syntax check
(use-package flycheck
  :load-path "~/.emacs.d/fork/flycheck/"
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp))
  :commands flycheck-mode)

;; save on focus lost
(use-package focus-autosave-mode
  :defer 5
  :diminish focus-autosave-mode
  :config
  (focus-autosave-mode))

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode 1))

;;;; End Experience

;;;; Begin Editing

(transient-mark-mode 1) ; enable transient mark mode

(setq kill-whole-line t) ; kills entire line if at the beginning
(fset 'yes-or-no-p 'y-or-n-p) ; yes or no to y or n
(column-number-mode 1) ; makes the column number show up

;; emacs 24+ auto indents by default if electric-indent-mode is on
;; so disable automatic indent by default
(electric-indent-mode 0)
;; but enable it in all programming modes
(dolist (mode '(prog-mode-hook
                yaml-mode-hook
                css-mode-hook
                html-mode-hook))
  (add-hook mode (lambda ()
                   (interactive)
                   (electric-indent-local-mode 1))))

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

;;; highlight parentheses
(use-package paren
  :config
  (show-paren-mode t))

;; automatic pairs
(use-package smartparens
  ;; :load-path "~/.emacs.d/fork/smartparens/"
  :diminish smartparens-mode
  :init
  (defun my/set-smartparens-settings ()
    "Setting smartparens settings."
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-autoskip-closing-pair 'always-end)
    (setq sp-autoskip-opening-pair t))
  (defun my/smartparens-hook ()
    "Custom hook for smartparens."
    (my/set-smartparens-settings))
  (add-hook 'prog-mode-hook #'my/smartparens-hook)
  :config
  (use-package smartparens-config :ensure nil)
  (smartparens-global-mode 1)
  (my/set-smartparens-settings)
  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "(" ")" :wrap "C-)")

  (setq bracy '(c-mode
                c++-mode
                objc-mode
                csharp-mode
                java-mode
                js-mode
                php-mode
                json-mode
                css-mode))

  (sp-local-pair bracy "/*" "*/"
                 :when '(sp-point-in-empty-line-p))
  (sp-local-pair bracy "(" nil
                 :unless '(sp-point-before-word-p))
  (sp-local-pair bracy "[" nil
                 :unless '(sp-point-before-word-p))
  (sp-local-pair bracy "{" "}"
                 :when '(("RET" "<evil-ret>"))
                 :post-handlers '(reindent-and-position-middle))

  (sp-with-modes '(objc-mode)
    (sp-local-pair "@interface" "@end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p)
                   :post-handlers '(add-pair-and-return))
    ;; this is not working yet
    (sp-local-pair "@implementation" "@end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p)
                   :post-handlers '(add-pair-and-return)))

  (defun add-pair-and-return (&rest _ignored)
    "Adds the pair and then return to position."
    (save-excursion
      (insert "x")
      (newline)
      (indent-according-to-mode))
    (delete-char 1))

  (defun reindent-and-position-middle (&rest _ignored)
    "Reindents and positions cursor in the middle."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

;;; clipboards
;; for linux
(use-package xclip
  :if (on-linux)
  :config
  (xclip-mode 1))

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
(global-set-key (kbd "C-q x") 'kill-this-buffer)
(global-set-key (kbd "C-q C-x") 'kill-this-buffer)
(global-set-key (kbd "C-q <backspace>") 'delete-window)
(global-set-key (kbd "C-q C-<backspace>") 'delete-window)
(global-set-key (kbd "C-q ,") 'evil-prev-buffer)
(global-set-key (kbd "C-q C-,") 'evil-prev-buffer)
(global-set-key (kbd "C-q .") 'evil-next-buffer)
(global-set-key (kbd "C-q C-.") 'evil-next-buffer)
(global-set-key (kbd "C-q u") 'winner-undo)
(global-set-key (kbd "C-q C-u") 'winner-undo)
(global-set-key (kbd "C-q r") 'winner-redo)
(global-set-key (kbd "C-q C-r") 'winner-redo)

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
  (define-key perspective-map (kbd "r") 'nil) ; this was persp-rename before
  (define-key perspective-map (kbd "c") 'persp-switch) ; mirroring tmux
  (define-key perspective-map (kbd "x") 'delete-perspective-or-window)
  (define-key perspective-map (kbd "/") 'persp-rename)
  (define-key perspective-map (kbd "C-n") 'persp-next)
  (define-key perspective-map (kbd "C-p") 'persp-prev)
  (persp-mode 1))

;;;; End Navigation

;;;; Begin File Management

(use-package swiper
  :ensure counsel
  :diminish ivy-mode
  :config
  ;; swapping behavior
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

  ;; default: "ag --nocolor --nogroup %s -- ."
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- .")
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
             projectile-ag
             my/projectile-find)
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
  (setq recentf-max-saved-items 200
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
  :config
  ;; http://spacemacs.org/doc/FAQ
  ;; https://github.com/syl20bnr/spacemacs/issues/2032
  ;; https://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-flash-delay 8) ;; control the highlight time of searches

  (defun magit-status-pick-repository ()
    "Calls magit-status with a prefix argument to allow picking the repository."
    (interactive)
    (let ((current-prefix-arg '(4))) ; C-u
      (call-interactively 'magit-status)))

  (defun my/set-evil-shift-width (width)
    "Changing local indent width based on width passed."
    (make-local-variable 'evil-shift-width)
    (setq evil-shift-width width))

  (defun my/find-file-dwim ()
    "Tries to find file in project.
If not in a project, fallback by using counsel-find-file."
    (interactive)
    (unless (ignore-errors ((call-interactively 'projectile-find-file)))
      (unless (ignore-errors (counsel-git))
        (unless (ignore-errors (counsel-find-file))
          (call-interactively 'find-file)))))

  ;; nesting evil-leader package declaration
  ;; (global-evil-leader-mode) should be before (evil-mode 1)
  ;; this is so evil-leader works in *messages* buffer
  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")

    (evil-leader/set-key
      ;; projectile
      "pp"  'projectile-switch-project
      "pa"  'projectile-ag
      "po"  'projectile-find-other-file
      "pO"  'projectile-find-other-file-other-window

      ;; ivy
      "f"  'my/find-file-dwim
      "b"  'ivy-switch-buffer
      "r"  'ivy-recentf
      "ss" 'swiper
      "sa" 'counsel-ag
      "sf" 'counsel-find-file
      "so" 'occur
      "sb" 'multi-occur
      "ag" 'ag
      "ap" 'ag-project

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

      ;; magit
      "gr" 'magit-status-pick-repository
      "gs" 'magit-status
      "gb" 'magit-blame
      "gl" 'magit-log
      "gm" 'mu4e))

  ;; macro to define keys for multiple modes
  ;; modified a little to not have to repeat the 'mode' var
  ;; https://www.reddit.com/r/emacs/comments/2u5uzq/i_wrote_a_somewhat_useful_elisp_macro/
  (defmacro evil-define-multiple (keymaps mode &rest bindings)
    "Macro to allow keymaps to be bound."
    `(progn ,@(loop for keymap in keymaps
                    appending
                    (loop for (key cmd) in bindings
                          collect `(evil-define-key ,mode ,keymap ,key ,cmd)))))

  ;; C-j jumps foward in jumplist, C-o goes the other way
  (setq evil-want-C-i-jump nil)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-jump-forward)

  ;; swap the ` and ' keys
  (define-key evil-motion-state-map "'" 'evil-goto-mark)
  (define-key evil-motion-state-map "`" 'evil-goto-mark-line)

  ;; https://stackoverflow.com/questions/32977277/emacs-backspace-at-beginning-of-tabbed-line-similar-to-intellij
  (defun intellij-backspace ()
    "Provides an intellij-like backspace."
    (interactive)
    (let* ((end (save-excursion
                  (end-of-line)
                  (point)))
           (beginning (save-excursion
                        (beginning-of-line)
                        (point))))
      (if (string-match "^[ \t]*$" (buffer-substring beginning end))
          (progn
            (beginning-of-line)
            (kill-line)
            (previous-line)
            (indent-for-tab-command)
            (end-of-line))
        (backward-delete-char-untabify 1))))

  (defun enable-or-disable-intellij-backspace (enable)
    "If enable, enable intellij-like backspace, otherwise use default backspace."
    (interactive)
    (define-key evil-insert-state-local-map (kbd "DEL") (if enable 'intellij-backspace nil)))

  (dolist (hook '(prog-mode-hook
                  org-mode-hook
                  css-mode-hook
                  html-mode-hook
                  yaml-mode-hook))
    (add-hook hook (lambda ()
                     (enable-or-disable-intellij-backspace t))))

  (dolist (hook '(python-mode-hook))
    (add-hook hook (lambda ()
                     (enable-or-disable-intellij-backspace nil))))

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

  (defun copy-to-end-of-line ()
    "Copy from point to end of line."
    (interactive)
    (evil-yank (point) (point-at-eol)))

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

  (defun mimic-find-references ()
    "When we don't have find-usages/find-references,
do a search for the string from projet root to mimic that functionality."
    (interactive)
    (ag-project (ag/dwim-at-point)))

  ;; help mode
  (evil-define-key 'motion help-mode-map (kbd "g.") 'push-button)

  ;; occur mode
  (evil-set-initial-state 'occur-mode 'motion)
  (evil-define-key 'motion occur-mode-map
    (kbd "RET") 'occur-mode-goto-occurrence
    (kbd "q")   'quit-window)

  ;; special mode
  (evil-define-key 'motion special-mode-map (kbd "q") 'quit-window)
  (evil-define-key 'normal special-mode-map (kbd "q") 'quit-window)
  (evil-define-key 'visual special-mode-map (kbd "q") 'quit-window)

  ;; package mode bindings
  (evil-add-hjkl-bindings package-menu-mode-map 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous)

  (setq evil-default-cursor t))  ; fix black cursor

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package key-chord
  :config
  ;; max time delay between two key presses to be considered a key chord
  (setq key-chord-two-keys-delay 0.2) ; default 0.1
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
  (setq neo-smart-open nil)
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

;; completion
(use-package auto-complete
  :diminish auto-complete-mode
  :init
  :config
  (use-package fuzzy)
  (defun my/ac-setup-default-sources ()
    "Setting up default sources before adding more."
    (setq ac-sources '(ac-source-yasnippet
                       ac-source-abbrev
                       ac-source-dictionary
                       ac-source-words-in-same-mode-buffers)))
  (defun my/ac-add-mode-to-modes (mode)
    "Convenience method to add mode to ac-modes if not already there."
    (unless (member mode 'ac-modes)
      (add-to-list 'ac-modes mode)))
  (defun my/ac-add-source (source)
    "Convenience method to add a source to buffer local ac-sources."
    (add-to-list 'ac-sources source))
  (ac-config-default)

  ;; adding yasnippet to default sources
  (setq-default ac-sources (push 'ac-source-yasnippet ac-sources))

  (setq ac-use-fuzzy t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

  ;; binding keys
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (define-key ac-menu-map [backtab] 'ac-previous)
  (define-key ac-menu-map (kbd "<backtab>") 'ac-previous)
  (define-key ac-menu-map [S-tab] 'ac-previous)
  (define-key ac-menu-map [S-iso-lefttab] 'ac-previous)
  (define-key ac-menu-map [(shift tab)] 'ac-previous)
  (define-key evil-insert-state-map (kbd "C-n") 'ac-fuzzy-complete)
  (define-key evil-insert-state-map (kbd "C-p") 'ac-fuzzy-complete))

;; snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :after auto-complete
  :config
  ;; yas messages stretches the status buffer when it starts up
  (setq yas-verbosity 2)
  ;; using yasnippet through company mode, so disable all the binds
  ;; (define-key yas-minor-mode-map (kbd "C-i") nil)
  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;; (define-key yas-minor-mode-map [(tab)] 'nil)
  ;; (define-key yas-minor-mode-map "\C-c&\C-s" nil)
  ;; (define-key yas-minor-mode-map "\C-c&\C-n" nil)
  ;; (define-key yas-minor-mode-map "\C-c&\C-v" nil)
  (yas-global-mode 1))

;;; C family of languages.

(use-package auto-complete-clang-async
  :commands (ac-clang-launch-completion-process)
  :init
  (defun my/setup-ios-completion ()
    "Setting up semantic ios/osx completion.
Used http://hyegar.com/2016/03/02/emacs-for-objc/ as baseline."
    (defvar xcode-base-path "/Applications/Xcode.app/Contents/Developer/Platforms/")
    (defvar ios-sdk-path "iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk")
    (defvar mac-sdk-path "MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk")
    (when (on-osx)
      (setenv "LC_CTYPE" "UTF-8")
      (let ((args `("-isysroot"
                    ,(concat xcode-base-path ios-sdk-path)
                    ;; ,(concat xcode-base-path mac-sdk-path)
                    ;; "-std=c++11"
                    "-I" "/usr/include/c++/4.2.1")))
        (setq ac-clang-flags args))))

  (defun my/alt-setup-ios-completion ()
    "Using instructions from https://github.com/yasuyk/auto-complete-clang-objc
to set up objc completion.
Notably, run '$ echo "" | g++ -v -x c++ -E -' to get the header paths on computer."
    (defun my/alt-setup-ios-completion ()
      (setq ac-clang-flags
            (mapcar (lambda (item) (concat "-I" item))
                    (split-string
                     "
   /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
   /usr/local/include
   /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/7.3.0/include
   /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
   /usr/include
   /System/Library/Frameworks
   /Library/Frameworks
  ")))))

  (defun my/ac-base-clang-setup ()
    "Base method to set up this package."
    (my/ac-add-mode-to-modes 'objc-mode)
    (my/ac-setup-default-sources)
    (my/ac-add-source 'ac-source-clang-async)
    (setq ac-clang-complete-executable
          "~/.emacs.d/fork/emacs-clang-complete-async/clang-complete")
    (ac-clang-launch-completion-process))

  (defun my/ac-objc-setup ()
    "Setting up objc completion in autocomplete."
    (my/ac-base-clang-setup)
    (my/setup-ios-completion)
    ;; (my/alt-setup-ios-completion)
    )
  (add-hook 'objc-mode-hook 'my/ac-objc-setup)
  (add-hook 'c-mode-hook 'my/ac-base-clang-setup)
  (add-hook 'c++-mode-hook 'my/ac-base-clang-setup))

(evil-define-multiple
 (c-mode-map objc-mode-map c++-mode-map)
 'normal
 ((kbd "go") 'ff-find-other-file))

(use-package dummy-h-mode
  :mode ("\\.h$" . dummy-h-mode))

(use-package clang-format
  :commands (clang-format-buffer clang-format-region)
  :init
  (defun clang-format-region-or-buffer()
    "If clang-format is not available, do the default indenting.
Otherwise try to use clang-format. Indents region if there's a selection,
otherwise buffer is formatted."
    (interactive)
    (if (not (executable-find "clang-format"))
        (indent-region-or-buffer)
      (if (region-active-p)
          (call-interactively 'clang-format-region)
        (clang-format-buffer))))

  (dolist (mode '(c-mode c++-mode objc-mode))
    (evil-leader/set-key-for-mode mode
      "=" 'clang-format-region-or-buffer)))

(use-package ggtags
  :diminish ggtags-mode
  :commands (ggtags-mode)
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  :config
  (evil-define-key 'normal ggtags-global-mode-map
    (kbd "h") 'evil-backward-char)
  (evil-define-key 'normal ggtags-mode-map
    (kbd "g.") 'ggtags-find-tag-dwim
    (kbd "g,") 'ggtags-prev-mark
    (kbd "gd") 'ggtags-find-definition
    (kbd "gf") 'ggtags-find-file
    (kbd "g?") 'ggtags-show-definition
    (kbd "gr") 'ggtags-find-reference)
  (ggtags-mode 1))

;; Objective-C
(use-package cc-mode
  :mode
  ("\\.m\\'" . objc-mode)
  ("\\.mm\\'" . objc-mode)
  ("\\.xctool.args\\'" . objc-mode)
  :config
  (defun occur-find-pragma ()
    (interactive)
    (occur "pragma mark [a-zA-Z0-9]")
    (pop-to-buffer "*Occur*"))
  (evil-define-key 'normal objc-mode-map
    (kbd "gr") 'mimic-find-references
    (kbd "gp") 'occur-find-pragma))

(use-package xcode-mode
  ;; https://github.com/phonegap/ios-sim
  ;; https://github.com/facebook/xctool
  :commands (xcode-mode)
  :load-path "~/.emacs.d/fork/xcode-mode/"
  :ensure nil
  :init
  (add-hook 'objc-mode-hook #'xcode-mode)
  :config
  (which-key-add-major-mode-key-based-replacements 'objc-mode
    "<SPC>yb" "build"
    "<SPC>yt" "test"
    "<SPC>yc" "clean"
    "<SPC>yp" "pod"
    "<SPC>yo" "open"
    "<SPC>yaa" "archive"
    "<SPC>ydd" "derived data"
    "<SPC>yy" "build and run"
    "<SPC>yrr" "run app"
    "<SPC>ybb" "build app"
    "<SPC>ybt" "build tests"
    "<SPC>ytr" "run tests"
    "<SPC>ytt" "test single"
    "<SPC>yto" "build tests only"
    "<SPC>ycc" "clean app"
    "<SPC>ypi" "install"
    "<SPC>yos" "storyboard"
    "<SPC>yop" "project"
    "<SPC>yow" "workspace")
  (evil-leader/set-key-for-mode 'objc-mode
    "yy" 'xcode-xctool-build-and-run
    "yrr" 'xcode-xctool-run
    "ybb" 'xcode-xctool-build
    "ybt" 'xcode-xctool-build-tests
    "ytr" 'xcode-xctool-run-tests
    "ytt" 'xcode-xctool-test
    "yto" 'xcode-xctool-build-tests-only
    "ycc" 'xcode-xctool-clean
    "ypi" 'xcode-pod-install
    "yos" 'xcode-open-storyboard
    "yop" 'xcode-open-project
    "yow" 'xcode-open-workspace
    "yaa" 'xcode-xctool-archive
    "ydd" 'xcode-delete-derived-data))

;; Java
(use-package cc-mode
  :mode ("\\.java\\'" . java-mode)
  :config
  (use-package javadoc-lookup
    :commands (javadoc-lookup)
    :init
    (evil-define-key 'normal java-mode-map (kbd "K") 'javadoc-lookup)))

;; C#
(use-package csharp-mode
  :mode "\\.cs\\'"
  :config
  (setq csharp-want-imenu t))

(use-package omnisharp
  :commands (omnisharp-mode)
  :init
  (defun my/ac-setup-csharp ()
    "Sets up c# completion with autocomplete."
    (my/ac-add-mode-to-modes 'csharp-mode)
    (my/ac-setup-default-sources)
    (my/ac-add-source 'ac-source-omnisharp))
  (defun my/omnisharp-setup ()
    "Bootstrap omnisharp."
    (setq omnisharp-debug t)
    (setq omnisharp-server-executable-path
          "~/.emacs.d/fork/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
    (omnisharp-mode))
  (defun my/csharp-mode-hook ()
    "csharp mode hook"
    (my/omnisharp-setup)
    (my/ac-setup-csharp))
  (add-hook 'csharp-mode-hook #'my/csharp-mode-hook)
  :config
  (setq omnisharp-eldoc-support t))

;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

;;; Dynamic and/or interpreted languages.

;; Groovy
(use-package groovy-mode
  :mode
  ("\\.gradle\\'" . groovy-mode)
  ("\\.groovy\\'" . groovy-mode)
  :config
  (groovy-mode))

;; Swift
(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :config
  (evil-leader/set-key-for-mode 'swift-mode
    "ez" 'swift-mode-run-repl
    "eb" 'swift-mode-send-buffer
    "er" 'swift-mode-send-region)
  (define-key swift-repl-mode-map [(shift return)] 'evil-jump-forward))

;; Ruby
(use-package ruby-mode
  :mode
  "\\.rb$\\'"
  "\\Rakefile$\\'"
  "\\.gemspec$\\'"
  "\\.ru$\\'"
  "\\Gemfile$\\'"
  "\\.rake$\\'"
  :interpreter "ruby"
  :config
  (my/set-evil-shift-width ruby-indent-level)
  (use-package bundler
    :config
    (dolist (mode '(ruby-mode motion-mode))
      (which-key-add-major-mode-key-based-replacements mode
        "<SPC>yb" "bundler")
      (evil-leader/set-key-for-mode mode
        "ybo" 'bundle-open
        "ybc" 'bundle-console
        "ybC" 'bundle-check
        "ybi" 'bundle-install
        "ybu" 'bundle-update
        "ybe" 'bundle-exec
        "ybO" 'bundle-outdated
        "ybs" 'bundle-show
        "ybv" 'bundle-version
        "ybb" 'bundle-command))))

(use-package robe
  :commands (robe-mode)
  :init
  (add-hook 'ruby-mode-hook #'my/robe-mode-hook)
  (defun my/robe-mode-hook ()
    "robe-mode hook."
    (if (derived-mode-p 'motion-mode)
        (robe-mode 0)
      (progn
        (robe-mode 1)
        (my/ac-setup-default-sources)
        (ac-robe-setup))))
  :config
  ;; (define-key map (kbd "C-c C-k") 'robe-rails-refresh)
  (evil-define-key 'normal robe-mode-map
    (kbd "g.") 'robe-jump
    (kbd "g,") 'pop-tag-mark
    (kbd "gd") 'robe-jump
    (kbd "gf") 'robe-find-file
    (kbd "K") 'robe-doc))

(use-package projectile-rails
  :commands (projectile-rails-on)
  :init
  (add-hook 'web-mode-hook #'projectile-rails-on)
  (add-hook 'ruby-mode-hook #'projectile-rails-on)
  :config
  (evil-define-key 'normal projectile-rails-mode-map
    (kbd "gf") 'projectile-rails-goto-file-at-point)
  (dolist (mode '(ruby-mode web-mode))
    (which-key-add-major-mode-key-based-replacements mode
      "<SPC>y" "rails"
      "<SPC>yg" "goto"
      "<SPC>ya" "find locale"
      "<SPC>yA" "find job"
      "<SPC>yc" "find controller"
      "<SPC>yC" "find current controller"
      "<SPC>yd" "dbconsole"
      "<SPC>yD" "console"
      "<SPC>ye" "find environment"
      "<SPC>yE" "generate"
      "<SPC>yf" "find feature"
      "<SPC>yF" "find validator"
      "<SPC>ygg" "goto gemfile"
      "<SPC>ygr" "goto routes"
      "<SPC>ygd" "goto schema"
      "<SPC>ygs" "goto seeds"
      "<SPC>ygh" "goto spec helper"
      "<SPC>yh" "find helper"
      "<SPC>yH" "find current helper"
      "<SPC>yi" "find initializer"
      "<SPC>yj" "find javascript"
      "<SPC>yJ" "find stylesheet"
      "<SPC>yl" "find lib"
      "<SPC>yL" "find layout"
      "<SPC>ym" "find model"
      "<SPC>yM" "find current model"
      "<SPC>yn" "find migration"
      "<SPC>yN" "find current migration"
      "<SPC>yo" "find log"
      "<SPC>yp" "find spec"
      "<SPC>yP" "find current spec"
      "<SPC>yr" "rake"
      "<SPC>yR" "find rake task"
      "<SPC>yt" "find test"
      "<SPC>yT" "find current test"
      "<SPC>yv" "find view"
      "<SPC>yV" "find current view"
      "<SPC>yu" "find fixture"
      "<SPC>yU" "find current fixture"
      "<SPC>yx" "extract region"
      "<SPC>yy" "server"
      "<SPC>y@" "find mailer"
      "<SPC>y <RET>" "goto file")
    (evil-leader/set-key-for-mode mode
      "ya" 'projectile-rails-find-locale
      "yA" 'projectile-rails-find-job
      "yc" 'projectile-rails-find-controller
      "yC" 'projectile-rails-find-current-controller
      "yd" 'projectile-rails-dbconsole
      "yD" 'projectile-rails-console
      "ye" 'projectile-rails-find-environment
      "yE" 'projectile-rails-generate
      "yf" 'projectile-rails-find-feature
      "yF" 'projectile-rails-find-validator
      "ygg" 'projectile-rails-goto-gemfile
      "ygr" 'projectile-rails-goto-routes
      "ygd" 'projectile-rails-goto-schema
      "ygs" 'projectile-rails-goto-seeds
      "ygh" 'projectile-rails-goto-spec-helper
      "yh" 'projectile-rails-find-helper
      "yH" 'projectile-rails-find-current-helper
      "yi" 'projectile-rails-find-initializer
      "yj" 'projectile-rails-find-javascript
      "yJ" 'projectile-rails-find-stylesheet
      "yl" 'projectile-rails-find-lib
      "yL" 'projectile-rails-find-layout
      "ym" 'projectile-rails-find-model
      "yM" 'projectile-rails-find-current-model
      "yn" 'projectile-rails-find-migration
      "yN" 'projectile-rails-find-current-migration
      "yo" 'projectile-rails-find-log
      "yp" 'projectile-rails-find-spec
      "yP" 'projectile-rails-find-current-spec
      "yr" 'projectile-rails-rake
      "yR" 'projectile-rails-find-rake-task
      "yt" 'projectile-rails-find-test
      "yT" 'projectile-rails-find-current-test
      "yu" 'projectile-rails-find-fixture
      "yU" 'projectile-rails-find-current-fixture
      "yv" 'projectile-rails-find-view
      "yV" 'projectile-rails-find-current-view
      "yx" 'projectile-rails-extract-region
      "yy" 'projectile-rails-server
      "y@" 'projectile-rails-find-mailer
      "y <RET>" 'projectile-rails-goto-file-at-point)
    (use-package evil-rails)))

(use-package motion-mode
  :commands (motion-recognize-project)
  :init
  (defun my/motion-mode-hook ()
    "Motion mode hook."
    (setq motion-flymake nil) ;; disable flymake
    (motion-recognize-project)
    (my/ac-add-mode-to-modes 'motion-mode)
    (setq flycheck-checker 'ruby-rubocop))
  (add-hook 'ruby-mode-hook #'my/motion-mode-hook)
  :config
  (defun my/rake-to-device ()
    "Executes rake device."
    (interactive)
    (motion-execute-rake-command "device"))
  (defun my/rake-pod-install ()
    "Executes rake install."
    (interactive)
    (motion-execute-rake-command "pod:install"))
  (defun my/rake-pod-update ()
    "Executes rake update."
    (interactive)
    (motion-execute-rake-command "pod:update"))
  (defun my/rake-clean ()
    "Executes rake update."
    (interactive)
    (motion-execute-rake-command "clean"))
  (defun my/rake-spec ()
    "Executes rake spec."
    (interactive)
    (motion-execute-rake-command "spec"))
  (defun my/rake-run-sim ()
    "Tries to reload motion app.
If failure, run rake instead."
    (interactive)
    (unless (ignore-errors (motion-reload-app))
      (motion-execute-rake)))
  (defun my/rake-execute (command)
    "Enter in rake command to execute."
    (interactive "sEnter Rake command:")
    (motion-execute-rake-command command))
  (evil-define-key 'normal motion-mode-map (kbd "K") 'motion-dash-at-point)
  (which-key-add-major-mode-key-based-replacements 'motion-mode
    "<SPC>yp" "pod")
  (evil-leader/set-key-for-mode 'motion-mode
    "ypi" 'my/rake-pod-install
    "ypu" 'my/rake-pod-update
    "yc" 'my/rake-clean
    "yd" 'my/rake-to-device
    "yR" 'my/rake-execute
    "yy" 'my/rake-run-sim
    "yt" 'my/rake-spec
    "yr" 'motion-execute-rake))

;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (evil-leader/set-key-for-mode 'python-mode
    "ci" 'run-python
    "cd" 'pdb
    "ez" 'python-shell-switch-to-shell
    "er" 'python-shell-send-region
    "eb" 'python-shell-send-buffer
    "es" 'python-shell-send-string
    "el" 'python-shell-send-file
    "ee" 'python-shell-send-defun)
  (my/set-evil-shift-width python-indent)

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

    (defun my/set-pdb-command-path ()
      (setq gud-pdb-command-name
            (concat windows-python-pdb-path " " buffer-file-name)))

    ;; everytime we enter a new python buffer, set the command path to include the buffer filename
    (add-hook 'python-mode-hook 'my/set-pdb-command-path)))

(use-package jedi
  :commands (jedi:setup jedi:install-server)
  :init
  (defun my/python-mode-hook ()
    (my/ac-setup-default-sources)
    (jedi:setup))
  (add-hook 'python-mode-hook #'my/python-mode-hook)
  :config
  (setq jedi:complete-on-dot t)
  (evil-define-key 'normal jedi-mode-map
    (kbd "g.") 'jedi:goto-definition
    (kbd "g,") 'jedi:goto-definition-pop-marker
    (kbd "gd") 'jedi:goto-definition
    (kbd "K") 'jedi:show-doc))

;;; Lisp like languages.

(use-package lisp-mode
  :ensure nil
  :config
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (evil-leader/set-key-for-mode mode
      "ez" 'ielm
      "er" 'eval-region
      "ee" 'eval-last-sexp
      "ex" 'eval-last-sexp-and-replace
      "eb" 'eval-buffer))
  (my/set-evil-shift-width lisp-body-indent))

;; Elisp
(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :commands (turn-on-elisp-slime-nav-mode)
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  :config
  (evil-define-key 'normal elisp-slime-nav-mode-map
    (kbd "g.") 'elisp-slime-nav-find-elisp-thing-at-point
    (kbd "g,") 'pop-tag-mark
    (kbd "gd") 'elisp-slime-nav-describe-elisp-thing-at-point
    (kbd "gr") 'mimic-find-references
    (kbd "K")  'elisp-slime-nav-describe-elisp-thing-at-point)

  (turn-on-elisp-slime-nav-mode)
  (defadvice elisp-slime-nav-describe-elisp-thing-at-point (after slime-move-to-doc activate)
    "Move point to the other window after opening up documentation window."
    (pop-to-buffer "*Help*")))

;; Clojure
(use-package clojure-mode
  :mode
  ("\\.clj\\'" . clojure-mode)
  ("\\.edn\\'" . clojure-mode))

(use-package cider
  :commands (cider-mode cider-jack-in)
  :init
  (defun my/cider-mode-hook ()
    (cider-mode 1)
    (clj-refactor-mode)
    (eldoc-mode))
  (add-hook 'clojure-mode-hook #'my/cider-mode-hook)
  (add-hook 'cider-repl-mode-hook #'my/cider-mode-hook)
  :config
  (evil-define-multiple
   (clojure-mode-map cider-mode-map cider-repl-mode-map)
   'normal
   ((kbd "gz") 'cider-switch-to-repl-buffer)
   ((kbd "g.") 'cider-find-dwim)
   ((kbd "g,") 'cider-pop-back)
   ((kbd "gd") 'cider-find-var)
   ((kbd "gf") 'cider-find-file)
   ((kbd "gr") 'mimic-find-references)
   ((kbd "K")  'cider-doc))

  ;; http://emacs.stackexchange.com/questions/20779/m-and-m-in-evil-mode
  (evil-leader/set-key-for-mode 'clojure-mode
    "cj" 'cider-jack-in
    "cz" 'cider-jack-in
    "ct" 'cider-test-run-test
    "er" 'cider-eval-region
    "ee" 'cider-eval-last-sexp
    "ex" 'cider-eval-last-sexp-and-replace
    "eb" 'cider-eval-buffer)

  ;; attempt to automatically look up symbol first
  (setq cider-prompt-for-symbol nil)
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

(use-package ac-cider
  :commands (ac-cider-setup)
  :init
  (defun my/ac-cider-setup ()
    "Setting up cider autocompletion."
    (my/ac-add-mode-to-modes 'cider-mode)
    (my/ac-add-mode-to-modes 'cider-repl-mode)
    (my/ac-setup-default-sources)
    (ac-flyspell-workaround)
    (ac-cider-setup))
  (add-hook 'cider-mode-hook #'my/ac-cider-setup)
  (add-hook 'cider-repl-mode-hook #'my/ac-cider-setup))

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

;;; Functional

;; Erlang
(use-package erlang
  ;; We need to specify erlang-mode explicitely as the package is not called
  ;; erlang-mode.
  :mode
  ("\\.erl\\'" . erlang-mode)
  ("\\.hrl\\'" . erlang-mode)
  ("\\.xrl\\'" . erlang-mode)
  :config
  ;; http://erlang.org/pipermail/erlang-questions/2003-June/009103.html
  (setq hs-special-modes-alist
        (cons '(erlang-mode
                "^\\([a-z][a-zA-Z0-9_]*\\|'[^\n']*[^\\]'\\)\\s *(" nil "%"
                erlang-end-of-clause) hs-special-modes-alist))
  (setq tab-width 4)
  (setq erlang-indent-level 4)
  (setq erlang-font-lock-level-4))

;; Haskell
(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'completion-ignored-extensions ".hi"))

;;; Web

;; http://web-mode.org/
(use-package web-mode
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.blade\\.php\\'" . web-mode)
  ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jsp\\'" . web-mode)
  :config
  (global-unset-key (kbd "C-d"))
  (global-set-key (kbd "C-d") 'evil-scroll-down))

;; Php
(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

;;; Extras

;; Yaml
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

;; Json
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;; Shell
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; Vimscript
(use-package vimrc-mode
  :mode ("\\.vimrc\\'" . vimrc-mode))

;; colors for various 'color codes' aka hex strings
(use-package rainbow-mode
  :commands (rainbow-mode)
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  :diminish rainbow-mode
  :config
  (rainbow-mode 1))

;; Documentation using Dash or Zeal
(use-package dash-at-point
  :if (on-osx)
  :commands (dash-at-point
             dash-at-point-query)
  :init
  (defun dash-at-point-query ()
    "Calls dash-at-point with editing."
    (interactive)
    (let ((current-prefix-arg '(4))) ; C-u
      (call-interactively 'dash-at-point)))
  :bind (:map evil-normal-state-map
              ("g?" . dash-at-point-query)
              ("g/" . dash-at-point)
              ("K" . dash-at-point))
  :config
  (add-to-list 'dash-at-point-mode-alist '(cider-mode . "clojure"))
  (add-to-list 'dash-at-point-mode-alist '(cider-repl-mode . "clojure")))

(use-package zeal-at-point
  :if (not (on-osx))
  :commands (zeal-at-point)
  :init
  :bind (:map evil-normal-state-map
              ("g?" . zeal-at-point-search)
              ("g/" . zeal-at-point)
              ("K" . zeal-at-point))
  :config
  (add-to-list 'zeal-at-point-mode-alist '(cider-mode . "clojure"))
  (add-to-list 'zeal-at-point-mode-alist '(cider-repl-mode . "clojure")))

;;;; End Languages

;;;; Begin Functions

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
          (untabify (point-min) (point-max))
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

;;(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  "Closes compilation window on successful compile."
  (cond ((string-match "finished" string)
         (message "Build maybe successful: closing window.")
         (run-with-timer 1 nil
                         'delete-window
                         (get-buffer-window buffer t)))
        (t
         (message "Compilation exited abnormally: %s" string))))

;;;; End Functions

;;;; Begin Org Mode

(use-package org
  ;; associate .org files with org-mode inside of emacs
  :commands (org-agenda)
  :mode ("\\.org\\'" . org-mode)
  :init
  ;; hotkeys for org-mode
  :bind (("\C-cl" . org-store-link)
         ("\C-cc" . org-capture)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-iswitchb))
  :config
  (evil-define-key 'normal org-mode-map
    (kbd "g.") 'org-open-at-point
    (kbd "TAB") 'org-cycle
    ;; to be backward compatible with older org version
    (kbd "]") (if (fboundp 'org-forward-same-level)
                  'org-forward-same-level
                'org-forward-heading-same-level)
    (kbd "[") (if (fboundp 'org-backward-same-level)
                  'org-backward-same-level
                'org-backward-heading-same-level))

  (evil-define-key 'emacs org-agenda-mode-map
    (kbd "b") 'evil-backward-word-begin
    (kbd "w") 'evil-forward-word-begin
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line
    (kbd "h") 'evil-backward-char
    (kbd "l") 'evil-forward-char)
  (my/set-evil-shift-width 4)

  ;; http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 ()
                                  (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (custom-theme-set-faces
   'user
   `(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   `(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   `(org-level-2 ((t (:inherit outline-2 :height 1.2 :weight bold))))
   `(org-level-1 ((t (:inherit outline-1 :height 1.3 :weight bold))))
   `(org-document-title ((t (:weight bold :height 1.4)))))

  (when (on-osx)
    (setq org-directory "~/Dropbox/Notes")
    (setq org-agenda-files '("~/Dropbox/Notes")))
  (when (on-windows)
    (setq org-directory "C:/Users/james/Dropbox/Notes")
    (setq org-agenda-files '("C:/Users/james/Dropbox/Notes")))

  (setq org-capture-templates
        '(;; standard todo
          ("t" "Todo" entry
           (file+headline (concat org-directory "/mine.org") "Tasks")
           "* TODO %u %a %?\n")
          ;; handle this message in the next two days
          ("H" "High Priority" entry
           (file+headline (concat org-directory "/mine.org") "High Priority")
           "* TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
          ;; wait for an e-mail reply
          ("W" "Wait for Reply"
           entry (file+headline (concat org-directory "/mine.org") "Waiting")
           "* WAIT %u %a %?\n")))

  (setq org-src-fontify-natively t)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-goto-interface 'outline-path-completion
        org-goto-max-level 10))

(use-package org-bullets
  :after org
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)))
  :config
  (org-bullets-mode 1))

;;;; End Org Mode

;;;; Begin Mail

;;; authentication
(use-package auth-source
  :ensure nil
  :config
  (add-to-list 'auth-sources "~/.emacs.d/mail/.email.gpg"))

;;; sending mail

;; use msmtp
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/local/bin/msmtp")
;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

;;; receiving mail

;; add the source shipped with mu to load-path
(when (on-osx)
  (add-to-list 'load-path
               (concat
                (replace-regexp-in-string "\n" "" (shell-command-to-string "echo $(brew --prefix mu)"))
                "/share/emacs/site-lisp/mu4e")))

(use-package mu4e
  :ensure nil
  :commands (mu4e)
  :config
  (use-package org-mu4e
    :ensure nil)
  (use-package evil-mu4e)
  (use-package mu4e-maildirs-extension
    :config
    (mu4e-maildirs-extension))

  (evil-define-key 'motion mu4e-view-mode-map
    (kbd "q") 'mu4e~view-quit-buffer
    (kbd "g.") 'mu4e~view-browse-url-from-binding
    (kbd "C-n") 'mu4e-view-headers-next
    (kbd "C-p") 'mu4e-view-headers-prev)

  (evil-define-key 'motion mu4e-headers-mode-map
    (kbd "q") 'mu4e~headers-quit-buffer
    (kbd "+") 'mu4e-headers-mark-for-flag)

  (evil-define-key 'motion mu4e-main-mode-map
    (kbd "gR") 'mu4e-update-mail-and-index
    (kbd "gM") 'mu4e~headers-jump-to-maildir
    (kbd "C-j") nil
    (kbd "C-k") nil
    (kbd "C-n") 'mu4e-headers-next
    (kbd "C-p") 'mu4e-headers-prev)

  ;; synergy with org mode capture
  (define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
  (define-key mu4e-view-mode-map    (kbd "C-c c") 'org-mu4e-store-and-capture)

  ;; additional actions -> a key
  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)

  ;; adding cc header
  (add-hook 'mu4e-compose-mode-hook
            (defun my-add-cc ()
              "Add a cc: header."
              (save-excursion (message-add-header "Cc: \n"))))

  ;; ivy completion
  (setq mu4e-completing-read-function 'ivy-completing-read)

  ;; don't show every a thread for every message in the inbox
  (setq mu4e-headers-show-threads nil)
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
  (setq mu4e-use-fancy-chars nil)
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

  ;; setting up contexts between personal and work
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Personal"
             :enter-func (lambda ()
                           (mu4e-message "Switched to the Personal context"))
             ;; leave-func not defined
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "ja.nguyen@gmail.com")))
             :vars '((user-mail-address . "ja.nguyen@gmail.com")
                     (user-full-name . "James Nguyen")
                     (mu4e-compose-signature .
                                             (concat
                                              "James Nguyen\n"))))
           ,(make-mu4e-context
             :name "Work"
             :enter-func (lambda ()
                           (mu4e-message "Switched to the Work context"))
             ;; leave-fun not defined
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "james@whoshere.net")))
             :vars '((user-mail-address . "james@whoshere.net")
                     (user-full-name . "James Nguyen")
                     (mu4e-compose-signature .
                                             (concat
                                              "James Nguyen\n"))))))

  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask-if-none))

;;;; End Mail

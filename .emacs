;; ----------------- START -----------------------------------
;; loadpath
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d/packages/")

;; no startup message
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("68769179097d800e415631967544f8b2001dae07972939446e21438b1010748c" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

;; package manager
(require 'package)
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; ----------------- START -----------------------------------

;; ----------------- MAC --------------------------------------

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Consolas")
  (set-face-attribute 'default nil :height 120)
  ;;(set-face-attribute 'default nil :family "PragmataPro")
  ;;(set-face-attribute 'default nil :height 130)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  ;; mac needs the menu bar 
  (if window-system
      (menu-bar-mode 1))
)

;; ----------------- MAC --------------------------------------

;; ----------------- MAPPINGS ---------------------------------

;; another way to use meta
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; ways to delete word backwards
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "<F2>") 'hs-toggle-hiding) ;; toggle folds
(global-set-key (kbd "<F3>") 'hs-show-all) ;; open all folds
(global-set-key (kbd "<F4>") 'hs-hide-all) ;; hides all folds
(global-set-key (kbd "<F5>") 'gdb) ;; debugging
(global-set-key (kbd "<F6>") 'recompile) ;; recompile
(global-set-key (kbd "<F7>") 'compile) ;; compiling
(global-set-key (kbd "\C-c o") 'occur) ;; occur!!

;; ----------------- MAPPINGS ---------------------------------

;; ----------------- EDITING ----------------------------------
;; line numbers
(require 'linum)
(global-linum-mode 1)

(require 'linum-relative)


;; highlight parentheses
(require 'paren)
(show-paren-mode t)

(setq kill-whole-line t) ;; kills entire line if at the beginning
(fset 'yes-or-no-p 'y-or-n-p) ;; yes or no to y or n
(column-number-mode 1) ;; makes the column number show up
(define-key global-map (kbd "RET") 'newline-and-indent) ;; autoindent

;; folding 
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;; indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-default-style "k&r"
      c-basic-offset 4)

;; ido mode for buffers
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; reload buffers
(global-auto-revert-mode t)

;; ctrn-n starts new lines
(setq next-line-add-newlines t)

;; autopairs
(require 'autopair)
(autopair-global-mode)

;; ----------------- EDITING ----------------------------------

;; ----------------- FUNCTIONS --------------------------------
;; indent whole buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; ----------------- FUNCTIONS --------------------------------

;; ----------------- NAVIGATION -------------------------------

;; windmove & framemove
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings 'meta))

;; navigating splits in emacs mode
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down) 

(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; switch between header and implementation
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c p") 'ff-find-other-file)))

;; ----------------- NAVIGATION -------------------------------

;; ----------------- EVIL -------------------------------------

;; undo-true
(require 'undo-tree)
(global-undo-tree-mode)

(add-to-list 'load-path "~/.emacs.d/packages/evil")
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map "L" 'evil-end-of-line)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-normal-state-map "\C-h" 'windmove-left) ;; move left around split
(define-key evil-normal-state-map "\C-l" 'windmove-right) ;; move right around split
(define-key evil-normal-state-map "\C-k" 'windmove-up) ;; move up a split
(define-key evil-normal-state-map "\C-j" 'windmove-down) ;; move down a split
(define-key evil-normal-state-map "\C-p" 'ff-find-other-file) ;; open .h or .cpp
(define-key evil-visual-state-map "L" 'evil-end-of-line)
(define-key evil-visual-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
(define-key evil-visual-state-map ";" 'evil-ex)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-normal-state-map (kbd "C-u") 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape]
            'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape]
            'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape]
            'minibuffer-keyboard-quit)

(define-key evil-normal-state-map [f2] 'hs-toggle-hiding)
(define-key evil-normal-state-map [f3] 'hs-show-all)
(define-key evil-normal-state-map [f4] 'hs-hide-all)
(define-key evil-normal-state-map [f5] 'gdb)
(define-key evil-normal-state-map [f6] 'recompile)
(define-key evil-normal-state-map [f7] 'compile)

;; evil surround
(add-to-list 'load-path "~/.emacs.d/packages/evil-surround")
(require 'surround)
(global-surround-mode 1)

;; ----------------- EVIL -------------------------------------

;; ----------------- AUTO COMPLETE ----------------------------
(add-to-list 'load-path "~/.emacs.d/packages/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/ac-dict")
(ac-config-default)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq ac-source-yasnippet nil)

;; ----------------- AUTO COMPLETE ----------------------------

;; ----------------- CEDET ------------------------------------

;; project management
(global-ede-mode 1)
;; integration with imenu
(defun my-semantic-hook ()
  (imenu-add-to-menubar "Tags"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(defun my-c-mode-cedet-hook ()
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; semantic
(semantic-mode 1)
(add-hook 'c++-mode (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))

;; ----------------- CEDET ------------------------------------

;; ----------------- THEME ------------------------------------
;; color schemes
(add-to-list 'custom-theme-load-path "~/.emacs.d/packages/solarized")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; theme changer based on time
(add-to-list 'load-path "~/.emacs.d/packages/theme-changer")
(setq calendar-location-name "Dallas, TX")
(setq calendar-latitude 32.85)
(setq calendar-longitude -96.85)
(require 'theme-changer)
;;(change-theme 'solarized-dark 'solarized-light)
(change-theme 'solarized-light 'solarized-dark)
;;(change-theme 'adwaita 'adwaita)

;; disable ui fluff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; ----------------- THEME ------------------------------------


;; ----------------- FILES ------------------------------------

;; stop creating backup and #autosave# files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; get the zsh path
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; ----------------- FILES ------------------------------------

;; Tips
;; <M-x> ielm opens up the ELISP interpreter.


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; loadpath
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; no startup message
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
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

;; disable ui fluff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; another way to use meta
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; ways to delete word backwards
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(when (eq system-type 'darwin)
  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "Consolas")
  (set-face-attribute 'default nil :height 120)
  ;;(set-face-attribute 'default nil :family "PragmataPro")
  ;;(set-face-attribute 'default nil :height 130)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  ;; mac needs the menu bar 
  (if window-system
      (menu-bar-mode 1))
)


;; line numbers
(require 'linum)
(global-linum-mode 1)

;; autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; highlight parentheses
(require 'paren)
(show-paren-mode t)

;; unset keys

;; key mappings
(global-set-key (kbd "<f2>") 'hs-toggle-hiding) ;; toggle folds
(global-set-key (kbd "<f3>") 'hs-show-all) ;; open all folds
(global-set-key (kbd "<f4>") 'hs-hide-all) ;; hides all folds
(global-set-key (kbd "<f5>") 'gdb) ;; debugging
(global-set-key (kbd "<f6>") 'recompile) ;; recompile
(global-set-key (kbd "<f7>") 'compile) ;; compiling
(global-set-key (kbd "\C-c o") 'occur) ;; occur!!

;; editing
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

;; indent whole buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

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

;; undo-true
(require 'undo-tree)
(global-undo-tree-mode)

;; windmove & framemove
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings 'meta))

(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down) 

(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; switch between header and implementation
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c p") 'ff-find-other-file)))

;; evil
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
(define-key evil-visual-state-map ";" 'evil-ex)

(add-to-list 'load-path "~/.emacs.d/packages/")

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/packages/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/ac-dict")
(ac-config-default)


(global-ede-mode 1)                      ; Enable the Project management system

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq ac-source-yasnippet nil)

;; semantic
(semantic-mode 1)
(add-hook 'c++-mode (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))

;; integration with imenu
(defun my-semantic-hook ()
  (imenu-add-to-menubar "Tags"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(defun my-c-mode-cedet-hook ()
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; color schemes
(add-to-list 'custom-theme-load-path "~/.emacs.d/packages/solarized")

;; theme changer based on time
(add-to-list 'load-path "~/.emacs.d/packages/theme-changer")
(setq calendar-location-name "Dallas, TX")
(setq calendar-latitude 32.85)
(setq calendar-longitude -96.85)
(require 'theme-changer)
;;(change-theme 'solarized-dark 'solarized-light)
(change-theme 'adwaita 'adwaita)

;; Tips
;; <M-x> ielm opens up the ELISP interpreter.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


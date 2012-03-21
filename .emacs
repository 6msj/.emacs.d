(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;; loadpath
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; remove toolbar
(tool-bar-mode -1)

;; line numbers
(require 'linum)
(global-linum-mode 1)

;; autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers_


;; ctrl+c ctrl+v
(cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; tab/auto completion
;;(require 'smart-tab)
;;(global-smart-tab-mode 1)

;; highlight parentheses
(require 'paren)
(show-paren-mode t)

;; key mappings
(global-set-key (kbd "<f2>") 'hs-toggle-hiding) ;; toggle folds
(global-set-key (kbd "<f3>") 'hs-show-all) ;; open all folds
(global-set-key (kbd "<f4>") 'hs-hide-all) ;; hides all folds
(global-set-key (kbd "<f5>") 'gdb) ;; debugging
(global-set-key (kbd "<f6>") 'recompile) ;; recompile
(global-set-key (kbd "<f7>") 'compile) ;; compiling
(global-set-key (kbd "\C-c o") 'occur) ;; occur!!
(global-set-key (kbd "C-S-n") 'scroll-up) ;; pagedown
(global-set-key (kbd "C-S-p") 'scroll-down) ;; pageup

;; editing
(setq kill-whole-line t) ;; kills entire line if at the beginning
(fset 'yes-or-no-p 'y-or-n-p) ;; yes or no to y or n
(column-number-mode 1) ;; makes the column number show up
(define-key global-map (kbd "RET") 'newline-and-indent) ;; autoindent

;; files
(require 'xclip) ;; link clipboard
    (xclip-mode 1) 

(setq x-select-enable-clipboard t) ;; link clipboard

;; solarized color scheme
(require 'color-theme-solarized)

;; theme changer based on time
(add-to-list 'load-path "~/.emacs.d/packages/theme-changer")
(setq calendar-location-name "Dallas, TX")
(setq calendar-latitude 32.85)
(setq calendar-longitude -96.85)
(require 'theme-changer)
(change-theme 'color-theme-solarized-light 'color-theme-solarized-dark)

;; org mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; folding 
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;; autocompletion
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

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

;; auto fill mode
(global-set-key (kbd "C-c q") 'auto-fill-mode)


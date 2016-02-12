;; - Begin Init - ;;
;; loadpath
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d/packages/")

;; disable startup screen
(setq inhibit-startup-screen t)

;; mute system sound
(setq ring-bell-function #'ignore)

;;auto-complete 
;;fuzzy 
;;grizzl htmlize jedi 
;;projectile 
;;yasnippet 

;; packages to be installed
(setq package-list '(
                     ; theme
                     spacemacs-theme theme-changer spaceline rainbow-delimiters git-gutter-fringe diminish
                     ; experience
                     smooth-scrolling fold-dwim fold-dwim-org magit autopair framemove xclip pbcopy
                     ; file management
                     flx-ido s dash; file management
                     ; evil
                     evil evil-leader evil-nerd-commenter surround undo-tree key-chord neotree
                     ; languages
                     haskell-mode
                     ; extras
                     org
                    ))

;; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; - End Init - ;;

;; - Begin Theme - ;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/packages/solarized")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(require 'dash)
(require 's)

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

;; theme changer based on time
(setq calendar-location-name "Dallas, TX")
(setq calendar-latitude 32.85)
(setq calendar-longitude -96.85)
(require 'theme-changer)
(change-theme 'spacesmacs-light 'spacemacs-dark)

(require 'spaceline-config)
(spaceline-spacemacs-theme)
(setq powerline-default-separator 'wave)

;; disable ui fluff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; cursorline
;;(global-hl-line-mode 1)

;; colorful delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; diminish modeline clutter
(require 'diminish)

(when (require 'diminish nil 'noerror)
  (eval-after-load "undo-tree"
                   '(diminish 'undo-tree-mode))
  (eval-after-load "autopair"
                   '(diminish 'autopair-mode))
  (eval-after-load "hideshow"
                   '(diminish 'hs-minor-mode))
  )

;; End Theme

;; Begin Platform

;; Mac Specific
;; https://github.com/adobe-fonts/source-code-pro
(when (eq system-type 'darwin)
  (defun find-and-set-font (&rest candidates)
  "Set the first font found in CANDIDATES."
  (let ((font (cl-find-if (lambda (f) (find-font (font-spec :name f)))
                          candidates)))
    (when font
      (set-face-attribute 'default nil :font font))
    font))
    (find-and-set-font "Source Code Pro-12" "Consolas-12" "Envy Code R-12" "DejaVu Sans Mono-11" "Menlo-12")

  (setenv "PATH" (concat (getenv "HOME") ".bin:"
                         "/usr/local/bin:"
                         "/usr/local/rsense-0.3:"
                         "/usr/local/share/python:"
                         "/usr/local/heroku/bin:"
                         "/Users/james/Developer/Android/sdk/tools:"
                         "/Library/PostgreSQL/9.2/bin"
                         "/opt/local/bin:"
                         "/opt/local/sbin:"
                         "/Applications/xampp/xamppfiles/bin:"
                         (getenv "PATH")))
)

(when (eq system-type 'linux)
  (set-face-attribute 'default nil :family "Inconsolata For Powerline")
  (set-face-attribute 'default nil :height 130)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
)

;; mac needs the menu bar
(if window-system
  (menu-bar-mode 1))

;; End Platform

;; Begin Experience 

;;;; scroll by 1 line at the end of the file
;;(setq scroll-step 1
;;      scroll-conservatively 10000)
;;;; set mouse wheel to scroll one line at a time
;;(setq mouse-wheel-progressive-speed nil)
;;;; scroll one line at a time (less "jumpy" than defaults)
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;;;; don't accelerate scrolling
;;(setq mouse-wheel-progressive-speed nil)
;;;; scroll window under mouse
;;(setq mouse-wheel-follow-mouse 't)
;;
;;(require 'smooth-scrolling)
;;(set-variable 'smooth-scroll-margin 5)
;;(setq scroll-preserve-screen-position 1)

;; wraps line visually when it reaches the end
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; git gutter
(require 'git-gutter-fringe)
(set-face-foreground 'git-gutter-fr:modified "yellow")
(set-face-foreground 'git-gutter-fr:added    "blue")
(set-face-foreground 'git-gutter-fr:deleted  "white")
;;(add-hook 'prog-mode-hook 'git-gutter)
(global-git-gutter-mode t)

(require 'fold-dwim-org)
;; hide the comments too when you do a 'hs-hide-all'
(setq hs-hide-comments nil)
;; Set whether isearch opens folded comments, code, or both
;; where x is code, comments, t (both), or nil (neither)
(setq hs-isearch-open 'x)

;; line numbers
(require 'linum)
(global-linum-mode 1)
;;(require 'linum-relative)

;; highlight parentheses
(require 'paren)
(show-paren-mode t)

;; - End Experience - ;;

;; - Begin Mappings - ;;

;; Another way to use Meta.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Ways to delete word backwards.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "C-c o") 'occur) ;; occur!!
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file) ;; rename 
(global-set-key (kbd "C-c C-g") 'magit-status) ;; git!!
(global-set-key (kbd "C-c g") 'magit-status) ;; git!!

;; - End Mappings - ;;

;; - Begin Editing - ;;

;; tramp for remote editing
(require 'tramp)
(setq tramp-default-method "scp")

;; enable transient mark mode
(transient-mark-mode 1)

(setq kill-whole-line t) ;; kills entire line if at the beginning
(fset 'yes-or-no-p 'y-or-n-p) ;; yes or no to y or n
(column-number-mode 1) ;; makes the column number show up

;; autoindent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; add auto indent to all programming modes
(add-hook 'prog-mode-hook 'set-newline-and-indent)

;; indenting related
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-default-style "k&r"
      c-basic-offset 4)

;; folding 
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;; reload buffers
(global-auto-revert-mode t)

;; autopairs
(require 'autopair)
(autopair-global-mode 1)

;; sync clipboards
(when (eq system-type 'linux)
  (require 'xclip)
  (xclip-mode 1)
)

;; mac copy paste
(if (window-system)
    (progn)
  (when (eq system-type 'darwin)
    (require 'pbcopy)
    (turn-on-pbcopy)))

;; - End Editing - ;;

;; - Begin Navigation - ;;

;; windmove & framemove
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings 'meta))

;; navigating splits in emacs mode
;;(global-set-key (kbd "C-c h") 'windmove-left)
;;(global-set-key (kbd "C-c l") 'windmove-right)
;;(global-set-key (kbd "C-c k") 'windmove-up)
;;(global-set-key (kbd "C-c j") 'windmove-down) 

(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; Projectile
;;(projectile-global-mode)
;;(setq projectile-completion-system 'grizzl)

;; - End Navigation - ;;

;; - Begin File Management - ;; 

;; no autosave
(setq auto-save-default nil)

;; write backup files to own directory
(setq backup-directory-alist
    `(("." . ,(expand-file-name
               (concat user-emacs-directory "backups")))))

;; make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-mode 'both) ;; for buffers and files

;; do not confirm a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-last-directory-history nil)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-use-filename-at-point t) ;; prefer file names near point

;; flx matching
(require 'flx-ido)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;; same filenames get the directory name inserted also
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; most recently used files
(require 'recentf)
    (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
    (recentf-mode 1)
(setq recentf-max-menu-items 25)

;; - End File Management - ;;

;; - Begin Evil - ;;

;; vim style undo
(require 'undo-tree)
(global-undo-tree-mode)

;; regain scroll up with c-u
(setq evil-want-C-u-scroll t)

;; C-i jumps forward in jumplist
(setq evil-want-C-i-jump t)

(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
(define-key evil-visual-state-map ";" 'evil-ex)
(evil-define-key 'normal org-mode-map (kbd "C-i") 'org-cycle) ;; cycle org mode in terminal


(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

;; esc quits
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

;; evil surround
(require 'surround)
(global-surround-mode 1)

(require 'neotree)

;; evil leader
(require 'evil-leader)
(global-evil-leader-mode)
(add-hook 'fundamental-mode 'evil-leader-mode)
(add-hook 'text-mode-hook 'evil-leader-mode)
(add-hook 'prog-mode-hook 'evil-leader-mode)
(add-hook 'speed-bar-mode-hook 'evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
;; evil nerd commenter
    "wh" 'split-window-below
    "wv" 'split-window-right
    "f"  'ido-find-file
    "b"  'ido-switch-buffer
    "="  'iwb
    "r"  'recentf-ido-find-file
    "n"  'neotree-toggle
)

;; occur mode
(evil-set-initial-state 'occur-mode 'motion)
(evil-define-key 'motion occur-mode-map (kbd "RET") 'occur-mode-goto-occurrence)
(evil-define-key 'motion occur-mode-map (kbd "q")   'quit-window)

;; fix black cursor
(setq evil-default-cursor t)


;; - End Evil - ;;

;; - Begin Languages - ;;

;; Haskell 
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-to-list 'completion-ignored-extensions ".hi")
(add-hook 'haskell-mode-hook 'auto-complete-mode)
(add-hook 'haskell-mode-hook 'auto-revert-mode)
(add-hook 'haskell-mode-hook 'fold-dwim-org/minor-mode)
(add-hook 'haskell-mode-hook 'set-newline-and-indent)

;; - End Languages - ;;

;; - Begin Functions - ;;

;; auto indent function using return
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

;; function to rename current buffer file
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

;; indent whole buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; toggle window split
(defun toggle-window-split ()
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

;; rotate windows
(defun rotate-windows-helper(x d)
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))

(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))


;; use variable width font faces in current buffer
(defun my-buffer-face-mode-variable ()
"Set font to a variable width (proportional) fonts in current buffer"
(interactive)
(setq buffer-face-mode-face '(:family "Helvetica" :height 130 :width semi-condensed))
(buffer-face-mode))

;; use monospaced font faces in current buffer
(defun my-buffer-face-mode-fixed ()
"Sets a fixed width (monospace) font in current buffer"
(interactive)
(setq buffer-face-mode-face '(:family "Consolas" :height 120))
(buffer-face-mode))

;; close compilation window on successful compile
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (message "Build maybe successful: closing window.")
         (run-with-timer 1 nil                      
                         'delete-window              
                         (get-buffer-window buffer t)))
        (t                                                                    
         (message "Compilation exited abnormally: %s" string))))

;; function to find recent files using ido
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
;; - End Functions - ;;

;; - Begin Org - ;;

;; folding like Org Mode in all modes
(add-hook 'prog-mode-hook 'fold-dwim-org/minor-mode)
(add-hook 'text-mode-hook 'fold-dwim-org/minor-mode)

;; org mode
(require 'org)

;; associate .org files with org-mode inside of emacs
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; hotkeys for org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-src-fontify-natively t)

;; - End Org - ;;

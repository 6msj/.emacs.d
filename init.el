;; ----------------- START -----------------------------------
;; loadpath
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;;(setenv "ESHELL" (expand-file-name "~/.bin/eshell"))
(add-to-list 'load-path "~/.emacs.d/packages/")

;; no startup message
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("0a47a318b366c8d5bf2a4738ff4cea9988c60f4b3b7f7a31cff565a7889406a5" "4325f9a9fb853d0116a1197ece0dc22027ae67ef798efa6e05e009fe41e2f899" "68769179097d800e415631967544f8b2001dae07972939446e21438b1010748c" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(inhibit-startup-screen t)
 '(show-paren-mode t))

; list the packages you want
(setq package-list '(auto-complete autopair diminish evil evil-leader
                     evil-nerd-commenter fold-dwim fold-dwim-org fuzzy key-chord
                     git-gutter-fringe haskell-mode jedi magit multiple-cursors
                     rainbow-delimiters org projectile smooth-scrolling
                     sr-speedbar surround theme-changer undo-tree xclip yasnippet))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; ----------------- START -----------------------------------

;; ----------------- THEME ------------------------------------
;; color schemes
(add-to-list 'custom-theme-load-path "~/.emacs.d/packages/solarized")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; theme changer based on time
(setq calendar-location-name "Dallas, TX")
(setq calendar-latitude 32.85)
(setq calendar-longitude -96.85)
(require 'theme-changer)
(change-theme 'solarized-light 'solarized-dark)
;;(change-theme 'molokai 'molokai)

;; disable ui fluff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;;(fringe-mode 0)

;; Mac Specific
(when (eq system-type 'darwin)
  ;;(set-face-attribute 'default nil :family "Consolas")
  ;;(set-face-attribute 'default nil :height 120)
  ;;(set-face-attribute 'default nil :family "Inconsolata For Powerline")
  ;;(set-face-attribute 'default nil :height 130)
  ;;(set-face-attribute 'default nil :family "DejaVu Sans Mono")
  ;;(set-face-attribute 'default nil :height 110)
  ;;(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  (defun find-and-set-font (&rest candidates)
  "Set the first font found in CANDIDATES."
  (let ((font (cl-find-if (lambda (f) (find-font (font-spec :name f)))
                          candidates)))
    (when font
      (set-face-attribute 'default nil :font font))
    font))
    (find-and-set-font "Envy Code R-12" "Consolas-12" "DejaVu Sans Mono-11"  "Menlo-12")

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

;; cursorline
(global-hl-line-mode 1)

;; scroll by 1 line at the end of the file
(setq scroll-step 1
      scroll-conservatively 10000)
;; set mouse wheel to scroll one line at a time
(setq mouse-wheel-progressive-speed nil)
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

(require 'smooth-scrolling)
(set-variable 'smooth-scroll-margin 5)
(setq scroll-preserve-screen-position 1)


;; Colorful Delimiters.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Wraps line visually when it reaches the end.
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Set default fonts for programming and for regular text mode.
;;(add-hook 'text-mode-hook 'my-buffer-face-mode-variable)
;;(add-hook 'prog-mode-hook 'my-buffer-face-mode-fixed)

;; Mute system sound.
(setq ring-bell-function #'ignore)

;; Git Gutter
(require 'git-gutter-fringe)
(set-face-foreground 'git-gutter-fr:modified "yellow")
(set-face-foreground 'git-gutter-fr:added    "blue")
(set-face-foreground 'git-gutter-fr:deleted  "white")
;;(add-hook 'prog-mode-hook 'git-gutter)
(global-git-gutter-mode t)

;;(require 'powerline)
;;(setq powerline-arrow-shape 'arrow)

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                  (face-foreground 'mode-line))))
 (add-hook 'post-command-hook
   (lambda ()
     (let ((color (cond ((minibufferp) default-color)
                        ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                        ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                        ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                        (t default-color))))
       (set-face-background 'mode-line (car color))
       (set-face-foreground 'mode-line (cdr color))))))

(require 'fold-dwim-org)
;; Hide the comments too when you do a 'hs-hide-all'
(setq hs-hide-comments nil)
;; Set whether isearch opens folded comments, code, or both
;; where x is code, comments, t (both), or nil (neither)
(setq hs-isearch-open 'x)
;; Add more here

;; ----------------- THEME ------------------------------------

;; ----------------- MAPPINGS ---------------------------------

;; Another way to use Meta.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Ways to delete word backwards.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "<F2>") 'hs-toggle-hiding) ;; toggle folds
(global-set-key (kbd "<F3>") 'hs-show-all) ;; open all folds
(global-set-key (kbd "<F4>") 'hs-hide-all) ;; hides all folds
(global-set-key (kbd "<F5>") 'gud-gdb) ;; debugging
(global-set-key (kbd "<F6>") 'recompile) ;; recompile
(global-set-key (kbd "<F7>") 'compile) ;; compiling
(global-set-key (kbd "C-c o") 'occur) ;; occur!!

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file) ;; rename 
(global-set-key (kbd "C-c C-s") 'sr-speedbar-toggle) ;; toggle speed bar in frame
(global-set-key (kbd "C-c C-g") 'magit-status) ;; git!!
(global-set-key (kbd "C-c g") 'magit-status) ;; git!!


;; ----------------- MAPPINGS ---------------------------------

;; ----------------- EDITING ----------------------------------
;; line numbers
(require 'linum)
(global-linum-mode 1)
;;(require 'linum-relative)

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

;; same filenames get the directory name inserted also
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; reload buffers
(global-auto-revert-mode t)

;; ctrn-n starts new lines
;; (setq next-line-add-newlines t)

;; autopairs
(require 'autopair)
(autopair-global-mode 1)

;; show speedbar in the same frame
(require 'sr-speedbar)
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; sync clipboards
(when (eq system-type 'linux)
  (require 'xclip)
  (xclip-mode 1)
)

(if (window-system)
    (progn)
  (when (eq system-type 'darwin)
    (require 'pbcopy)
    (turn-on-pbcopy)))


;; ----------------- EDITING ----------------------------------

;; ----------------- FUNCTIONS --------------------------------
;; indent whole buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;toggle window split
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

;; Use variable width font faces in current buffer
(defun my-buffer-face-mode-variable ()
"Set font to a variable width (proportional) fonts in current buffer"
(interactive)
(setq buffer-face-mode-face '(:family "Helvetica" :height 130 :width semi-condensed))
(buffer-face-mode))

;; Use monospaced font faces in current buffer
(defun my-buffer-face-mode-fixed ()
"Sets a fixed width (monospace) font in current buffer"
(interactive)
(setq buffer-face-mode-face '(:family "Consolas" :height 120))
(buffer-face-mode))

;; Close compilation window on successful compile.
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (message "Build maybe successful: closing window.")
         (run-with-timer 1 nil                      
                         'delete-window              
                         (get-buffer-window buffer t)))
        (t                                                                    
         (message "Compilation exited abnormally: %s" string))))

(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

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

;; ----------------- FUNCTIONS --------------------------------

;; ----------------- NAVIGATION -------------------------------

;; Most Recently Used Files.
(require 'recentf)
    (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
    (recentf-mode 1)
(setq recentf-max-menu-items 25)

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

;; Projectile
(projectile-global-mode)

;; ----------------- NAVIGATION -------------------------------

;; ----------------- EVIL -------------------------------------

;; undo-true
(require 'undo-tree)
(global-undo-tree-mode)

(require 'evil)
(evil-mode 1)
;;(define-key evil-normal-state-map "L" 'evil-end-of-line)
;;(define-key evil-normal-state-map "H" 'evil-first-non-blank)
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
(evil-define-key 'normal org-mode-map (kbd "C-i") 'org-cycle) ;; cycle org mode in terminal
;;(define-key evil-normal-state-map "J" (kbd "10j"))
;;(define-key evil-visual-state-map "J" (kbd "10j"))
;;(define-key evil-normal-state-map "K" (kbd "10k"))
;;(define-key evil-visual-state-map "K" (kbd "10k"))
;;(define-key some-relevant-keymap (kbd "J") (kbd "10j"))
;;(define-key evil-normal-state-map "J" (lambda () (interactive) (forward-line 10)))
;;(define-key evil-visual-state-map "J" (lambda () (interactive) (forward-line 10)))
;;(define-key evil-normal-state-map "K" (lambda () (interactive) (forward-line -10)))
;;(define-key evil-visual-state-map "K" (lambda () (interactive) (forward-line -10)))

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

;; esc quits
;;(define-key evil-normal-state-map [escape] 'keyboard-quit)
;;(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

(define-key evil-normal-state-map [f2] 'hs-toggle-hiding)
(define-key evil-normal-state-map [f3] 'hs-show-all)
(define-key evil-normal-state-map [f4] 'hs-hide-all)
(define-key evil-normal-state-map [f5] 'gud-gdb)
(define-key evil-normal-state-map [f6] 'recompile)
(define-key evil-normal-state-map [f7] 'compile)
(define-key evil-normal-state-map [f8] 'eval-last-sexp)
(define-key evil-motion-state-map [f8] 'eval-last-sexp)

;; evil surround
(require 'surround)
(global-surround-mode 1)

;; evil leader
(require 'evil-leader)
(global-evil-leader-mode)
(add-hook 'text-mode-hook 'evil-leader-mode)
(add-hook 'prog-mode-hook 'evil-leader-mode)
(add-hook 'speed-bar-mode-hook 'evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
;; evil nerd commenter
    "ci" 'evilnc-comment-or-uncomment-lines
    "cc" 'evilnc-comment-or-uncomment-to-the-line
    "v"  (lambda() (interactive)(find-file "~/.emacs.d/init.el"))
    "wh" 'split-window-below
    "wv" 'split-window-right
    "f"  'ido-find-file
    "b"  'ido-switch-buffer
    "n"  'sr-speedbar-toggle
    "="  'iwb
    "r"  'recentf-ido-find-file
)

;; Occur Mode
(evil-set-initial-state 'occur-mode 'motion)
(evil-define-key 'motion occur-mode-map (kbd "RET") 'occur-mode-goto-occurrence)
(evil-define-key 'motion occur-mode-map (kbd "q")   'quit-window)

;; fix black cursor
(setq evil-default-cursor t)

;; ----------------- EVIL -------------------------------------

;; ----------------- AUTO COMPLETE ----------------------------

;; yasnippet
;;(require 'yasnippet)
;;(yas-global-mode 1)
;;(setq ac-source-yasnippet nil)
;;(setq yas/prompt-functions '(yas/x-prompt 'yas/ido-prompt))

;; autocompletion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/ac-dict")
;;(add-to-list 'ac-sources 'ac-source-yasnippet)
(ac-config-default)

;; Python Completion with Jedi.
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)


;; ----------------- AUTO COMPLETE ----------------------------

;; ----------------- CEDET ------------------------------------

;; project management
;; (global-ede-mode 1)

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

;; ----------------- FILES ------------------------------------

;; No autosave
(setq auto-save-default nil)

;; Write backup files to own directory
(setq backup-directory-alist
    `(("." . ,(expand-file-name
               (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)


;; ----------------- FILES ------------------------------------

;; ----------------- LANGUAGES --------------------------------

;; Haskell 
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-to-list 'completion-ignored-extensions ".hi")
(add-hook 'haskell-mode-hook 'auto-complete-mode)
(add-hook 'haskell-mode-hook 'auto-revert-mode)
(add-hook 'haskell-mode-hook 'fold-dwim-org/minor-mode)

;; ----------------- LANGUAGES --------------------------------

;; ----------------- END --------------------------------------

;; Folding like Org Mode in all modes.
(add-hook 'prog-mode-hook 'fold-dwim-org/minor-mode)
(add-hook 'text-mode-hook 'fold-dwim-org/minor-mode)

;; Diminish modeline clutter
(require 'diminish)

;; Enable transient mark mode.
(transient-mark-mode 1)

;; Load org-mode
(require 'org)

;; Associate .org files with org-mode inside of Emacs
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Hotkeys for org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-src-fontify-natively t)

;; Tips
;; <M-x> ielm : Is the ELISP interpreter.
;; C-h k : Is the describe-key function.
;; C-h f : Is the describe-function function.
;; C-c p C-h : List Projectile Keybindings.

;; ----------------- END --------------------------------------

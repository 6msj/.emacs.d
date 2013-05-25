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

;; package manager
;;(require 'package)
;;;; Add the original Emacs Lisp Package Archive
;;(add-to-list 'package-archives
;;             '("elpa" . "http://tromey.com/elpa/")
;;             '("marma" . "http://marmalade-repo.org/packages/")
;;             '("melpa" . "http://melpa.milkbox.net/") t)

(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))

; Apparently needed for the package auto-complete (why?)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(setq url-http-attempt-keepalives nil)

;; Marmalade
;;(add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/"))
;; Melpa
;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.milkbox.net/"))


;; Download and install packages if they aren't already installed.
(require 'package)
(package-initialize)
(dolist (package '(
                     auto-complete
                     autopair
                     evil
                     evil-leader
                     evil-nerd-commenter
                     fuzzy
                     helm
                     key-chord
                     rainbow-delimiters
                     org
                     smooth-scrolling
                     sr-speedbar
                     surround
                     theme-changer
                     undo-tree
                     yasnippet
                     ))
  (unless (package-installed-p package)
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
;;(change-theme 'solarized-light 'solarized-dark)
(change-theme 'molokai 'molokai)

;; disable ui fluff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(fringe-mode 0)

;; Mac Specific
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Consolas")
  (set-face-attribute 'default nil :height 120)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  ;; mac needs the menu bar
  (if window-system
      (menu-bar-mode 1))
)

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

;; scroll one line at a time (less "jumpy" than defaults)
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;(setq scroll-step 1) ;; keyboard scroll one line at a time

(require 'smooth-scrolling)
(set-variable 'smooth-scroll-margin 5)
(setq scroll-preserve-screen-position 1)

;; Colorful Delimiters.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Wraps line visually when it reaches the end.
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Set default fonts for programming and for regular text mode.
(add-hook 'text-mode-hook 'my-buffer-face-mode-variable)
(add-hook 'prog-mode-hook 'my-buffer-face-mode-fixed)

;; Mute system sound.
(setq ring-bell-function #'ignore)

;; Mode line setup
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; emacsclient [default -- keep?]
   mode-line-client
   "  "
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "      ")))
   "    "
   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ; narrow [default -- keep?]
   " %n "
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "    "
   ; nyan-mode uses nyan cat as an alternative to %p
   (:eval (when nyan-mode (list (nyan-create))))
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "gray60" :background "gray20"
    :inverse-video nil
    :box '(:line-width 6 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray80" :background "gray40"
    :inverse-video nil
    :box '(:line-width 6 :color "gray40" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")

;;(require 'powerline)
;;(powerline-default-theme)


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
(global-set-key (kbd "\C-c o") 'occur) ;; occur!!

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file) ;; rename 

(global-set-key (kbd "C-c C-s") 'sr-speedbar-toggle) ;; toggle speed bar in frame

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
(setq next-line-add-newlines t)

;; autopairs
(require 'autopair)
(autopair-global-mode)

;; show speedbar in the same frame
(require 'sr-speedbar)
(setq speedbar-use-images nil)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

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
(evil-leader/set-leader ",")
(evil-leader/set-key
;; evil nerd commenter
    "ci" 'evilnc-comment-or-uncomment-lines
    "cc" 'evilnc-comment-or-uncomment-to-the-line
    "v"  (lambda() (interactive)(find-file "~/.emacs.d/init.el"))
    "wh" 'split-window-below
    "wv" 'split-window-right
)

;; fix black cursor
(setq evil-default-cursor t)

;; ----------------- EVIL -------------------------------------

;; ----------------- AUTO COMPLETE ----------------------------

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq ac-source-yasnippet nil)
(setq yas/prompt-functions '(yas/x-prompt 'yas/ido-prompt))

;; autocompletion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/ac-dict")
(add-to-list 'ac-sources 'ac-source-yasnippet)
(ac-config-default)


;; ----------------- AUTO COMPLETE ----------------------------

;; ----------------- CEDET ------------------------------------

;; project management
;;(global-ede-mode 1)

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


(if window-system
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path 
          (append
           (split-string-and-unquote path ":")
           exec-path))))

;; ----------------- FILES ------------------------------------

;; ----------------- END --------------------------------------

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

;; Tips
;; <M-x> ielm opens up the ELISP interpreter.

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ----------------- END --------------------------------------

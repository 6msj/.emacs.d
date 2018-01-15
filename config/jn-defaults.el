;;;; -*- lexical-binding: t; -*-

;; Use full gdb-mi UI.
(setq gdb-many-windows t)

;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Mute system sound.
(setq ring-bell-function #'ignore)

;; Warn when opening files bigger than 100MB.
(setq large-file-warning-threshold 100000000)

;; https://www.reddit.com/r/emacs/comments/6hzq1j/whats_the_secret_behind_doom/
;; This might help with long lines.
(setq-default bidi-display-reordering nil)

;; Set default frame size.
(when (display-graphic-p)
  (setq initial-frame-alist (cond
                             ((+macbook-retina-p)
                              '((width . 100) (height . 61)))
                             ((eq system-type 'windows-nt)
                              '((width . 92) (height . 46)))
                             ((+desktop-p)
                              '((width . 132) (height . 86)))
                             (:else
                              '((width . 90) (height . 45))))))

;; https://emacs.stackexchange.com/questions/29441/how-do-i-disable-menu-bar-mode-only-for-tty-frames
(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                       (if (display-graphic-p frame)
                           1
                         0)))

(defun +buffer-predicate (buffer)
  "Exclude buffers that match regex.

If there's not that many buffers, just show all of them."
  (if (memq this-command '(json-pretty-print-buffer))
      t
    (let ((case-fold-search)
          (name (buffer-name buffer))
          (regex (mapconcat
                  (lambda (x) x)
                  '(
                    "\*Help\*"
                    "\*Messages\*"
                    "\*Compile-Log*"
                    "\*Backtrace\*"
                    "\*alchemist-server\*"   ; `alchemist'
                    "\*nrepl-*"              ; `cider'
                    "\*cider-error\*"        ; `cider'
                    "^:"                     ; `dired-sidebar'
                    "\*magit"                ; `magit'
                    "\*omnisharp-debug\*"    ; `omnisharp'
                    "OmniServer"             ; `omnisharp'
                    "\*prodigy-imapnotify-*" ; `prodigy'
                    "\*rbt\*"                ; `rbt-post'
                    "\*RTags Diagnostics\*"  ; `rtags'
                    "\*RTags Log\*"          ; `rtags'
                    "\*inferior-lisp\*"      ; `slime'
                    "\*slime-description\*"  ; `slime'
                    "\*slime-events\*"       ; `slime'
                    "\*xref\*"               ; `xref'
                    "\*ycmd-server\*"        ; `ycmd'
                    ) "\\|")))
      (not (string-match-p regex name)))))

(defun +set-buffer-predicate (&optional frame)
  "Set buffer-predicate on FRAME."
  (set-frame-parameter frame 'buffer-predicate #'+buffer-predicate))

(defun +handle-after-make-frame (&optional frame)
  "Handle `after-make-frame-functions'."
  (contextual-menubar frame)
  (+set-buffer-predicate frame))

(+set-buffer-predicate)
(add-hook 'after-make-frame-functions #'+handle-after-make-frame)

;; Set title of window to current file or buffer name if not a file.
(setq frame-title-format
      '(""(:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

;; Start server only if not already a server.
;; Also don't start a server on windows.
(unless (or (daemonp) (eq system-type 'windows-nt))
  (add-hook 'after-init-hook
            (lambda ()
              (run-with-idle-timer 8 nil #'+start-server))))

(defun +start-server ()
  "Start server emacsclient can connect to."
  ;; `server-running-p' is not autoloaded.
  (load "server")
  (unless (server-running-p)
    (server-start)))

;; https://lists.gnu.org/archive/html/emacs-devel/2017-06/msg00318.html
(when (fboundp 'display-line-numbers-mode)
  (dolist (hook '(prog-mode-hook
                  nroff-mode-hook
                  nxml-mode-hook
                  conf-space-mode-hook))
    (add-hook hook
              (lambda ()
                (when (boundp 'display-line-numbers-widen)
                  (setq-default display-line-numbers-widen t))
                (set-face-attribute 'line-number-current-line nil :weight 'bold)
                (setq display-line-numbers-type 't)
                (display-line-numbers-mode)))))

(use-package nlinum
  :ensure t
  :if (not (boundp 'display-line-numbers))
  :init
  (dolist (hook '(prog-mode-hook
                  nxml-mode-hook
                  conf-space-mode-hook))
    (add-hook hook #'nlinum-mode))
  :config
  (setq nlinum-format "%d ")
  (setq nlinum-highlight-current-line t)
  (use-package nlinum-hl
    :ensure t
    :config
    ;; With `markdown-fontify-code-blocks-natively' enabled in `markdown-mode',
    ;; line numbers tend to vanish next to code blocks.
    (advice-add #'markdown-fontify-code-block-natively
                :after #'nlinum-hl-do-markdown-fontify-region)

    ;; When using `web-mode's code-folding an entire range of line numbers will
    ;; vanish in the affected area.
    (advice-add #'web-mode-fold-or-unfold :after #'nlinum-hl-do-generic-flush)

    ;; Changing fonts can leave nlinum line numbers in their original size; this
    ;; forces them to resize.
    (advice-add #'set-frame-font :after #'nlinum-hl-flush-all-windows)))

;; Save window configurations.
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Set up scrolling behavior depending on platform.
(cond
 ((featurep 'mac-win)
  ;; Enable pixel scrolling on Yamamoto's port.
  ;; Smoother scrolling exists in 25.2-6.5 and up in Yamamoto's port.
  ;; ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-25.2-mac-6.5.tar.gz
  (setq mac-mouse-wheel-mode t)
  (setq mac-mouse-wheel-smooth-scroll t)
  (setq scroll-margin 5
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position nil))
 (:default
  ;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
  (setq scroll-margin 5
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position nil)
  ;; https://superuser.com/questions/1133436/way-too-fast-scrolling-in-emacs-on-osx
  (setq mouse-wheel-scroll-amount (list (if (+desktop-p) 5 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't)))

;; Wrap line when it reaches end.
(setq-default truncate-lines 1)
(global-visual-line-mode 0)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Hide the comments too when doing `hs-hide-all'.
(setq hs-hide-comments nil)

;; Set whether isearch opens folded comments, code, or both.
(setq hs-isearch-open t)

;; Enable transient mark mode.
(transient-mark-mode 1)

;; Kills entire line if at the beginning.
(setq kill-whole-line t)
;; yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; Make the column number show up.
(column-number-mode 1)
;; Don't ask to kill compilation buffer.
(setq compilation-always-kill t)
;; View PDF continuously.
(setq doc-view-continuous t)
;; Allow narrowing.
(put 'narrow-to-region 'disabled nil)

;; emacs 24+ auto indents by default if `electric-indent-mode' is on
;; so disable automatic indent by default
;; but enable it in all programming modes.
(electric-indent-mode 0)

(dolist (mode '(prog-mode-hook
                yaml-mode-hook
                css-mode-hook
                html-mode-hook
                nxml-mode-hook))
  (add-hook mode (lambda ()
                   (electric-indent-local-mode 1))))

;; Set up default indentation settings.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default cperl-indent-level 4)

;; Eldoc
(global-eldoc-mode)

;; Automatically reload buffers on change.
(setq global-auto-revert-non-file-buffers t)
(add-hook 'prog-mode-hook #'auto-revert-mode)
(add-hook 'org-mode-hook #'auto-revert-mode)
(add-hook 'dired-mode-hook #'auto-revert-mode)
(add-hook 'ibuffer-mode-hook #'auto-revert-mode)

;; Quit Emacs without confirming.
(if (>= emacs-major-version 26)
    (setq confirm-kill-processes nil)
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    ;; Try to exit Rtags cleanly.
    (when (bound-and-true-p rtags-rdm-process)
      (delete-process rtags-rdm-process)
      (kill-buffer "*rdm*"))
    (dolist (process-name '("mu4e-update"
                            "Omni-Server"
                            "terminal<1>"
                            "terminal<2>"
                            "terminal<3>"
                            "terminal<4>"
                            "*ansi-term*"
                            "*ansi-term*<2>"
                            "*ansi-term*<3>"
                            "*ansi-term*<4>"
                            "nrepl-connection"
                            "nrepl-server"
                            "Python"
                            "*alchemist mix*"
                            "Alchemist-IEx"
                            "compilation"))
      (let ((process (or
                      (get-buffer-process process-name)
                      (get-process process-name))))
        (when process
          (set-process-query-on-exit-flag process nil))))
    ad-do-it))

;; No Autosave by default
(setq auto-save-default nil)

;; Write backup files to own directory.
;; https://www.emacswiki.org/emacs/BackupDirectory
(defvar +backup-directory (expand-file-name (concat user-emacs-directory "backups"))
  "Location of backup directory.")

(setq backup-directory-alist
      `((".*" . ,+backup-directory)))

;; Purge old backups.
(add-hook 'after-init-hook
          (lambda ()
            (run-with-idle-timer 30 nil #'+delete-backups)))

(defun +delete-backups ()
  "Delete backups."
  (lambda ()
    (let ((week (* 60 60 24 7))
          (current (float-time (current-time))))
      (dolist (file (directory-files +backup-directory t))
        (when (and (backup-file-name-p file)
                   (> (- current (float-time (nth 5 (file-attributes file))))
                      week))
          (message "Deleted backup %s" file)
          (delete-file file))))))

;; Don't make backups of files in version control.
(setq vc-make-backup-files nil)

(use-package uniquify
  ;; Same filenames get the directory name inserted also.
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Set this variable to a non-nil value to speed up display of characters
;; using large fonts, at the price of a larger memory footprint of the
;; Emacs session.
(setq inhibit-compacting-font-caches t)

(setq source-directory
      (if (> emacs-major-version 25)
          "~/Code/emacs/"
        "~/.source/emacs/"))

;; Escape quits.
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Font Lock Task comments.
;; https://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(add-hook 'prog-mode-hook
          (defun +font-lock-add-todos ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\):"
                1 font-lock-warning-face t)))))

;; Font Lock Function calls.
;; https://stackoverflow.com/questions/14715181/emacs-function-call-highlight/14716203
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face))))
      (+standard-modes))

(provide 'jn-defaults)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-defaults.el ends here

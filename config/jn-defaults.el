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

(setq-default fill-column 80)

;; https://lists.gnu.org/archive/html/emacs-devel/2018-06/msg00728.html
(setq w32-pipe-read-delay 0)

(setq confirm-nonexistent-file-or-buffer nil)

;; Set UTF-8 as the default coding system.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Set default frame size.
(when (display-graphic-p)
  (setq initial-frame-alist (cond
                             ((eq system-type 'gnu/linux)
                              '((width . 90) (height . 45)))
                             ((eq system-type 'windows-nt)
                              '((width . 92) (height . 46)))
                             ((j|desktop-p)
                              '((width . 132) (height . 86)))
                             (:else
                              '((width . 100) (height . 61))))))

;; `ediff' settings
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      ediff-diff-options "-w")
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; `message'
(setq message-sendmail-f-is-evil 't
      ;; Use msmtp
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/local/bin/msmtp"
      ;; Don't leave stray buffers behind.
      message-kill-buffer-on-exit t
      ;; Choose the SMTP server according to the from field in the outgoing email.
      message-sendmail-extra-arguments '("--read-envelope-from")
      ;; Add Cc and Bcc headers to the message buffer.
      message-default-mail-headers "Cc: \nBcc: \n")

;; https://emacs.stackexchange.com/questions/29441/how-do-i-disable-menu-bar-mode-only-for-tty-frames
(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                       (if (display-graphic-p frame)
                           1
                         0)))

(defun j|buffer-predicate (buffer)
  "Exclude buffers that match regex.

If there's not that many buffers, just show all of them."
  (if (or (memq this-command '(json-pretty-print-buffer))
          (with-current-buffer buffer (derived-mode-p 'prog-mode
                                                      'vc-dir-mode
                                                      'web-mode)))
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
                    "\*Shell Command Output\*"
                    "\*vc\*"
                    "\*alchemist-server\*"    ; `alchemist'
                    "\*nrepl-*"               ; `cider'
                    "\*cider-error\*"         ; `cider'
                    "^:"                      ; `dired-sidebar'
                    "\*EGLOT"                 ; `eglot'
                    "\*Flymake log\*"         ; `flymake'
                    "\*lsp-intellij stderr\*" ; `lsp-intellij'
                    "magit-process:\*"        ; `magit'
                    "magit-diff:\*"           ; `magit'
                    "magit-revision:\*"       ; `magit'
                    "\*meghanada-*"           ; `meghanada'
                    "\*omnisharp-debug\*"     ; `omnisharp'
                    "OmniServer"              ; `omnisharp'
                    "\*prodigy-imapnotify-*"  ; `prodigy'
                    "\*quickrun\*"            ; `quick-run'
                    "\*rbt\*"                 ; `rbt-post'
                    "\*RTags Diagnostics\*"   ; `rtags'
                    "\*RTags Log\*"           ; `rtags'
                    "\*inferior-lisp\*"       ; `slime'
                    "\*slime-description\*"   ; `slime'
                    "\*slime-events\*"        ; `slime'
                    "\*sync_and_index\*"      ; `mail'
                    "\*tide-server\*"         ; `tide'
                    "\*xref\*"                ; `xref'
                    ) "\\|")))
      (not (string-match-p regex name)))))

(defun j|set-buffer-predicate (&optional frame)
  "Set buffer-predicate on FRAME."
  (set-frame-parameter frame 'buffer-predicate #'j|buffer-predicate))

(defun j|handle-after-make-frame (&optional frame)
  "Handle `after-make-frame-functions'."
  (contextual-menubar frame)
  (j|set-buffer-predicate frame))

(j|set-buffer-predicate)
(add-hook 'after-make-frame-functions #'j|handle-after-make-frame)

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
              (run-with-idle-timer 6 nil #'j|server-start))))

(defun j|server-start ()
  "Start server emacsclient can connect to."
  ;; `server-running-p' is not autoloaded.
  (load "server")
  (unless (server-running-p)
    (server-start)))

;; https://lists.gnu.org/archive/html/emacs-devel/2017-06/msg00318.html
(if (fboundp 'display-line-numbers-mode)
    (dolist (hook '(conf-space-mode-hook
                    dummy-h-mode-hook
                    nroff-mode-hook
                    nxml-mode-hook
                    prog-mode-hook
                    protobuf-mode-hook
                    web-mode-hook))
      (add-hook hook
                (lambda ()
                  (when (boundp 'display-line-numbers-widen)
                    (setq-default display-line-numbers-widen t))
                  (set-face-attribute
                   'line-number-current-line nil
                   :weight 'bold
                   :inherit 'line-number)
                  (setq display-line-numbers-type 't)
                  (display-line-numbers-mode))))
  (use-package nlinum
    :ensure t
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
      (advice-add #'set-frame-font :after #'nlinum-hl-flush-all-windows))))

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
  (setq mouse-wheel-scroll-amount (list (if (j|desktop-p) 5 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't)))

;; Wrap line when it reaches end.
(setq-default truncate-lines 1)
(global-visual-line-mode 0)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Kills entire line if at the beginning.
(setq kill-whole-line t)
;; Don't ask to kill compilation buffer.
(setq compilation-always-kill t)
;; View PDF continuously.
(setq doc-view-continuous t)
;; Allow narrowing.
(put 'narrow-to-region 'disabled nil)

;; Set up default indentation settings.
(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              cperl-indent-level 4)

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

(winner-mode 1) ;; Save windowing state.
(transient-mark-mode 1) ;; Highlight region selection.
(column-number-mode 1) ;; Make the column number show up.
(global-eldoc-mode)

;; Automatically reload buffers on change.
(setq global-auto-revert-non-file-buffers t)

(defun j|auto-revert-mode-if-local ()
  "Turn on `auto-revert-mode' unless remote."
  (unless (file-remote-p default-directory)
    (auto-revert-mode)))
(add-hook 'prog-mode-hook #'j|auto-revert-mode-if-local)
(add-hook 'org-mode-hook #'j|auto-revert-mode-if-local)
(add-hook 'ibuffer-mode-hook #'j|auto-revert-mode-if-local)

(when (>= emacs-major-version 26)
  (setq column-number-indicator-zero-based nil))

;; Quit Emacs without confirming.
;; For Emacs 26 and up.
;; If Emacs 25 or less, just kill processes manually.
(setq confirm-kill-processes nil)

;; Saving and Backups

;; Write backup files to own directory.
;; https://www.emacswiki.org/emacs/BackupDirectory
(defvar j|backup-directory (expand-file-name
                            (concat user-emacs-directory "backups"))
  "Location of backup directory.")

(setq backup-directory-alist
      `((".*" . ,j|backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,j|backup-directory t)))

(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 600)

(setq version-control t
      kept-new-versions 5
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t)

(defvar-local buffer-save-count 0 "Number of teams buffer has been saved.")

(defun buffer-force-backup ()
  "Make a \"per save\" backup on each save.

Gate the number of backups by `buffer-save-count'.

https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files"
  (setq buffer-save-count (1+ buffer-save-count))
  (when (eq (% buffer-save-count 5) 0)
    (setq buffer-save-count 0)
    (let ((buffer-backed-up nil))
      (backup-buffer))))

(add-hook 'before-save-hook 'buffer-force-backup)

(use-package uniquify
  ;; Same filenames get the directory name inserted also.
  :ensure nil
  :defer 3
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Set this variable to a non-nil value to speed up display of characters
;; using large fonts, at the price of a larger memory footprint of the
;; Emacs session.
(setq inhibit-compacting-font-caches t)

(setq source-directory "~/Code/emacs/")

;; Escape quits.
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Font Lock Task comments.
;; https://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(add-hook 'prog-mode-hook
          (defun j|font-lock-add-todos ()
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
      (j|standard-modes))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (setq flymake-no-changes-timeout .6)
  (setq flymake-wrap-around t)

  (custom-set-variables
   '(help-at-pt-timer-delay .7)
   '(help-at-pt-display-when-idle '(flymake-diagnostic))))

(provide 'jn-defaults)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-defaults.el ends here

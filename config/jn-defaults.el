;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

;; Use full gdb-mi UI.
(setq gdb-many-windows t)

;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Mute system sound.
(setq ring-bell-function #'ignore)

;; Warn when opening files bigger than 100MB.
(setq large-file-warning-threshold 100000000)

;; https://www.reddit.com/r/emacs/comments/6hzq1j/whats_the_secret_behind_doom/
;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but we inhibit it there anyway. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help with performance while scrolling.
(setq redisplay-skip-fontification-on-input t)

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
(setq-default locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
;; The clipboard's on Windows could be in a wider (or thinner) encoding than
;; utf-8 (likely UTF-16), so let Emacs/the OS decide what encoding to use there.
(unless WINDOWS-P
  (setq selection-coding-system 'utf-8)) ; with sugar on top

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

;; `tramp'
(setq tramp-terminal-type "tramp")
(setq tramp-verbose 0) ;; Set to 6 for more debugging.
(setq tramp-completion-reread-directory-timeout nil)

(defun j-buffer-predicate (buffer)
  "Exclude buffers that match regex.

If there's not that many buffers, just show all of them."

  (cond
   ((or (memq this-command '(json-pretty-print-buffer))
        (with-current-buffer buffer (derived-mode-p 'prog-mode
                                                    'vc-dir-mode
                                                    'web-mode)))
    :include-buffer)
   ((with-current-buffer buffer (derived-mode-p 'compilation-mode
                                                'dired-sidebar-mode))
    :exclude-buffer nil)
   (:default
    (let ((case-fold-search)
          (name (buffer-name buffer))
          (regex (mapconcat
                  (lambda (x) x)
                  '(
                    "\*Help\*"
                    "\*Messages\*"
                    "\*vc\*"
                    "\*alchemist-server\*"    ; `alchemist'
                    "\*nrepl-*"               ; `cider'
                    "\*cider-error\*"         ; `cider'
                    "\*EGLOT"                 ; `eglot'
                    "\*Flymake log\*"         ; `flymake'
                    "\*lsp-intellij stderr\*" ; `lsp-intellij'
                    "magit-process:\*"        ; `magit'
                    "magit-diff:\*"           ; `magit'
                    "magit-revision:\*"       ; `magit'
                    "\*prodigy-imapnotify-*"  ; `prodigy'
                    "\*rbt\*"                 ; `rbt-post'
                    "\*inferior-lisp\*"       ; `slime'
                    "\*slime-description\*"   ; `slime'
                    "\*slime-events\*"        ; `slime'
                    "\*sync_and_index\*"      ; `mail'
                    "\*tide-server\*"         ; `tide'
                    "\*tramp/\*"              ; `tramp'
                    "\*debug tramp\*"         ; `tramp'
                    "\*xref\*"                ; `xref'
                    ) "\\|")))
      (not (string-match-p regex name))))))

(defun j-set-buffer-predicate (&optional frame)
  "Set buffer-predicate on FRAME."
  (set-frame-parameter frame 'buffer-predicate #'j-buffer-predicate))

(j-set-buffer-predicate)
(add-hook 'after-make-frame-functions #'j-set-buffer-predicate)

;; Set title of window to current file or buffer name if not a file.
;; If `dired', use `default-directory'.
(setq frame-title-format
      '(""(:eval
           (cond
            (buffer-file-name
             (abbreviate-file-name (buffer-file-name)))
            ((derived-mode-p 'dired-mode)
             default-directory)
            (t
             "%b")))))

;; Start server only if not already a server.
;; Also don't start a server on windows.
(unless (or (daemonp) WINDOWS-P)
  (add-hook 'after-init-hook
            (lambda ()
              (run-with-idle-timer 6 nil #'j-server-start))))

(defun j-server-start ()
  "Start server emacsclient can connect to."
  ;; `server-running-p' is not autoloaded.
  (load "server")
  (if (server-running-p)
      (message "Emacs daemon already started...")
    (progn
      (message "Starting Emacs daemon...")
      (server-start))))

;; Set up line numbers.
;; https://lists.gnu.org/archive/html/emacs-devel/2017-06/msg00318.html
(setq-default display-line-numbers-widen t)
(setq-default display-line-numbers-type 't)
(set-face-attribute 'line-number-current-line nil
                    :weight 'bold
                    :inherit 'line-number)
(dolist (hook '(conf-space-mode-hook
                dummy-h-mode-hook
                nroff-mode-hook
                nxml-mode-hook
                prog-mode-hook
                protobuf-mode-hook
                web-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

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
  (setq mouse-wheel-scroll-amount (list (if DESKTOP-P 5 1)))
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

(defun j-auto-revert-mode-if-local ()
  "Turn on `auto-revert-mode' unless remote."
  (unless (file-remote-p default-directory)
    (auto-revert-mode)))

(dolist (hook '(prog-mode-hook
                org-mode-hook
                ibuffer-mode-hook))
  (add-hook hook #'j-auto-revert-mode-if-local))

(when (>= emacs-major-version 26)
  (setq column-number-indicator-zero-based nil))

;; Quit Emacs without confirming.
;; For Emacs 26 and up.
;; If Emacs 25 or less, just kill processes manually.
(setq confirm-kill-processes nil)

;; Saving and Backups

;; Write backup files to own directory.
;; https://www.emacswiki.org/emacs/BackupDirectory
(defvar j-backup-directory (expand-file-name
                            (concat user-emacs-directory "backups"))
  "Location of backup directory.")

(setq backup-directory-alist
      `((".*" . ,j-backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,j-backup-directory t)))

(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 0)

(setq version-control t
      kept-new-versions 5
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files nil)

(use-package uniquify
  ;; Same filenames get the directory name inserted also.
  :ensure nil
  :defer 3
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator nil)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

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
          (defun j-font-lock-add-todos ()
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
      (j-standard-modes))

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

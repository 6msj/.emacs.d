;;;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'subr-x))

(defun byte-compile-my-packages ()
  "Byte compile my own packages."
  (interactive)
  (let ((packages '("dired-sidebar"
                    "evil-collection"
                    "flycheck-gradle"
                    "flycheck-jest"
                    "flycheck-swiftlint"
                    "flycheck-xcode"
                    "flymake-gradle"
                    "flymake-ktlint"
                    "flymake-racket"
                    "fruity-theme"
                    "ibuffer-sidebar"
                    "smart-jump"
                    "vscode-icon-emacs"
                    "matcha")))
    (mapc
     (lambda (package)
       (let ((default-directory
               (expand-file-name package "~/.emacs.d/fork")))
         (byte-recompile-directory default-directory 0 nil)))
     packages)))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY.
https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html"
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defmacro j|make-async-shell-cmd (cmd &optional mode)
  "Create a command that runs asynchronously in a `compilation-mode' buffer.
CMD can be a a string or a list of strings.

    ;; Note the lack of quote for the list.
    (j|make-async-shell-cmd (\"make all\" \"make test\") 'makefile-mode)

If MODE is non-nil, MODE will be preprended to to function's name.

MODE can be a 'symbol (cons), a symbol or a variable that points to a symbol.

    (j|make-async-shell-cmd \"make all\" 'c-mode)
    (j|make-async-shell-cmd \"make all\" c-mode)
    (j|make-async-shell-cmd \"make all\" mode) ;; MODE points to 'c-mode.

These are all valid and results in a function named j|shell-c-mode-make-all.

If MODE is nil, something like:

    (j|make-async-shell-cmd \"make all\")

will result in a function named j|make-all."
  (let ((commands
         (if (consp cmd)
             cmd
           (list cmd))))
    `(progn
       ,@(cl-loop
          for command in commands
          collect
          (let ((funsymbol
                 (intern (concat "j|shell-"
                                 (if mode
                                     (concat
                                      (cond
                                       ((eq (type-of mode) 'cons)
                                        (symbol-name (nth 1 mode)))
                                       ((eq (type-of mode) 'string)
                                        mode)
                                       ((eq (type-of mode) 'symbol)
                                        (if (boundp mode)
                                            (symbol-name (symbol-value mode))
                                          (symbol-name mode)))
                                       (t
                                        (throw 'foo t)))
                                      "-")
                                   nil)
                                 (mapconcat (lambda (s) s)
                                            (split-string command) "-")))))
            `(defun ,funsymbol ()
               ,(concat "Run $ " command ".")
               (interactive)
               (compilation-start
                ,command
                'compilation-mode
                (lambda (_mode-name)
                  (concat "*" (symbol-name ',funsymbol) "*"))
                t)))))))

(defmacro j|make-terminal-dot-app-command (cmd)
  "Creates a function that runs a terminal CMD in Terminal.app."
  (let ((name (intern (concat "j|terminal-"
                              (replace-regexp-in-string " "  "-" cmd)))))
    `(defun ,name ()
       ,(concat "Run " cmd " in Terminal.app.")
       (interactive)
       (let ((dir (if (projectile-project-p)
                      (projectile-project-root)
                    default-directory)))
         (do-applescript
          (format "
 tell application \"Terminal\"
   activate
   try
     do script with command \"cd %s; %s\"
   on error
     beep
   end try
 end tell" dir ,cmd))))))

(j|make-terminal-dot-app-command "make install")

(defun j|monitor-width ()
  "Return monitor size with focus on Macbook monitor."
  (car (cdr (cdr (cdr (assoc 'geometry (frame-monitor-attributes)))))))

(defun j|macbook-retina-p ()
  "Are we on macbook?"
  (if (boundp 'using-macbook-p)
      using-macbook-p
    (defvar using-macbook-p
      (let ((width
             (if-let
                 ((w
                   (let ((width-result))
                     (dolist (display-alist
                              (display-monitor-attributes-list) width-result)
                       ;; (name . "Color LCD")
                       (when-let ((name-of-display
                                   (cdr (assoc 'name display-alist))))
                         (when (string-match-p
                                "Color LCD"
                                (cdr (assoc 'name display-alist)))
                           ;; (workarea 0 23 1280 709)
                           (setq
                            width-result
                            (car
                             (cdr (cdr (cdr
                                        (assoc
                                         'workarea display-alist))))))))))))
                 w
               ;; Give up and return display-pixel-width.
               (j|monitor-width))))
        (or (eq width 1440)
            (eq width 1280))))))

(defun j|desktop-p ()
  "Are we on a desktop?"
  (or
   (eq system-type 'windows-nt)
   (member (system-name) '("jnguyen-mac2"))
   (> (j|monitor-width) 4400)))

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
  (unless indent-tabs-mode
    (untabify (point-min) (point-max))))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (unless indent-tabs-mode
            (untabify (point-min) (point-max)))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(defun j|format-open-buffers ()
  "Format buffers opened with indent region."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless buffer-read-only
        (indent-region (point-min) (point-max))
        (when buffer-file-name
          (save-buffer))))))

(defun whitespace-region-or-buffer-cleanup ()
  "Clean up whitespace in region or buffer"
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (whitespace-cleanup-region (region-beginning) (region-end))
          (message "Cleaned up white space in selected region."))
      (progn
        (whitespace-cleanup)
        (message "Cleaned up white space.")))))

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

(defun rotate-windows-helper (x d)
  "Rotates windows."
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))

(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))

(defun j|explorer-finder ()
  "Opens up file explorer based on operating system."
  (interactive)
  (cond
   ((and (eq system-type 'darwin)
         (fboundp #'reveal-in-osx-finder))
    (reveal-in-osx-finder))
   ((and (eq system-type 'windows-nt)
         (fboundp #'explorer))
    (explorer))
   (t
    (message "Implement `explorer-finder' for this OS!"))))

(defalias 'explore 'j|explorer-finder)
(defalias 'finder 'j|explorer-finder)

(defun j|open-shell ()
  "Opens up a specific terminal depending on operating system."
  (interactive)
  (cond
   ((or
     (eq system-type 'darwin)
     (eq system-type 'gnu/linux))
    (multi-term))
   ((eq system-type 'windows-nt) (eshell))
   (t
    (message "Implement `j|open-shell' for this OS!"))))

(defun j|open-terminal ()
  "Open system terminal."
  (interactive)
  (cond
   ((eq system-type 'darwin)
    (shell-command
     ;; open -a Terminal doesn't allow us to open a particular directory unless
     ;; We use --args AND -n, but -n opens an entirely new Terminal application
     ;; instance on every call, not just a new window. Using the
     ;; bundle here always opens the given directory in a new window.
     (concat "open -b com.apple.terminal " default-directory) nil nil))
   ((eq system-type 'windows-nt)
    ;; https://stackoverflow.com/questions/13505113/how-to-open-the-native-cmd-exe-window-in-emacs
    (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
      (set-process-query-on-exit-flag proc nil)))
   (t
    (message "Implement `j|open-terminal' for this OS!"))))

(defalias 'terminal 'j|open-terminal)

(defun j|find-references ()
  "When we don't have find-usages/find-references,
do a search for the string from projet root to mimic that functionality."
  (interactive)
  (cond
   ((eq system-type 'darwin)
    (ag-project (ag/dwim-at-point)))
   ((j|linux-p)
    (ag-project (ag/dwim-at-point)))
   ((eq system-type 'windows-nt)
    (call-interactively #'rgrep))
   (t
    (message "Implement `j|find-references' for this OS!"))))

(defun j|resize-window ()
  "Resize window to fit contents."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((fit-window-to-buffer-horizontally t))
      (fit-window-to-buffer))))

(defun j|standard-modes ()
  "Return languages that use braces."
  '(c++-mode
    c-mode
    csharp-mode
    css-mode
    elixir-mode
    go-mode
    groovy-mode
    java-mode
    js-mode
    js2-mode
    json-mode
    kotlin-mode
    lua-mode
    mhtml-mode
    objc-mode
    php-mode
    protobuf-mode
    python-mode
    rjsx-mode
    ruby-mode
    rust-mode
    sh-mode
    swift-mode
    typescript-mode
    web-mode))

(defun j|lisp-modes ()
  "Return modes that are lispy.

Copied from `sp-lisp-modes'."
  '(cider-repl-mode
    clojure-mode
    clojurec-mode
    clojurescript-mode
    clojurex-mode
    common-lisp-mode
    emacs-lisp-mode
    eshell-mode
    geiser-mode
    geiser-repl-mode
    inf-clojure-mode
    inferior-emacs-lisp-mode
    inferior-lisp-mode
    inferior-scheme-mode
    lisp-interaction-mode
    lisp-mode
    monroe-mode
    racket-mode
    racket-repl-mode
    scheme-interaction-mode
    scheme-mode
    slime-repl-mode
    stumpwm-mode))

(defun j|lisp-hooks ()
  "Return hooks that are lispy."
  (mapcar (lambda (mode)
            (intern (concat (symbol-name mode) "-hook")))
          (j|lisp-modes)))

(defun j|indent-offset ()
  "Determine through various heuristics indent settings."
  (cond
   ((member major-mode '(web-mode
                         rjsx-mode
                         json-mode
                         js2-mode
                         js-mode
                         js2-jsx-mode
                         js-jsx-mode
                         css-mode
                         html-mode
                         mhtml-mode
                         typescript-mode))
    2)
   ((not (projectile-project-p)) 4)
   (:else 4)))

(defun j|c-set-c-style (alist)
  "Update default c style with ALIST."
  (eval-after-load 'cc-vars (lambda () (push alist c-default-style))))

(defun j|find-file-dwim ()
  "Find file DWIM."
  (interactive)
  (cond
   ((or (eq major-mode 'dired-mode)
        (eq major-mode 'dired-sidebar-mode))
    (let ((default-directory (dired-current-directory)))
      (call-interactively #'find-file)))
   ((derived-mode-p 'magit-mode)
    (if-let ((magit-file (magit-file-at-point)))
        (let ((default-directory
                (file-name-directory
                 (concat (magit-toplevel) magit-file))))
          (call-interactively #'find-file))
      (call-interactively #'find-file)))
   (:default
    (call-interactively #'find-file))))

(defun j|recentf-dwim ()
  "Find recent files DWIM."
  (interactive)
  (cond
   ((bound-and-true-p helm-mode)
    (helm-recentf))
   ((bound-and-true-p ivy-mode)
    (counsel-recentf))
   ((bound-and-true-p ido-mode)
    (ido-recentf-open))
   (:else
    (recentf-open-files))))

(defun j|buffers-dwim ()
  "List buffers DWIM."
  (interactive)
  (cond
   ((bound-and-true-p helm-mode)
    (helm-buffers-list))
   ((bound-and-true-p ivy-mode)
    (ivy-switch-buffer))
   ((bound-and-true-p ido-mode)
    (ido-switch-buffer))
   (:else
    (call-interactively #'switch-to-buffer))))

(defun j|save-all-buffers ()
  "Save all buffers without confirming."
  (interactive)
  (save-some-buffers :all-buffers-no-confirm))

(defun j|get-ip-address (&optional dev)
  "Get the IP-address for device DEV (default: en0)"
  (if (eq system-type 'windows-nt)
      (let ((ipconfig (shell-command-to-string "ipconfig | findstr Address")))
        (string-match "\\(\\([0-9]+.\\)+[0-9]+\\)" ipconfig)
        (match-string 0 ipconfig))
    (let ((dev (if dev dev "en0")))
      (format-network-address (car (network-interface-info dev)) t))))

;; https://www.masteringemacs.org/article/searching-buffers-occur-mode
(defun j|get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (push buf buffer-mode-matches))))
    buffer-mode-matches))

(defun j|multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (j|get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun j|buffer-contains-string-p (string)
  "Check if current buffer contains STRING while preserving mark and match data."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun j|buffer-contains-regex-p (regex)
  "Check if current buffer contains REGEX while preserving mark and match data."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (re-search-forward regex nil t))))

(defun j|paste-column ()
  "Paste a column of text after another column of text."
  (interactive)
  (insert-rectangle
   (split-string
    (if (bound-and-true-p evil-mode)
        (evil-get-register ?0)
      (current-kill 0 t)) "[\r]?\n")))

(defun j|sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

;; https://stackoverflow.com/questions/16090517/elisp-conditionally-change-keybinding/22863701
(defvar j|newline-or-indent-new-comment-line
  `(menu-item
    "" nil :filter
    ,(lambda (_cmd)
       (when (and
              ;; Is point in a comment?
              (nth 4 (syntax-ppss))
              ;; Is the entire line a comment?
              (save-excursion
                ;; Need to go to beginning of text instead of line,
                ;; otherwise, we'll hit whitespace which will always
                ;; return nil when checking if inside a comment.
                (beginning-of-line-text)
                (nth 4 (syntax-ppss))))
         (key-binding (kbd "M-j")))))
  "If in a comment, call function defined by M-j, otherwise fallthrough.

M-j's function is most likely `c-indent-new-comment-line'.")

(defun j|bind-newline-or-indent-comment-line ()
  "Conditionaly bind `j|newline-or-indent-new-comment-line' to RET."
  (when (memq major-mode (j|standard-modes))
    (define-key (symbol-value (intern (format "%S-map" major-mode)))
      (kbd "RET") j|newline-or-indent-new-comment-line)))

(add-hook 'prog-mode-hook
          #'j|bind-newline-or-indent-comment-line)

(defun j|symbol-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

(defun j|counsel-rg ()
  "Call `counsel-rg' with a DWIM symbol."
  (interactive)
  (counsel-rg (j|symbol-at-point)))

(defun j|counsel-ag ()
  "Call `counsel-ag' with a DWIM symbol."
  (interactive)
  (counsel-ag (j|symbol-at-point)))

(defun fill-region-and-comment ()
  "Fill region and comment."
  (interactive)
  (let* ((old-fill-column fill-column)
         (fill-column (- old-fill-column 2)))
    (call-interactively #'fill-region)
    (call-interactively #'comment-region)))

(provide 'jn-functions)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-functions.el ends here

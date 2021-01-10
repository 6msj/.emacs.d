;;;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'subr-x))

(defconst MAC-P      (eq system-type 'darwin))
(defconst LINUX-P    (eq system-type 'gnu/linux))
(defconst WINDOWS-P  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst BSD-P      (eq system-type 'berkeley-unix))
(defconst YT-P       (string-equal "jameshn" (getenv "USER")))
(defconst DESKTOP-P  (or
                      WINDOWS-P
                      (string-match-p "cpro.roa" (system-name))
                      ;; Check width of monitor.
                      (> (cadddr (assoc 'geometry
                                        (frame-monitor-attributes)))
                         4400)))

(defun byte-compile-my-packages ()
  "Byte compile my own packages."
  (interactive)
  (let ((packages '("dired-sidebar"
                    "evil-collection"
                    "flycheck-gradle"
                    "flycheck-swiftlint"
                    "flycheck-xcode"
                    "flymake-gradle"
                    "flymake-ktlint"
                    "flymake-racket"
                    "hg-histedit"
                    "ibuffer-sidebar"
                    "matcha"
                    "smart-jump"
                    "vscode-icon-emacs")))
    (mapc
     (lambda (package)
       (let ((dir (expand-file-name package "~/.emacs.d/fork")))
         (byte-recompile-directory dir 0 nil)))
     packages)))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY.
https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html"
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defmacro j-make-async-shell-cmd (cmd &optional mode)
  "Create a command that runs asynchronously in a `compilation-mode' buffer.
CMD can be a a string or a list of strings.

    ;; Note the lack of quote for the list.
    (j-make-async-shell-cmd (\"make all\" \"make test\") 'makefile-mode)

If MODE is non-nil, MODE will be preprended to to function's name.

MODE can be a 'symbol (cons), a symbol or a variable that points to a symbol.

    (j-make-async-shell-cmd \"make all\" 'c-mode)
    (j-make-async-shell-cmd \"make all\" c-mode)
    (j-make-async-shell-cmd \"make all\" mode) ;; MODE points to 'c-mode.

These are all valid and results in a function named j-shell-c-mode-make-all.

If MODE is nil, something like:

    (j-make-async-shell-cmd \"make all\")

will result in a function named j-make-all."
  (let ((commands
         (if (consp cmd)
             cmd
           (list cmd))))
    `(progn
       ,@(cl-loop
          for command in commands
          collect
          (let ((funsymbol
                 (intern (concat "j-shell-"
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

(defmacro j-make-terminal-dot-app-command (cmd)
  "Creates a function that runs a terminal CMD in Terminal.app."
  (let ((name (intern (concat "j-terminal-"
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

(j-make-terminal-dot-app-command "make install")

(defun j-macbook-retina-p ()
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
               (cadddr (assoc 'geometry (frame-monitor-attributes))))))
        (or (eq width 1440)
            (eq width 1280))))))

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
    (set-window-buffer (car x) (window-buffer (cadr x)))
    (rotate-windows-helper (cdr x) d)))

(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))

(defun j-explorer-finder ()
  "Opens up file explorer based on operating system."
  (interactive)
  (cond
   ((and MAC-P
         (fboundp #'reveal-in-osx-finder))
    (reveal-in-osx-finder))
   ((and WINDOWS-P
         (fboundp #'explorer))
    (explorer))
   (LINUX-P
    (if (executable-find "gio")
        (progn
          (shell-command (format "gio open %s" default-directory))
          (message (format "Opened %s in file browser!" default-directory)))
      (message "On platform Linux but executable gio not found!")))
   (t
    (message "Implement `explorer-finder' for this OS!"))))

(defalias 'explore 'j-explorer-finder)
(defalias 'finder 'j-explorer-finder)

(defun j-open-shell ()
  "Opens up a specific terminal depending on operating system."
  (interactive)
  (cond
   ((or
     BSD-P
     MAC-P
     LINUX-P)
    (if (fboundp 'vterm)
        (vterm)
      (multi-term)))
   (WINDOWS-P (eshell))
   (t
    (message "Implement `j-open-shell' for this OS!"))))

(defun j-open-terminal ()
  "Open system terminal."
  (interactive)
  (cond
   (MAC-P
    (shell-command
     ;; open -a Terminal doesn't allow us to open a particular directory unless
     ;; We use --args AND -n, but -n opens an entirely new Terminal application
     ;; instance on every call, not just a new window. Using the
     ;; bundle here always opens the given directory in a new window.
     (concat "open -b com.apple.terminal " default-directory) nil nil))
   (WINDOWS-P
    ;; https://stackoverflow.com/questions/13505113/how-to-open-the-native-cmd-exe-window-in-emacs
    (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
      (set-process-query-on-exit-flag proc nil)))
   (t
    (message "Implement `j-open-terminal' for this OS!"))))

(defalias 'terminal 'j-open-terminal)

(defun j-find-references ()
  "When we don't have find-usages/find-references,
do a search for the string from projet root to mimic that functionality."
  (interactive)
  (cond
   (MAC-P
    (ag-project (ag/dwim-at-point)))
   ((j-linux-p)
    (ag-project (ag/dwim-at-point)))
   (WINDOWS-P
    (call-interactively #'rgrep))
   (t
    (ag-project (ag/dwim-at-point)))))

(defun j-resize-window ()
  "Resize window to fit contents."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((fit-window-to-buffer-horizontally t))
      (fit-window-to-buffer))))

(defun j-standard-modes ()
  "Return languages that use braces."
  '(borg-mode
    c++-mode
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
    protobuffer-mode
    protobuf-mode
    python-mode
    rjsx-mode
    ruby-mode
    rust-mode
    sh-mode
    swift-mode
    typescript-mode
    web-mode))

(defun j-lisp-modes ()
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
    gerbil-mode
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

(defun j-lisp-hooks ()
  "Return hooks that are lispy."
  (mapcar (lambda (mode)
            (intern (concat (symbol-name mode) "-hook")))
          (j-lisp-modes)))

(defun j-standard-mode-hooks ()
  "Return hooks that are standard."
  (mapcar (lambda (mode)
            (intern (concat (symbol-name mode) "-hook")))
          (j-standard-modes)))

(defun j-indent-offset ()
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

(defun j-c-set-c-style (alist)
  "Update default c style with ALIST."
  (eval-after-load 'cc-vars (lambda () (push alist c-default-style))))

;; https://www.masteringemacs.org/article/searching-buffers-occur-mode
(defun j-get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (push buf buffer-mode-matches))))
    buffer-mode-matches))

(defun j-multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (j-get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun j-buffer-contains-string-p (string)
  "Check if current buffer contains STRING while preserving mark and match data."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun j-buffer-contains-regex-p (regex)
  "Check if current buffer contains REGEX while preserving mark and match data."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (re-search-forward regex nil t))))

(defun j-paste-column ()
  "Paste a column of text after another column of text."
  (interactive)
  (insert-rectangle
   (split-string
    (if (bound-and-true-p evil-mode)
        (evil-get-register ?0)
      (current-kill 0 t)) "[\r]?\n")))

(defun j-symbol-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

(defun j-search ()
  "Try to use `counsel-fzf' and fall back to `projectile-find-file'."
  (interactive)
  (if (and (executable-find "fzf")
           (fboundp 'counsel-FZF))
      (counsel-FZF)
    (projectile-find-file)))

(defun fill-region-and-comment ()
  "Fill region and comment."
  (interactive)
  (let* ((old-fill-column fill-column)
         (fill-column (- old-fill-column 2)))
    (call-interactively #'fill-region)
    (call-interactively #'comment-region)))

(defmacro yt-create-search-and-replace-function (project-name &rest directories)
  "Create a function to find and replace in DIRECTORIES. PROJECT-NAME will
determine the resulting function name."
  `(defun ,(intern (format "yt-%S-dired-do-find-regexp-and-replace" project-name)) (from to)
     "Replace matches of FROM with TO, in all marked files.
For any marked directory, matches in all of its files are replaced,
recursively.  However, files matching `grep-find-ignored-files'
and subdirectories matching `grep-find-ignored-directories' are skipped
in the marked directories.

REGEXP should use constructs supported by your local `grep' command.

This is a copy of `dired-do-find-regexp-and-replace' that uses
`yt-dired-do-find-regexp' instead of `dired-do-find-regexp' as well as hardcodes
the directories to be used in the regex replace."
     (interactive
      (let ((common
             (query-replace-read-args
              "Query replace regexp in marked files" t t)))
        (list (nth 0 common) (nth 1 common))))
     (with-current-buffer (yt-dired-do-find-regexp from '(,@directories))
       (xref-query-replace-in-results from to))))

(defun yt-dired-do-find-regexp (regexp directories)
  "Find all matches for REGEXP in all DIRECTORIES.
For any DIRECTORY, all of its files are searched recursively.
However, files matching `grep-find-ignored-files' and subdirectories
matching `grep-find-ignored-directories' are skipped in the marked
directories.

REGEXP should use constructs supported by your local `grep' command.

This function is a copy of `dired-do-find-regexp' without the marking."
  (interactive "sSearch marked files (regexp): ")
  (require 'grep)
  (defvar grep-find-ignored-files)
  (defvar grep-find-ignored-directories)
  (let* ((files directories)
         (ignores (nconc (mapcar
                          (lambda (s) (concat s "/"))
                          grep-find-ignored-directories)
                         grep-find-ignored-files))
         (xrefs (mapcan
                 (lambda (file)
                   (xref-collect-matches regexp "*" file
                                         (and (file-directory-p file)
                                              ignores)))
                 files)))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    (xref--show-xrefs xrefs nil t)))

(provide 'jn-functions)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-functions.el ends here

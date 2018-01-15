;;;; -*- lexical-binding: t; -*-
(require 'jn-dependencies)
(require 'jn-functions)

;; Dependencies

;; brew install node

;; npm install -g n

;; npm install -g react-native-cli
;; brew install yarn

;; https://github.com/jeffijoe/typesync
;; npm install -g typesync

(use-package js2-mode
  ;; References
  ;; https://truongtx.me/2014/04/20/emacs-javascript-completion-and-refactoring
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  :interpreter
  ("node" . js2-mode)
  :init
  (defun +setup-js2 ()
    "Set up `js2-mode'."
    (interactive)
    (setq mode-name "JS2")
    (+setup-web-indents))

  (defun +setup-web-indents ()
    "Set up indentation."
    (interactive)
    (let ((n (+indent-offset)))
      (setq-local js-indent-level n)
      (setq-local sgml-basic-offset n)
      (with-eval-after-load 'evil
        (setq-local evil-shift-width js-indent-level))))

  (add-hook 'js2-mode-hook #'+setup-js2)
  (add-hook 'js2-jsx-mode-hook #'+setup-web-indents)
  :config
  (setq js-chain-indent t)
  (setq js-enabled-frameworks '(javascript))
  (setq js2-highlight-level 3)
  (setq js2-idle-timer-delay 0.5)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package js2-refactor
  :ensure t
  :diminish js2-refactor-mode
  :commands
  (js2-refactor-mode)
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package xref-js2
  :ensure t
  :commands
  (xref-js2-xref-backend)
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  :config
  (define-key js2-mode-map (kbd "M-.") nil))

(use-package company-tern
  ;; npm install -g tern
  :ensure t
  :commands (company-tern)
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (when (executable-find "tern")
                (tern-mode t)
                (+company-push-backend #'company-tern t)))))

(use-package rjsx-mode
  :ensure t
  :mode
  ("\\.jsx?\\'" . rjsx-mode)
  :interpreter
  ("node" . rjsx-mode)
  :init
  (+make-terminal-dot-app-command "react-native run-ios")
  (+make-terminal-dot-app-command "react-native run-android")
  (+make-terminal-dot-app-command "yarn run dev:run-ios")
  (+make-terminal-dot-app-command "yarn run dev:run-android")
  :config
  ;; Inspired by http://blog.binchen.org/posts/indent-jsx-in-emacs.html
  (defun +js-jsx-indent-line-align-closing-bracket ()
    "Workaround sgml-mode and align closing bracket with opening bracket"
    (save-excursion
      (beginning-of-line)
      (when (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))

  (advice-add #'js-jsx-indent-line
              :after
              #'+js-jsx-indent-line-align-closing-bracket)

  (with-eval-after-load 'rjsx
    (define-key rjsx-mode-map "<" nil)))

(use-package indium
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'indium-interaction-mode)
  :commands (indium-run-node
             indium-switch-to-repl-buffer
             indium-interaction-mode))

(use-package js-import
  :ensure t
  :commands (js-import js-import-dev))

(use-package prettier-js
  ;; npm install -g prettier
  :ensure t
  :commands (prettier-js-mode prettier-js)
  :init
  (defvar +prettier-phoenix-args
    '("--single-quote" "true"
      "--print-width" "120"
      "--tab-width" "2"
      "--trailing-comma" "es5"
      "--parser" "typescript"
      "--write" "'src/**/*.{ts,tsx}' 'tests/**/*.{ts,tsx}'")
    "`prettier-js' args for Phoenix.")

  (defun +phoenix-project-p ()
    (condition-case nil
        (string-match-p
         "phoenix"
         (projectile-project-root))
      (error nil)))

  (add-hook
   'web-mode-hook
   (lambda ()
     (when-let* ((ext (or buffer-file-name
                          (file-name-extension buffer-file-name))))
       (when (and (or
                   (string-equal ext "jsx")
                   (string-equal ext "tsx"))
                  (+phoenix-project-p)))
       (setq-local prettier-js-args +prettier-phoenix-args))))

  (add-hook 'typescript-mode-hook
            (lambda ()
              (when (+phoenix-project-p)
                (setq-local prettier-js-args +prettier-phoenix-args)))))

(use-package typescript-mode
  ;; npm install -g typescript
  :ensure t
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.ts$\\'" . typescript-mode)
  :config
  (setq typescript-enabled-frameworks '(typescript)))

(use-package tide
  :ensure t
  :commands
  (tide-setup)
  :init
  (defun +setup-tide-mode ()
    (interactive)
    (when (locate-dominating-file default-directory "tsfmt.json")
      (add-hook 'before-save-hook #'tide-format-before-save nil t))
    ;; Disable linting for Typescript Definition files.
    (when (and (buffer-file-name)
               (string-match-p ".d.ts$" (buffer-file-name)))
      (flycheck-mode -1))
    (tide-setup)
    (tide-hl-identifier-mode +1)
    (+company-merge-backends))
  (add-hook 'typescript-mode-hook #'+setup-tide-mode)

  (add-hook 'js2-mode-hook
            (lambda ()
              (when (or
                     (locate-dominating-file default-directory "tsconfig.json")
                     (locate-dominating-file default-directory "jsconfig.json"))
                (+setup-tide-mode))))

  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up Tide mode if Typescript.
              (when (string-equal (file-name-extension buffer-file-name) "tsx")
                (setq-local web-mode-enable-auto-quoting nil)
                (when (fboundp 'yas-activate-extra-mode)
                  (yas-activate-extra-mode 'typescript-mode))
                (+setup-tide-mode))))
  :config
  ;; Set up Typescript linting with `web-mode'.
  ;; https://github.com/ananthakumaran/tide/pull/161
  (eval-after-load 'flycheck
    (lambda ()
      (flycheck-add-mode 'typescript-tslint 'web-mode))))

(use-package ts-comint
  ;; REPL for Typescript
  ;; npm install -g tsun
  :ensure t
  :commands (run-ts
             ts-send-last-sexp
             ts-send-last-sexp-and-go
             ts-send-buffer
             ts-send-buffer-and-go
             ts-load-file-and-go)
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
              (local-set-key (kbd "C-c b") 'ts-send-buffer)
              (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
              (local-set-key (kbd "C-c k") 'ts-load-file-and-go))))

(use-package mocha
  :ensure t
  :commands (mocha-test-project
             mocha-debug-project
             mocha-test-file
             mocha-debug-file
             mocha-test-at-point
             mocha-debug-at-point)
  :config
  (defun +mocha-run (&optional mocha-file test)
    "Run mocha in a compilation buffer.

If MOCHA-FILE is specified run just that file otherwise run
MOCHA-PROJECT-TEST-DIRECTORY.

IF TEST is specified run mocha with a grep for just that test."
    (let ((test-command-to-run (mocha-generate-command nil mocha-file test))
          (default-directory (mocha-find-project-root))
          (compilation-buffer-name-function (lambda (_) "" "*mocha tests*")))
      (compile test-command-to-run 'mocha-compilation-mode)))
  (advice-add 'mocha-run :override '+mocha-run)

  (defun +mocha-setup-automatic-test-runs ()
    "Set up `mocha-test-file' to run when this buffer is saved."
    (interactive)
    (if (member major-mode '(js2-mode
                             rjsx-mode
                             typescript-mode
                             web-mode))
        (add-hook 'after-save-hook #'mocha-test-file nil t)
      (message (concat
                (symbol-name major-mode)
                " is not suitable for mocha-test-file."))))
  (defalias 'mocha-setup-automatic-test-runs
    #'+mocha-setup-automatic-test-runs)

  ;; Clear up stray ansi escape sequences.
  (defvar +mocha-ansi-escape-sequences
    ;; https://emacs.stackexchange.com/questions/18457/stripping-stray-ansi-escape-sequences-from-eshell
    (rx (or
         "^[\\[[0-9]+[a-z]"
         "[1A"
         "[999D")))

  (defun +mocha-compilation-filter ()
    "Filter function for compilation output."
    (ansi-color-apply-on-region compilation-filter-start (point-max))
    (save-excursion
      (goto-char compilation-filter-start)
      (while (re-search-forward +mocha-ansi-escape-sequences nil t)
        (replace-match ""))))

  (advice-add 'mocha-compilation-filter :override '+mocha-compilation-filter)

  ;; https://github.com/scottaj/mocha.el/issues/3
  (defcustom mocha-jest-command "node_modules/jest/bin/jest.js --colors"
    "The path to the jest command to run."
    :type 'string
    :group 'mocha)

  (defun mocha-generate-command--jest-command (debug &optional filename testname)
    "Generate a command to run the test suite with jest.
If DEBUG is true, then make this a debug command.
If FILENAME is specified run just that file otherwise run
MOCHA-PROJECT-TEST-DIRECTORY.
IF TESTNAME is specified run jest with a pattern for just that test."
    (let ((target (if testname (concat " --testNamePattern \"" testname "\"") ""))
          (path (if (or filename mocha-project-test-directory)
                    (concat " --testPathPattern \""
                            (if filename filename mocha-project-test-directory)
                            "\"")
                  ""))
          (node-command
           (concat mocha-which-node
                   (if debug (concat " --debug=" mocha-debug-port) ""))))
      (concat node-command " "
              mocha-jest-command
              target
              path)))

  (advice-add 'mocha-generate-command
              :override 'mocha-generate-command--jest-command))

(use-package add-node-modules-path
  :ensure t
  :commands (add-node-modules-path)
  :init
  (mapcar
   (lambda (x)
     (add-hook x #'add-node-modules-path))
   '(js-mode-hook
     js2-mode-hook
     rjsx-mode-hook
     typescript-mode-hook
     web-mode-hook)))

(use-package mhtml
  :ensure nil
  :mode
  ("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . mhtml-mode)
  :init
  (add-hook 'mhtml-mode-hook
            (lambda ()
              (let ((n (+indent-offset)))
                (setq-local css-indent-offset n)
                (setq-local js-indent-level n)
                (setq-local sgml-basic-offset n)))))

(use-package web-mode
  :ensure t
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.blade\\.php\\'" . web-mode)
  ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jsp\\'" . web-mode)
  ("\\.eex\\'" . web-mode)
  ("\\.tsx\\'" . web-mode)
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up indentation.
              (let ((n (+indent-offset)))
                (setq-local web-mode-markup-indent-offset n)
                (setq-local web-mode-css-indent-offset n)
                (setq-local web-mode-code-indent-offset n)
                (with-eval-after-load 'evil
                  (setq-local evil-shift-width n)))))
  :config
  ;; Use `company-dabbrev-code' with `web-mode'.
  (when (boundp 'company-dabbrev-code-modes)
    (push 'web-mode company-dabbrev-code-modes))

  (with-eval-after-load 'evil
    (evil-define-key 'normal web-mode-map
      (kbd "C-d") 'evil-scroll-down))

  ;; https://github.com/bbatsov/prelude/blob/master/modules/prelude-web.el#L50
  (eval-after-load 'smartparens
    (lambda ()
      (setq web-mode-enable-auto-pairing nil)
      (sp-with-modes '(web-mode)
        (sp-local-pair "%" "%"
                       :unless '(sp-in-string-p)
                       :post-handlers '(((lambda (&rest _ignored)
                                           (just-one-space)
                                           (save-excursion (insert " ")))
                                         "SPC" "=" "#")))
        (sp-local-tag "%" "<% "  " %>")
        (sp-local-tag "=" "<%= " " %>")
        (sp-local-tag "#" "<%# " " %>")))))

(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

(defun +web-remove-autoloads ()
  "Remove autoloads."
  (setq auto-mode-alist (rassq-delete-all #'+typescript-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'+php-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'+web-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'+mhtml-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'+javascript-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'+javascript-jsx-mode auto-mode-alist)))

;;;###autoload
(defun +php-mode ()
  "Bootstrap `jn-php'."
  (+web-remove-autoloads)
  (php-mode))

;;;###autoload
(defun +web-mode ()
  "Bootstrap `jn-web'."
  (+web-remove-autoloads)
  (web-mode))

;;;###autoload
(defun +mhtml-mode ()
  "Bootstrap `jn-web'."
  (+web-remove-autoloads)
  (mhtml-mode))

;;;###autoload
(defun +javascript-mode ()
  "Bootstrap `jn-javascript'."
  (+web-remove-autoloads)
  (js2-mode))

;;;###autoload
(defun +javascript-jsx-mode ()
  "Bootstrap `jn-javascript'."
  (+web-remove-autoloads)
  (rjsx-mode))

;;;###autoload
(defun +typescript-mode ()
  "Bootstrap `jn-javascript'."
  (+web-remove-autoloads)
  (typescript-mode))

;; Leaving Javascript indentation code outside of package block so other
;; packages can use the same indentation settings.

;; https://emacs.stackexchange.com/questions/29973/stop-javascript-mode-from-lining-up-function-parameters-after-newline/29975#29975
(defun +js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--chained-expression-p))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (progn ; nothing following the opening paren/bracket
               (skip-syntax-backward " ")
               (when (eq (char-before) ?\)) (backward-list))
               (back-to-indentation)
               (js--maybe-goto-declaration-keyword-end parse-status)
               (let* ((in-switch-p (unless same-indent-p
                                     (looking-at "\\_<switch\\_>")))
                      (same-indent-p (or same-indent-p
                                         (and switch-keyword-p
                                              in-switch-p)))
                      (indent
                       (cond (same-indent-p
                              (current-column))
                             (continued-expr-p
                              (+ (current-column) (* 2 js-indent-level)
                                 js-expr-indent-offset))
                             (t
                              (+ (current-column) js-indent-level
                                 (pcase (char-after (nth 1 parse-status))
                                   (?\( js-paren-indent-offset)
                                   (?\[ js-square-indent-offset)
                                   (?\{ js-curly-indent-offset)))))))
                 (if in-switch-p
                     (+ indent js-switch-indent-offset)
                   indent)))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t (prog-first-column)))))

(defun +25-js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (progn ; nothing following the opening paren/bracket
               (skip-syntax-backward " ")
               (when (eq (char-before) ?\)) (backward-list))
               (back-to-indentation)
               (js--maybe-goto-declaration-keyword-end parse-status)
               (let* ((in-switch-p (unless same-indent-p
                                     (looking-at "\\_<switch\\_>")))
                      (same-indent-p (or same-indent-p
                                         (and switch-keyword-p
                                              in-switch-p)))
                      (indent
                       (cond (same-indent-p
                              (current-column))
                             (continued-expr-p
                              (+ (current-column) (* 2 js-indent-level)
                                 js-expr-indent-offset))
                             (t
                              (+ (current-column) js-indent-level
                                 (pcase (char-after (nth 1 parse-status))
                                   (?\( js-paren-indent-offset)
                                   (?\[ js-square-indent-offset)
                                   (?\{ js-curly-indent-offset)))))))
                 (if in-switch-p
                     (+ indent js-switch-indent-offset)
                   indent)))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))

;; When Emacs 26 is released, parts of this can be removed.
(cond
 ((boundp 'js-indent-align-list-continuation)
  (setq js-indent-align-list-continuation nil))
 ((>= emacs-major-version 26)
  (advice-add 'js--proper-indentation :override '+js--proper-indentation))
 (:else
  (advice-add 'js--proper-indentation :override '+25-js--proper-indentation)))

(provide 'jn-web)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-web.el ends here

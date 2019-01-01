;;;; -*- lexical-binding: t; -*-
(require 'jn-dependencies)
(require 'jn-functions)

;; Dependencies

;; brew install node
;; npm install -g n
;; npm install -g react-native-cli
;; brew install yarn

(use-package js2-mode
  ;; References
  ;; https://truongtx.me/2014/04/20/emacs-javascript-completion-and-refactoring
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  :interpreter
  ("node" . js2-mode)
  :init
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27503
  ;; https://emacs.stackexchange.com/questions/29780/changing-how-argument-lists-are-indented-in-javascript/29790
  (setq js-indent-align-list-continuation nil)

  (defun j|js2-setup ()
    "Set up `js2-mode'."
    (setq mode-name "JS2")
    (j|web-set-indent-level))

  (defun j|web-set-indent-level ()
    "Set up indentation."
    (interactive)
    (let ((n (j|indent-offset)))
      (setq-local js-indent-level n)
      (setq-local sgml-basic-offset n)))

  (add-hook 'js2-mode-hook #'j|js2-setup)
  (add-hook 'js2-jsx-mode-hook #'j|web-set-indent-level)
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

(use-package rjsx-mode
  :ensure t
  :mode
  ("\\.jsx?\\'" . rjsx-mode)
  :interpreter
  ("node" . rjsx-mode)
  :init
  (j|make-terminal-dot-app-command "react-native run-ios")
  (j|make-terminal-dot-app-command "react-native run-android")
  (j|make-terminal-dot-app-command "yarn run dev:run-ios")
  (j|make-terminal-dot-app-command "yarn run dev:run-android")
  :config
  ;; Inspired by http://blog.binchen.org/posts/indent-jsx-in-emacs.html
  (defun j|js-jsx-indent-line-align-closing-bracket ()
    "Workaround sgml-mode and align closing bracket with opening bracket"
    (save-excursion
      (beginning-of-line)
      (when (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))

  (advice-add #'js-jsx-indent-line
              :after
              #'j|js-jsx-indent-line-align-closing-bracket)

  (with-eval-after-load 'rjsx
    (define-key rjsx-mode-map "<" nil)))

(use-package indium
  ;; :load-path "~/Code/Indium"
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
  (defvar j|prettier-phoenix-args
    '("--single-quote" "true"
      "--print-width" "120"
      "--tab-width" "2"
      "--trailing-comma" "es5"
      "--parser" "typescript"
      "--write" "'src/**/*.{ts,tsx}' 'tests/**/*.{ts,tsx}'")
    "`prettier-js' args for Phoenix.")

  (defun j|phoenix-project-p ()
    (condition-case nil
        (string-match-p
         "phoenix"
         (projectile-project-root))
      (error nil)))

  (add-hook
   'web-mode-hook
   (lambda ()
     (when buffer-file-name
       (let ((ext (file-name-extension buffer-file-name)))
         (when (and (or
                     (string-equal ext "jsx")
                     (string-equal ext "tsx"))
                    (j|phoenix-project-p))
           (setq-local prettier-js-args j|prettier-phoenix-args))))))

  (add-hook 'typescript-mode-hook
            (lambda ()
              (when (j|phoenix-project-p)
                (setq-local prettier-js-args j|prettier-phoenix-args)))))

(use-package typescript-mode
  ;; npm install -g typescript
  :ensure t
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.ts$\\'" . typescript-mode)
  :config
  (setq typescript-indent-level 2)
  (setq typescript-enabled-frameworks '(typescript)))

(use-package tide
  :ensure t
  :commands
  (tide-setup)
  :init
  (defun j|tide-setup ()
    (interactive)
    (when (locate-dominating-file default-directory "tsfmt.json")
      (add-hook 'before-save-hook #'tide-format-before-save nil t))
    ;; Disable linting for Typescript Definition files.
    (when (and (buffer-file-name)
               (string-match-p ".d.ts$" (buffer-file-name)))
      (when (fboundp 'flycheck-mode)
        (flycheck-mode -1)))
    (tide-setup)
    (tide-hl-identifier-mode +1)
    (j|company-merge-backends))
  (add-hook 'typescript-mode-hook #'j|tide-setup)

  (add-hook 'js2-mode-hook
            (lambda ()
              (when (or
                     (locate-dominating-file default-directory "tsconfig.json")
                     (locate-dominating-file default-directory "jsconfig.json"))
                (j|tide-setup))))

  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up Tide mode if Typescript.
              (when (string-equal (file-name-extension buffer-file-name) "tsx")
                (setq-local web-mode-enable-auto-quoting nil)
                (when (fboundp 'yas-activate-extra-mode)
                  (yas-activate-extra-mode 'typescript-mode))
                (j|tide-setup))))
  :config
  ;; Set up Typescript linting with `web-mode'.
  ;; https://github.com/ananthakumaran/tide/pull/161
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'typescript-tslint 'web-mode)))

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
  ;; Clear up stray ansi escape sequences.
  (defvar j|mocha-ansi-escape-sequences
    ;; https://emacs.stackexchange.com/questions/18457/stripping-stray-ansi-escape-sequences-from-eshell
    (rx (or
         "^[\\[[0-9]+[a-z]"
         "[1A"
         "[999D")))

  (defun j|mocha-compilation-filter ()
    "Filter function for compilation output."
    (ansi-color-apply-on-region compilation-filter-start (point-max))
    (save-excursion
      (goto-char compilation-filter-start)
      (while (re-search-forward j|mocha-ansi-escape-sequences nil t)
        (replace-match ""))))

  (advice-add 'mocha-compilation-filter :override 'j|mocha-compilation-filter)

  ;; https://github.com/scottaj/mocha.el/issues/3
  (setq mocha-jest-command "node_modules/jest/bin/jest.js --colors")

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
              ;; FIXME: Make this optional when working with React Native.
              (when :react " --env=jsdom")
              target
              path)))

  (advice-add 'mocha-generate-command
              :override 'mocha-generate-command--jest-command)

  (defun mocha-projectile-root (f &rest args)
    "Return project root for use with `mocha'."
    (if (projectile-project-p)
        (projectile-project-root)
      (apply f args)))

  (advice-add 'mocha-find-project-root :around 'mocha-projectile-root))

(use-package add-node-modules-path
  :ensure t
  :commands (add-node-modules-path)
  :init
  (defun j|add-node-modules-path ()
    "`mhtml-mode' triggers these mode-hooks earlier on before
 `buffer-file-name' is populated. When `mhtml-mode-hook'
finally runs, `buffer-file-name' will be populated."
    (when buffer-file-name
      (add-node-modules-path)))
  (mapcar
   (lambda (x)
     (add-hook x #'j|add-node-modules-path))
   '(js-mode-hook
     js2-mode-hook
     mhtml-mode-hook
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
              (let ((n (j|indent-offset)))
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
              ;; https://github.com/fxbois/web-mode/issues/964
              ;; https://github.com/fxbois/web-mode/issues/966
              ;; https://github.com/fxbois/web-mode/pull/967/files
              ;; Run `prog-mode-hook' since `web-mode' doesn't derive from it.
              (run-hooks 'prog-mode-hook)

              ;; Set up indentation.
              (let ((n (j|indent-offset)))
                (setq-local web-mode-markup-indent-offset n)
                (setq-local web-mode-css-indent-offset n)
                (setq-local web-mode-code-indent-offset n)
                (with-eval-after-load 'evil
                  (setq-local evil-shift-width n)))))
  :config
  (with-eval-after-load 'company
    ;; Use `company-dabbrev-code' with `web-mode'.
    (when (boundp 'company-dabbrev-code-modes)
      (push 'web-mode company-dabbrev-code-modes)))

  (with-eval-after-load 'hideshow
    ;; Add \(\) to folding regex.
    (add-to-list 'hs-special-modes-alist
                 '(web-mode "(\\|{" ")\\|}" "/[*/]" web-mode-forward-sexp nil)))

  (with-eval-after-load 'smartparens
    ;; https://github.com/bbatsov/prelude/blob/master/modules/prelude-web.el#L50
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
      (sp-local-tag "#" "<%# " " %>"))))

(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

(use-package flycheck-jest
  :load-path "~/.emacs.d/fork/flycheck-jest"
  :ensure nil
  :config
  (flycheck-jest-setup))

(defun j|web-remove-autoloads ()
  "Remove autoloads."
  (setq auto-mode-alist (rassq-delete-all #'j|typescript-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j|php-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j|web-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j|mhtml-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j|javascript-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j|javascript-jsx-mode auto-mode-alist)))

;;;###autoload
(defun j|php-mode ()
  "Bootstrap `jn-php'."
  (j|web-remove-autoloads)
  (php-mode))

;;;###autoload
(defun j|web-mode ()
  "Bootstrap `jn-web'."
  (j|web-remove-autoloads)
  (web-mode))

;;;###autoload
(defun j|mhtml-mode ()
  "Bootstrap `jn-web'."
  (j|web-remove-autoloads)
  (mhtml-mode))

;;;###autoload
(defun j|javascript-mode ()
  "Bootstrap `jn-javascript'."
  (j|web-remove-autoloads)
  (js2-mode))

;;;###autoload
(defun j|javascript-jsx-mode ()
  "Bootstrap `jn-javascript'."
  (j|web-remove-autoloads)
  (rjsx-mode))

;;;###autoload
(defun j|typescript-mode ()
  "Bootstrap `jn-javascript'."
  (j|web-remove-autoloads)
  (typescript-mode))

(provide 'jn-web)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-web.el ends here

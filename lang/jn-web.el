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

  (defun j-js2-setup ()
    "Set up `js2-mode'."
    (setq mode-name "JS2")
    (j-web-set-indent-level))

  (defun j-web-set-indent-level ()
    "Set up indentation."
    (interactive)
    (let ((n (j-indent-offset)))
      (setq-local js-indent-level n)
      (setq-local sgml-basic-offset n)))

  (add-hook 'js2-mode-hook #'j-js2-setup)
  (add-hook 'js2-jsx-mode-hook #'j-web-set-indent-level)
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
              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package rjsx-mode
  :ensure t
  :mode
  ("\\.jsx?\\'" . rjsx-mode)
  :interpreter
  ("node" . rjsx-mode)
  :init
  (j-make-terminal-dot-app-command "react-native run-ios")
  (j-make-terminal-dot-app-command "react-native run-android")
  (j-make-terminal-dot-app-command "yarn run dev:run-ios")
  (j-make-terminal-dot-app-command "yarn run dev:run-android")
  :config
  ;; Inspired by http://blog.binchen.org/posts/indent-jsx-in-emacs.html
  (defun j-js-jsx-indent-line-align-closing-bracket ()
    "Workaround sgml-mode and align closing bracket with opening bracket"
    (save-excursion
      (beginning-of-line)
      (when (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))

  (advice-add #'js-jsx-indent-line
              :after
              #'j-js-jsx-indent-line-align-closing-bracket)

  (with-eval-after-load 'rjsx
    (define-key rjsx-mode-map "<" nil)))

(use-package js-import
  :ensure t
  :commands (js-import js-import-dev))

(use-package prettier-js
  ;; npm install -g prettier
  :ensure t
  :commands (prettier-js-mode prettier-js))

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
  (defun j-tide-setup ()
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
    (j-company-merge-backends))
  (add-hook 'typescript-mode-hook #'j-tide-setup)

  (add-hook 'js2-mode-hook
            (lambda ()
              (when (or
                     (locate-dominating-file default-directory "tsconfig.json")
                     (locate-dominating-file default-directory "jsconfig.json"))
                (j-tide-setup))))

  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up Tide mode if Typescript.
              (when (string-equal (file-name-extension buffer-file-name) "tsx")
                (setq-local web-mode-enable-auto-quoting nil)
                (when (fboundp 'yas-activate-extra-mode)
                  (yas-activate-extra-mode 'typescript-mode))
                (j-tide-setup))))
  :config
  ;; Set up Typescript linting with `web-mode'.
  ;; https://github.com/ananthakumaran/tide/pull/161
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'typescript-tslint 'web-mode)))

(use-package add-node-modules-path
  :ensure t
  :commands (add-node-modules-path)
  :init
  (defun j-add-node-modules-path ()
    "`mhtml-mode' triggers these mode-hooks earlier on before
 `buffer-file-name' is populated. When `mhtml-mode-hook'
finally runs, `buffer-file-name' will be populated."
    (when buffer-file-name
      (add-node-modules-path)))
  (mapcar
   (lambda (x)
     (add-hook x #'j-add-node-modules-path))
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
              (let ((n (j-indent-offset)))
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
              (let ((n (j-indent-offset)))
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

(defun j-web-remove-autoloads ()
  "Remove autoloads."
  (setq auto-mode-alist (rassq-delete-all #'j-typescript-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j-php-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j-web-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j-mhtml-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j-javascript-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j-javascript-jsx-mode auto-mode-alist)))

;;;###autoload
(defun j-php-mode ()
  "Bootstrap `jn-php'."
  (j-web-remove-autoloads)
  (php-mode))

;;;###autoload
(defun j-web-mode ()
  "Bootstrap `jn-web'."
  (j-web-remove-autoloads)
  (web-mode))

;;;###autoload
(defun j-mhtml-mode ()
  "Bootstrap `jn-web'."
  (j-web-remove-autoloads)
  (mhtml-mode))

;;;###autoload
(defun j-javascript-mode ()
  "Bootstrap `jn-javascript'."
  (j-web-remove-autoloads)
  (js2-mode))

;;;###autoload
(defun j-javascript-jsx-mode ()
  "Bootstrap `jn-javascript'."
  (j-web-remove-autoloads)
  (rjsx-mode))

;;;###autoload
(defun j-typescript-mode ()
  "Bootstrap `jn-javascript'."
  (j-web-remove-autoloads)
  (typescript-mode))

(provide 'jn-web)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-web.el ends here

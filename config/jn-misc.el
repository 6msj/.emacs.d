;;;; -*- lexical-binding: t; -*-
(require 'jn-functions)

(use-package multi-term
  ;; Terminal
  :ensure t
  :commands (multi-term
             multi-term-next
             multi-term-prev
             multi-term-dedicated-open
             multi-term-dedicated-close
             multi-term-dedicated-toggle
             multi-term-dedicated-select)
  :config
  (setq multi-term-dedicated-window-height 18)
  (setq multi-term-dedicated-select-after-open-p t)
  (setq multi-term-dedicated-close-back-to-open-buffer-p t)
  (add-to-list 'term-unbind-key-list "C-q") ; C-q binds to raw input by default
  (setq multi-term-program "/bin/zsh"))

(use-package eshell
  :ensure nil
  :commands (eshell)
  :config
  ;; https://www.emacswiki.org/emacs/EshellAlias
  (defun eshell/emacs (file)
    "Open file in emacs."
    (find-file file))
  (defun eshell/e (file)
    "Open file in emacs."
    (eshell/emacs file)))

(use-package term
  :ensure nil
  :commands (term ansi-term)
  :init
  (add-hook 'term-mode-hook
            (lambda ()
              (when (bound-and-true-p company-mode)
                (company-mode -1))))

  (defun ansi-term-handle-close ()
    "Close current term buffer when `exit' from term buffer."
    (when (ignore-errors (get-buffer-process (current-buffer)))
      (set-process-sentinel (get-buffer-process (current-buffer))
                            (lambda (proc change)
                              (when (string-match "\\(finished\\|exited\\)"
                                                  change)
                                (kill-buffer (process-buffer proc))
                                (when (> (count-windows) 1)
                                  (delete-window)))))))
  (add-hook 'term-mode-hook 'ansi-term-handle-close))

(use-package ix
  :ensure t
  :commands (ix ix-browse ix-delete)
  :config
  (setq ix-user "jnjames")
  (setq ix-token "jnjames"))

(use-package prodigy
  :ensure t
  :defer 10)

(use-package pass
  :ensure t
  :commands (pass)
  :config
  (use-package password-store
    :ensure t
    :config
    (defun j|password-store-dir (f &rest args)
      "Return password store directory looking for store in Dropbox."
      (if (eq system-type 'windows-nt)
          (concat
           (string-remove-suffix "\\AppData\\Roaming"
                                 (getenv "HOME"))
           "\\Dropbox\\.password-store")
        (if (file-exists-p "~/Dropbox/.password-store")
            "~/Dropbox/.password-store"
          (apply f args))))

    (advice-add 'password-store-dir :around 'j|password-store-dir)))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  ;; Yaml
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(use-package json-mode
  ;; Json
  :ensure t
  :mode
  ("apple-app-site-association" . json-mode)
  ("\\.json\\'" . json-mode)
  ("\\.xctool.args\\'" . json-mode)
  :init
  (add-hook 'json-mode-hook
            (lambda ()
              (let ((n (j|indent-offset)))
                (setq-local js-indent-level n)))))

(use-package powershell
  :ensure t
  :mode ("\\.ps1\\'" . powershell-mode))

(use-package vimrc-mode
  ;; Vimscript
  :ensure t
  :mode ("\\.vimrc\\'" . vimrc-mode))

(use-package restclient
  ;; HTTP
  :ensure t
  :commands (restclient-mode))

;; Shell
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(use-package nxml-mode
  :ensure nil
  :mode
  ("\\.xml\\'" . nxml-mode)
  ("\\.axml\\'" . nxml-mode)
  :init
  (add-hook 'nxml-mode-hook '(lambda ()
                               (setq tab-width 4)))
  :config
  (setq nxml-child-indent 4))

(use-package make-mode
  :ensure nil
  :mode
  ("Imakefile\\'" . makefile-imake-mode)
  ("Makeppfile\\(?:\\.mk\\)?\\'" . makefile-makepp-mode)
  ("\\.makepp\\'" . makefile-makepp-mode)
  ("\\.mk\\'" . makefile-bsdmake-mode)
  ("\\.make\\'" . makefile-bsdmake-mode)
  ("GNUmakefile\\'" . makefile-gmake-mode)
  ("[Mm]akefile\\'" . makefile-bsdmake-mode)
  ("\\.am\\'" . makefile-automake-mode))

(use-package cmake-mode
  :ensure t
  :mode
  ("\\.cmake\\'" . cmake-mode)
  ("CMakeLists\\.txt\\'" . cmake-mode)
  :config
  (setq cmake-tab-width 2))

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package deadgrep
  :ensure t
  :commands (deadgrep))

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(provide 'jn-misc)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-misc.el ends here

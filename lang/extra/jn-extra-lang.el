;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

;; Yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

;; Json
(use-package json-mode
  :ensure t
  :mode
  ("apple-app-site-association" . json-mode)
  ("\\.json\\'" . json-mode)
  ("\\.xctool.args\\'" . json-mode))

(use-package powershell
  :ensure t
  :mode ("\\.ps1\\'" . powershell-mode))

;; Vimscript
(use-package vimrc-mode
  :ensure t
  :mode ("\\.vimrc\\'" . vimrc-mode))

;; HTTP
(use-package restclient
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
  ("\\.am\\'" . makefile-automake-mode)
  :config
  (+make-async-shell-cmd
   ("make all" "make test" "make install")
   'makefile-mode)

  (defhydra hydra-makefile-mode (:color teal :hint nil)
    "C Mode"
    ("u" +makefile-mode-make-all "Make")
    ("t" +makefile-mode-make-test "Make Test")
    ("i" +makefile-mode-make-install "Make Install"))

  (when (fboundp '+add-mode-command)
    (+add-mode-command #'hydra-makefile-mode/body '(makefile-mode))))

(use-package cmake-mode
  :ensure t
  :mode
  ("\\.cmake\\'" . cmake-mode)
  ("CMakeLists\\.txt\\'" . cmake-mode)
  :config
  (setq cmake-tab-width 2))

(provide 'jn-extra-lang)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-extra-lang.el ends here

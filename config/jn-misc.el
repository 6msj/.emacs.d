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
    (defun j-password-store-dir (f &rest args)
      "Return password store directory looking for store in Dropbox."
      (if (eq system-type 'windows-nt)
          (concat
           (string-remove-suffix "\\AppData\\Roaming"
                                 (getenv "HOME"))
           "\\Dropbox\\.password-store")
        (if (file-exists-p "~/Dropbox/.password-store")
            "~/Dropbox/.password-store"
          (apply f args))))

    (advice-add 'password-store-dir :around 'j-password-store-dir)))

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
              (let ((n (j-indent-offset)))
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
  :commands (deadgrep)
  :config
  (defun j-deadgrep--project-root ()
    "Guess the project root of the given FILE-PATH."
    (if (derived-mode-p 'dired-mode)
        (dired-current-directory)
      (deadgrep--project-root)))

  (setq deadgrep-project-root-function 'j-deadgrep--project-root))

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(use-package shackle
  :ensure t
  :defer 3
  :init
  :config
  (setq shackle-rules
        '(
          ;; `alchemist'
          (alchemist-mix-mode :align below :size 0.3 :select nil)
          (alchemist-iex-mode :align below :size 0.3 :select t)
          (alchemist-test-report-mode :align below :size 0.3 :select nil)
          ("*alchemist help*" :regexp t :align below :size 0.2 :select t)

          ;; `anaconda-mode'
          ("*Anaconda*" :regexp t :align below :size 0.3 :select t)
          (anaconda-mode-view-mode :align below :size 0.3 :select t)
          ("*anaconda-mode*" :regexp t :align below :size 0.3 :select nil)

          (compilation-mode :align below :size 0.3 :select nil)

          ;; `csearch'
          ("*csearch*" :regexp t :align below :size 0.4 :select nil)

          ;; `dired'
          ("*Dired log*" :regexp t :align below :size 0.3 :select nil)

          ;; `eglot'
          ("*eglot help*" :regexp t :align below :size 0.3 :select t)

          ;; `evil'
          ("*evil-registers*" :regexp t :align below :size 0.3 :select nil)

          ;; `flycheck'
          (flycheck-error-list-mode :align below :size 0.3 :select t)
          ("*Flycheck checkers*" :regexp t :align below :size 0.3 :select nil)

          ;; `geiser'
          (geiser-doc-mode :align below :size 0.3 :select t)
          (geiser-debug-mode :align below :size 0.3 :select nil)

          ;; `go-guru'
          ("*go-guru-output*" :regexp t :align below :size 0.3 :select t)

          ;; `google-translate'
          ("*Google Translate*" :regexp t :align below :size 0.3 :select t)

          ;; `go-mode'
          (godoc-mode :align below :size 0.3 :select t)
          ("*Gofmt Errors" :regexp t :align below :size 0.2 :select nil)

          ;; `help'
          ("*Help*" :regexp t :align below :size 0.3 :select t)

          ;; `indium'
          (indium-debugger-locals-mode :align below :size 0.4 :select nil)

          ;; `magit'
          (magit-process-mode :align below :size 0.35 :select nil)

          ;; `mocha'
          (mocha-compilation-mode :align below :size 0.3 :select nil)

          ;; `p4'
          (p4-basic-mode :align below :size 0.35 :select nil)

          ;; `restclient'
          ("*HTTP Response*" :regexp t :align below :size 0.4 :select nil)

          ;; `rtags'
          (rtags-mode :align below :size 0.4 :select nil)

          ;; `simple'
          ("Async Shell Command*" :regexp t :align below :size 0.2 :select nil)
          ("*Shell Command Output*" :regexp t :align below :size 0.2 :select nil)

          ;; `tide'
          (tide-references-mode :regexp t :align below :size 0.3 :selct nil)

          ;; `undo-tree'
          (undo-tree-visualizer-mode :align below :size 0.4 :select t)

          ;; `vc-mode'
          ("*vc-git:*" :regexp t :align below :size .3 :select nil)
          ("*vc-hg:*" :regexp t :align below :size .3 :select nil)
          ("*vc-diff*" :regexp t :align below :size .4 :other t)

          ("*hgcmd output:" :regexp t :align below :size .2 :select nil)

          ;; `xref'
          (xref--xref-buffer-mode :align below :size 0.4 :select nil)

          ;; Misc
          ("*+shell" :regexp t :align below :size 0.3 :select t)
          ("*make*" :regexp t :align below :size 0.3 :select nil)))
  (shackle-mode))

(use-package google-translate
  :ensure t
  :commands (google-translate-at-point
             google-translate-buffer
             google-translate-query-translate
             google-translate-line-or-region)
  :config
  (require 'google-translate-default-ui)
  (setq google-translate-show-phonetic t)
  (setq google-translate-backend-method 'curl)
  (setq google-translate-default-source-language "ja")
  (setq google-translate-default-target-language "en")

  (defun %google-translate-line-or-region (override-p reverse-p)
    (let* ((langs (google-translate-read-args override-p reverse-p))
           (source-language (car langs))
           (target-language (cadr langs)))
      (google-translate-translate
       source-language target-language
       (if (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end))
         (string-trim (thing-at-point 'line :no-properties))))))

  (defun google-translate-line-or-region (&optional override-p)
    "Translate the line or the words in the active region.

For the meaning of OVERRIDE-P, see `google-translate-query-translate'."
    (interactive "P")
    (%google-translate-line-or-region override-p nil))

  ;; https://github.com/atykhonov/google-translate/issues/98
  (defun google-translate-json-suggestion (json)
    "Retrieve from JSON (which returns by the
`google-translate-request' function) suggestion. This function
does matter when translating misspelled word. So instead of
translation it is possible to get suggestion."
    (let ((info (aref json 7)))
      (if (and info (> (length info) 0))
          (aref info 1)
        nil)))

  ;; https://github.com/atykhonov/google-translate/pull/95
  (defun google-translate--request (source-language
                                    target-language
                                    text
                                    &optional for-test-purposes)
    "Send to the Google Translate http request which consigned to
translate TEXT from SOURCE-LANGUAGE to TARGET-LANGUAGE."
    (google-translate--http-response-body
     (google-translate--format-request-url
      `(("client" . "webapp")
        ("ie"     . "UTF-8")
        ("oe"     . "UTF-8")
        ("sl"     . ,source-language)
        ("tl"     . ,target-language)
        ("q"      . ,text)
        ("dt"     . "bd")
        ("dt"     . "ex")
        ("dt"     . "ld")
        ("dt"     . "md")
        ("dt"     . "qc")
        ("dt"     . "rw")
        ("dt"     . "rm")
        ("dt"     . "ss")
        ("dt"     . "t")
        ("dt"     . "at")
        ("pc"     . "1")
        ("otf"    . "1")
        ("srcrom" . "1")
        ("ssel"   . "0")
        ("tsel"   . "0")
        ("tk"     . ,(google-translate--gen-tk text))))
     for-test-purposes))

  ;; JP
  (defun google-translate-word-at-point-into-clipboard ()
    "Translate the word at point and send it to the clipboard."
    (interactive)
    (let* ((langs (google-translate-read-args nil nil))
           (source-language (car langs))
           (target-language (cadr langs)))
      (google-translate-translate-to-clipboard
       source-language target-language
       (symbol-name (symbol-at-point)))))

  (defun google-translate-translate-to-clipboard (source-language target-language text &optional output-destination)
    "Translate TEXT from SOURCE-LANGUAGE to TARGET-LANGUAGE.

Add translation to clipboard."
    (let* ((json (google-translate-request source-language
                                           target-language
                                           text)))
      (if (null json)
          (message "Nothing to translate.")
        (let ((result (google-translate-json-translation json)))
          (kill-new result)
          result))))

  (defun new-authentic-translate-vocabulary ()
    "Translate vocabulary in new authentic."
    (interactive)
    (let ((result (google-translate-word-at-point-into-clipboard)))
      (end-of-thing 'symbol)
      (delete-char 1)
      (insert ",")
      (insert result)
      (insert ",")
      (end-of-line)
      (insert ",")
      (beginning-of-line)
      (forward-line 1)))

  (defun new-authentic-translate-list-into-csv-format ()
    "Translate new authentic list into csv format."
    (interactive)
    (while (not (eobp))
      (new-authentic-translate-vocabulary)
      (redisplay)
      ;; Try to avoid getting throttled by google.
      (sit-for .1))))

(use-package csv-mode
  :ensure t
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :config
  (setq csv-separators '("," "    ")))

(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :commands (org-agenda org-capture org-store-link)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :init
  (defun j-org-maybe-setup-writing ()
    "Set up writing conditionally."
    (when (string-equal (buffer-name) "M.org.gpg")
      (turn-on-auto-fill)
      (set-fill-column 80)
      (define-key evil-normal-state-local-map
        (kbd "<return>") #'org-tree-to-indirect-buffer)
      (define-key evil-normal-state-local-map
        (kbd "RET") #'org-tree-to-indirect-buffer)
      (define-key evil-normal-state-local-map
        [return] #'org-tree-to-indirect-buffer)))

  (defun j-org-customize-ui ()
    "Customize various Org Mode UI Elements"
    (set-face-attribute 'org-document-title nil :weight 'bold :height 1.4)
    (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.3 :weight 'bold)
    (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.2 :weight 'bold)
    (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.1)
    (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :height 1.0))

  (add-hook 'org-mode-hook
            (lambda ()
              (j-org-maybe-setup-writing)
              (j-org-customize-ui)))
  :config
  (require 'ox-odt) ;; Open Document Format

  (with-eval-after-load 'evil
    (evil-define-key 'normal org-mode-map
      (kbd "M-.") 'org-open-at-point
      [tab] #'j-org-indent-block-automatically-or-cycle
      (kbd "TAB") #'j-org-indent-block-automatically-or-cycle))

  (when MAC-P
    (setq org-directory "~/Dropbox/Notes")
    (setq org-agenda-files '("~/Dropbox/Notes")))
  (when (eq system-type 'windows-nt)
    (setq org-directory "C:/Users/james/Dropbox/Notes")
    (setq org-agenda-files '("C:/Users/james/Dropbox/Notes")))

  (setq org-capture-templates
        '((;; Standard Todo
           "t" "Todo" entry
           (file+headline "mine.org" "Tasks")
           "* TODO %u %a %?\n")
          (;; Add as Productivity task.
           "p" "Productivity" entry
           (file+headline "mine.org" "Productivity")
           "* TODO %u %?\n")))

  (when YT-P
    (if MAC-P
        (progn
          (setq org-directory "~/Google Drive File Stream/My Drive/Notes")
          (setq org-agenda-files '("~/Google Drive File Stream/My Drive/Notes")))
      (setq org-directory "~/DriveFileStream/My Drive/Notes")
      (setq org-agenda-files "~/DriveFileStream/My Drive/Notes"))

    (setq org-capture-templates
          '((;; Standard Todo
             "t" "Todo" entry
             (file+headline "Tasks.org" "Tasks")
             "* TODO %u %a %?\n")
            (;; Handle this message in the next two days.
             "h" "High Priority" entry
             (file+headline "Tasks.org" "High Priority")
             "* TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
            (;; Blocked task
             "b" "Blocked"
             entry (file+headline "Tasks.org" "Blocked")
             "* BLOCKED %u %a %?\n"))))

  (defun j-org-indent-block-automatically-or-cycle ()
    "Indent source code in source blocks."
    (interactive)
    (if (org-in-src-block-p)
        (progn
          (org-edit-special)
          (indent-region (point-min) (point-max))
          (org-edit-src-exit))
      (call-interactively #'org-cycle)))

  (with-eval-after-load 'evil-org
    (evil-define-key 'normal evil-org-mode-map
      (kbd "<tab>") 'j-org-indent-block-automatically-or-cycle))

  (setq org-export-backends '(ascii html icalendar latex md))
  (setq org-src-fontify-natively t)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-goto-interface 'outline-path-completion
        org-goto-max-level 10))

(use-package groovy-mode
  :mode
  ("\\.gradle\\'" . groovy-mode)
  ("\\.groovy\\'" . groovy-mode))

(use-package scala-mode
  :ensure t
  :mode
  ("\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'" . scala-mode))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'completion-ignored-extensions ".hi"))

(provide 'jn-misc)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-misc.el ends here

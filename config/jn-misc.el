;;;; -*- lexical-binding: t; -*-
(require 'jn-functions)

(use-package super-save
  :ensure t
  :defer 5
  :diminish super-save-mode
  :config
  (defun +super-save-command (f &rest args)
    "`super-save-command' advice to handle silently saving and disabling modes."
    (unless (memq major-mode '(emacs-lisp-mode
                               snippet-mode))
      (let ((save-silently t))
        (apply f args))))

  (advice-add 'super-save-command :around '+super-save-command)

  (setq super-save-idle-duration 1.5)
  (setq auto-save-default nil)
  (setq super-save-auto-save-when-idle t)
  (super-save-mode +1))

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

(use-package ace-jump-mode
  ;; Movement
  :ensure t
  :commands
  (ace-jump-mode))

(use-package shackle
  :ensure t
  :commands (shackle-mode)
  :init
  (add-hook 'after-init-hook #'shackle-mode)
  :config
  (setq shackle-rules
        '(
          ;; C
          (rtags-mode :align below :size 0.4 :select nil)
          ;; Csharp
          ("OmniSharp" :regexp t :align below :size 0.3 :select t)
          ;; Elixir
          (alchemist-mix-mode :align below :size 0.3 :select nil)
          ("*alchemist help*" :regexp t :align below :size 0.2 :select t)
          (alchemist-iex-mode :align below :size 0.3 :select t)
          (alchemist-test-report-mode :align below :size 0.3 :select nil)
          ;; Flycheck
          ("*Flycheck checkers*" :regexp t :align below :size 0.3 :select nil)
          (flycheck-error-list-mode :align below :size 0.4 :select t)
          ;; Lua
          ("*pdrun*" :regexp t :align below :size 0.3 :select nil)
          ("*lua test results*" :regexp t :align below :size 0.3 :select nil)
          ;; Magit
          (magit-process-mode :align below :size 0.35 :select nil)
          ;; Python
          (anaconda-mode-view-mode :align below :size 0.3 :select t)
          ("*anaconda-mode*" :regexp t :align below :size 0.3 :select nil)
          ;; Scheme
          (geiser-doc-mode :align below :size 0.3 :select nil)
          (geiser-debug-mode :align below :size 0.3 :select nil)
          ;; Javascript
          (indium-debugger-locals-mode :align below :size 0.4 :select nil)
          ;; Typescript
          (tide-references-mode :regexp t :align below :size 0.3 :selct nil)
          ;; Web
          (mocha-compilation-mode :align below :size 0.3 :select nil)
          ;; Misc
          ("*+shell" :regexp t :align below :size 0.3 :select t)
          ("*Help*" :regexp t :align below :size 0.3 :select t)
          (undo-tree-visualizer-mode :align below :size 0.4 :select t)
          (xref--xref-buffer-mode :align below :size 0.4 :select nil)
          ("*evil-registers*" :regexp t :align below :size 0.3 :select nil)
          ("*Dired log*" :regexp t :align below :size 0.3 :select nil)
          ("*make*" :regexp t :align below :size 0.3 :select nil)
          ("*lein*" :regexp t :align below :size 0.3 :select nil)
          ("Async Shell Command*" :regexp t :align below :size 0.2 :select nil)
          ("*Shell Command Output*" :regexp t :align below :size 0.2 :select nil)
          ;; Perforce
          (p4-basic-mode :align below :size 0.35 :select nil))))

(use-package speed-type
  :ensure t
  :commands (speed-type-region
             speed-type-buffer
             speed-type-text))

(use-package ix
  :ensure t
  :commands (ix ix-browse ix-delete)
  :config
  (setq ix-user "jnjames")
  (setq ix-token "jnjames"))

(use-package rainbow-mode
  ;; Colors for various 'color codes' aka hex strings.
  :ensure t
  :commands (rainbow-mode)
  :init
  (add-hook 'nxml-mode-hook #'rainbow-mode)

  ;; Emacs 26 fontifies the color by default.
  (when (< emacs-major-version 26)
    (add-hook 'css-mode-hook
              (lambda ()
                ;; This 2 spaces check could go in a css mode package.
                ;; Adding it here for now out of laziness.
                (setq-local css-indent-offset (+indent-offset))
                (rainbow-mode))))
  :diminish rainbow-mode
  :config
  (rainbow-mode 1))

(use-package vlf
  :ensure t
  :commands (vlf)
  :config
  (require 'vlf-setup))

(use-package imenu-anywhere
  :ensure t
  :commands (imenu-anywhere))

(use-package prodigy
  :ensure t
  :defer 5
  :config
  (prodigy-define-service
    :name (concat "DeepLink Server: " (+get-ip-address) ":6001")
    :command "python"
    :args '("-m" "SimpleHTTPServer" "6001")
    :cwd "~/Dropbox/python-server"
    :tags '(work)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)
  (defun +prodigy-start-python-simple-http-server ()
    "Start Python SimpleHTTPServer from `prodigy'."
    (interactive)
    (+prodigy-start-service-for-name "Python SimpleHTTPServer")))

(use-package pass
  :ensure t
  :commands (pass)
  :config
  (use-package password-store
    :ensure t :config))

(use-package whitespace
  ;; Show trailing whitespace, tabs and lines over 80 characters.
  :ensure nil
  :diminish whitespace-mode
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  :config
  (setq whitespace-style '(face trailing tabs lines-tail)))

(use-package paren
  ;; Highlight matching parentheses.
  :ensure nil
  :config
  (defun +set-show-paren-match-face ()
    "Set `show-paren-match' face."
    (set-face-attribute 'show-paren-match nil :underline t))
  (+set-show-paren-match-face)
  (add-hook 'after-load-theme-hook #'+set-show-paren-match-face)

  (show-paren-mode t))

(use-package so-long
  ;; Work with files that have long lines.
  :ensure nil
  :config
  (setq so-long-threshold 50000)

  (push 'json-mode so-long-target-modes)
  (advice-add 'json-pretty-print-buffer :after
              (lambda ()
                (unless (bound-and-true-p json-mode)
                  (json-mode))))

  (defvar +so-long-disable-mode-list
    '(show-paren-mode
      ws-butler-mode
      highlight-symbol-mode
      rainbow-delimiters-mode
      eldoc-mode
      flycheck-mode)
    "Extra modes to disable when `so-long' is active.")

  (when (<= emacs-major-version 25)
    (push 'nlinum-mode +so-long-disable-mode-list))

  (defvar +so-long-enable-mode-list
    '(visual-line-mode)
    "Extra modes to enable when `so-long' is active.")

  (defun +so-long-disable-modes ()
    "Disable some modes when `so-long' is triggered."
    (dolist (mode +so-long-enable-mode-list)
      (funcall mode 1))
    (dolist (mode +so-long-disable-mode-list)
      (funcall mode -1))
    (so-long-revert-buffer-read-only)
    (+so-long-redefine-evil nil))

  (defun +so-long-reenable-modes ()
    "Reenable some modes when `so-long' is reverted."
    (dolist (mode +so-long-enable-mode-list)
      (funcall mode -1))
    (dolist (mode +so-long-disable-mode-list)
      (funcall mode 1))
    (+so-long-redefine-evil t))

  (defun +so-long-redefine-evil (&optional original)
    "Swap j/k and gj/gk keybinds.
If ORIGINAL is t, use original `evil-mode' keymap."
    (with-eval-after-load 'evil
      (let ((map evil-normal-state-local-map))
        (if original
            (progn
              (define-key map (kbd "gj") #'evil-next-visual-line)
              (define-key map (kbd "gk") #'evil-previous-visual-line)
              (define-key map (kbd "j") #'evil-next-line)
              (define-key map (kbd "k") #'evil-previous-line))
          (define-key map (kbd "j") #'evil-next-visual-line)
          (define-key map (kbd "k") #'evil-previous-visual-line)
          (define-key map (kbd "gj") #'evil-next-line)
          (define-key map (kbd "gk") #'evil-previous-line)))))

  (add-hook 'so-long-hook #'+so-long-disable-modes t)
  (add-hook 'so-long-revert-hook #'+so-long-reenable-modes t)
  (so-long-enable))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(provide 'jn-misc)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-misc.el ends here

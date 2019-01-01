;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

(use-package smartparens
  ;; :load-path "~/.emacs.d/fork/smartparens/"
  :ensure t
  :diminish smartparens-mode
  :init
  (defun j|sp-change-sexp (&optional arg dont-kill)
    "Like `sp-kill-sexp' but enter insert state after."
    (interactive)
    (sp-kill-sexp arg dont-kill)
    (evil-insert-state))

  (defvar j|smartparens-bindings
    '(("M-t" . sp-transpose-sexp)
      ("C-<right>" . sp-forward-slurp-sexp)
      ("C-<left>" . sp-forward-barf-sexp)
      ("C-M-<left>" . sp-backward-slurp-sexp)
      ("C-M-<right>" . sp-backward-barf-sexp)
      ("M-d" . sp-kill-sexp)
      ("M-y" . sp-copy-sexp)
      ("M-<backspace>" . sp-backward-kill-sexp)
      ("M-c" . j|sp-change-sexp)))

  (dolist (hook (j|lisp-hooks))
    (add-hook hook #'smartparens-strict-mode))
  :config
  ;; Disable highlights.
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-autoskip-closing-pair 'always-end
        sp-autoskip-opening-pair t)

  (use-package dsedivec-comment-sexp-dwim
    :ensure nil
    :bind
    (:map smartparens-strict-mode-map
          ("M-;" . dsedivec:lisp-comment-dwim)))

  (require 'smartparens-config)
  (smartparens-global-mode 1)

  (with-eval-after-load 'evil
    (dolist (alist j|smartparens-bindings)
      (evil-define-key* '(normal motion insert) smartparens-mode-map
                        (read-kbd-macro (car alist))
                        (cdr alist))))

  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "(" ")" :wrap "M-)")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "[" "]" :wrap "M-]")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "{" "}" :wrap "M-}")
  (sp-pair "\"" "\"" :wrap "M-\"")

  (sp-local-pair '(xml-mode nxml-mode php-mode) "<!--" "-->"
                 :post-handlers '(("| " "SPC")))

  (sp-local-pair '(emacs-lisp-mode
                   lisp-interaction-mode
                   inferior-emacs-lisp-mode) "`" "\'" :wrap "M-`")

  (sp-with-modes (j|lisp-modes)
    (sp-local-pair "(" nil :unless '(sp-in-string-p))
    (sp-local-pair "[" nil :unless '(sp-in-string-p))
    (sp-local-pair "{" nil :unless '(sp-in-string-p))
    (sp-local-pair "\"" nil :unless '(sp-point-before-word-p
                                      sp-point-after-word-p)))

  (sp-with-modes (j|standard-modes)
    (sp-local-pair "/*" "*/" :when '(sp-point-in-empty-line-p))

    (sp-local-pair "(" nil :unless '(sp-point-before-word-p sp-in-string-p))

    (sp-local-pair "{" "}" :when '(("RET" "<evil-ret>" "SPC"))
                   :unless '(sp-point-before-word-p sp-in-string-p)
                   :post-handlers '(j|reindent-and-position-middle))

    ;; Default `sp-pairs' adds a post-handler to single-quote.
    (sp-local-pair "'" nil :wrap "M-'"
                   :unless '(sp-in-string-quotes-p
                             sp-point-before-word-p
                             sp-point-after-word-p
                             sp-in-string-p)
                   :post-handlers nil))

  (sp-local-pair (remove 'objc-mode (j|standard-modes)) "[" nil
                 :unless '(sp-point-before-word-p sp-in-string-p))

  (sp-local-pair (remove 'objc-mode (j|standard-modes)) "\"" nil
                 :unless '(sp-point-before-word-p
                           sp-point-after-word-p))

  (defun objc-mode-handle-string-quotes (id action context)
    "Remove quotes when they were entered before or after a word.

If quotes were entered after a word, check if that word is a @.

Since @ is a string literal, don't remove the quote."
    (when (eq action 'insert)
      (save-excursion
        (forward-char 1)
        (when (sp-point-before-word-p id action context)
          (delete-char -1)))
      (save-excursion
        (when (sp-point-after-word-p id action context)
          (unless (save-excursion
                    (forward-char -1)
                    (sp--looking-back-p (regexp-quote "@")))
            (delete-char 1))))))

  (defun objc-mode-insert-right-bracket (id action context)
    (interactive)
    (save-excursion
      (forward-char 1)
      (when (or
             (sp-point-before-same-p id action context)
             (sp-point-before-word-p id action context))
        (delete-char -1)
        (forward-word 1)
        (insert "]"))))

  (defun objc-mode-maybe-insert-left-bracket-filter (_)
    "Return CMD if at start of unmodified snippet field.
Use as a `:filter' argument for a conditional keybinding."
    (unless (looking-at-p "]") #'objc-mode-insert-left-bracket))

  (defconst objc-mode-maybe-insert-left-bracket
    '(menu-item "" nil
                :filter objc-mode-maybe-insert-left-bracket-filter)
    "A conditional key definition.

Apply `objc-mode-maybe-insert-left-bracket-filter'.")

  (defun objc-mode-insert-left-bracket ()
    "Add a matching left bracket to pair with right bracket."
    (interactive)
    (insert "]")
    (when (not (nth 3 (syntax-ppss))) ;; When not in string.
      (forward-char -1)
      (save-excursion
        (while (not (or
                     (looking-at-p "=")
                     (looking-at-p "[[:space:]]")))
          (forward-char -1))
        (forward-char 1)
        (insert "[")
        (indent-according-to-mode))
      (forward-char 1)))

  (with-eval-after-load 'evil
    (evil-define-key 'insert objc-mode-map "]" objc-mode-maybe-insert-left-bracket))

  (sp-with-modes '(objc-mode)
    (sp-local-pair  "[" "]"
                    :unless '(sp-in-string-p)
                    :post-handlers '(objc-mode-insert-right-bracket))
    (sp-local-pair "\"" "\""
                   :post-handlers '(objc-mode-handle-string-quotes)))

  (defun j|reindent-and-position-middle (_id action _context)
    "Reindents and positions cursor in the middle."
    (cond
     ((eq 'skip-closing-pair action)
      t)
     ((string-match-p ".*{.*}.*" (thing-at-point 'line t))
      (insert " ")
      (backward-char))
     (:default
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode)))))

(use-package lispy
  :ensure t
  :diminish (lispy-mode)
  :init
  (dolist (hook (j|lisp-hooks))
    (add-hook hook (lambda ()
                     (lispy-mode)
                     (define-key lispy-mode-map (kbd "M-q") nil)
                     (define-key lispy-mode-map (kbd "[") nil)
                     (define-key lispy-mode-map (kbd "]") nil)
                     (define-key lispy-mode-map (kbd "}") nil)
                     (define-key lispy-mode-map (kbd "\"") nil)
                     (define-key lispy-mode-map (kbd "/") nil))))
  :config
  (setq lispy-teleport-global t
        lispy-no-permanent-semantic t
        lispy-visit-method 'projectile
        lispy-eval-display-style 'message
        lispy-insert-space-after-wrap nil)

  (defun lispy-action-then-next-sexp (lispy-action)
    "Return function that triggers LISPY-ACTION and then moves to next sexp."
    (defalias (intern (format "%S-then-next-sexp" lispy-action))
      (lambda ()
        (interactive)
        (call-interactively lispy-action)
        (unless (or (lispy-left-p)
                    (lispy-right-p)
                    (and (lispy-bolp)
                         (or (looking-at lispy-outline-header)
                             (looking-at lispy-outline))))
          (call-interactively #'sp-next-sexp)))))

  (defun noct:lispy-delete (arg)
    "Copy and delete current sexp.
Passes ARG to `lispy-delete' or `lispy-delete-back'."
    (interactive "p")
    (cond ((or (lispy-left-p)
               (region-active-p))
           (lispy-new-copy)
           (lispy-delete arg))
          ((lispy-right-p)
           (lispy-new-copy)
           (lispy-delete-backward arg))))

  (lispy-define-key lispy-mode-map-special
      "d" (lispy-action-then-next-sexp 'noct:lispy-delete)) ;; `lispy-different' -> o

  (lispy-define-key lispy-mode-map-special "M-j" 'lispy-move-down) ;; `lispy-split'
  (lispy-define-key lispy-mode-map-special "M-k" 'lispy-move-up) ;; `lispy-kill-sentence'

  ;; Disable in favor of `smart-jump'.
  (define-key lispy-mode-map-lispy (kbd "M-.") nil)
  (define-key lispy-mode-map-lispy (kbd "M-,") nil))

(use-package lispyville
  :ensure t
  :diminish (lispyville-mode)
  :commands (lispyville-mode)
  :init
  (dolist (hook (j|lisp-hooks))
    (add-hook hook #'lispyville-mode))

  (add-hook 'lispy-mode-hook
            (lambda ()
              (setq-local lispyville-preferred-lispy-state 'insert)
              (setq-local lispyville-motions-put-into-special t)))
  :config
  (setq evil-insert-state-tag `(:eval
                                (if (lispyville--lispy-keybindings-active-p)
                                    (propertize " Lispy ")
                                  (propertize " Insert "))))
  (lispyville-set-key-theme
   '(insert
     operators
     (additional-movement normal)
     slurp/barf-cp
     additional
     escape)))

(use-package ws-butler
  :defer 5
  :diminish ws-butler-mode
  :ensure t
  :config
  (setq ws-butler-keep-whitespace-before-point t)
  (ws-butler-global-mode))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (with-eval-after-load 'evil
    (defun j|hungry-delete-if-enabled (f &rest args)
      "Wrapper function to run `hungry-delete-backward' if
`hungry-delete-mode' is on."
      (interactive)
      (if (bound-and-true-p hungry-delete-mode)
          (hungry-delete-backward 1)
        ;; This is a hack. Not sure if args is ever populated but abuse the fact
        ;; that the function being advised expects a number and that generally
        ;; this will be called in insert state. So the number will generally be 1.
        (if args
            (apply f args)
          (apply f '(1)))))
    (advice-add 'evil-delete-backward-char
                :around #'j|hungry-delete-if-enabled)
    (advice-add 'evil-delete-backward-char-and-join
                :around #'j|hungry-delete-if-enabled))

  (with-eval-after-load 'smartparens
    (defun sp-hungry-delete-backward (f &rest args)
      "Integrate `smartparens' with `hungry-delete'."
      (if (bound-and-true-p smartparens-mode)
          (save-match-data
            ;; For some reason `sp-get-expression' doesn't get single-quote
            ;; pairs correctly.
            ;; `sp-delete-pair' -> `sp-get-sexp' -> `sp-get-expression'
            ;; Manually check for it and delete it.
            (if (and
                 (looking-at "\'")
                 (save-excursion
                   (forward-char -1)
                   (looking-at "\'")))
                (progn
                  (delete-char 1 nil)
                  (delete-char -1 nil))
              ;; https://github.com/syl20bnr/spacemacs/issues/6584
              (sp-delete-pair (nth 0 args))
              (apply f args)))
        (apply f args)))

    (advice-add 'hungry-delete-backward :around 'sp-hungry-delete-backward))

  (global-hungry-delete-mode))

(use-package flycheck
  ;; Syntax Checking
  :if (not (eq system-type 'windows-nt))
  :defer 4
  :ensure t
  :diminish flycheck-mode
  :commands (flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 1.2)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (defun j|flycheck-maybe-turn-on ()
    "Turn off `flycheck-mode' conditionally."
    (cond
     ((null buffer-file-name)
      (flycheck-mode))
     ((string= (file-name-extension (buffer-file-name)) "gpg")
      nil)
     (:else
      (flycheck-mode))))

  ;; Since we're adding the hook after :config, need to loop through
  ;; current buffers and turn on `flycheck' there.
  (add-hook 'prog-mode-hook #'j|flycheck-maybe-turn-on)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (or (derived-mode-p 'prog-mode)
                (derived-mode-p 'web-mode))
        (j|flycheck-maybe-turn-on)))))

(if (version<= "26" emacs-version)
    (use-package flycheck-posframe
      :ensure t
      :commands (flycheck-posframe-mode)
      :init
      (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
      :config
      (defun j|flycheck-posframe-configure-pretty-defaults ()
        "Configure some nicer settings for prettier display."
        (setq flycheck-posframe-warning-prefix "w: ")
        (setq flycheck-posframe-error-prefix "e: ")
        (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
        (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error))

      (j|flycheck-posframe-configure-pretty-defaults))
  (use-package flycheck-pos-tip
    :ensure t
    :after flycheck
    :config
    (setq flycheck-pos-tip-timeout 10)
    (flycheck-pos-tip-mode)))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  :config
  (setq hs-hide-comments nil
        hs-isearch-open t
        hs-allow-nesting t)

  (defvar j|hideshow-hs-hide nil "Current state of hideshow for toggling all.")
  (defun j|hideshow-toggle-hideshow-all ()
    "Toggle hideshow all."
    (interactive)
    (setq j|hideshow-hs-hide (not j|hideshow-hs-hide))
    (if j|hideshow-hs-hide
        (hs-hide-all)
      (hs-show-all)))

  (defun j|hs-toggle-node ()
    "Toggle fold with a heuristic dependent on language."
    (interactive)
    (let ((current-line (s-trim (thing-at-point 'line t))))
      (cond
       ((and (eq major-mode 'lua-mode)
             (j|hs-toggle-looking-for current-line "function" "end"))
        :folded)
       ((and (eq major-mode 'csharp-mode)
             (j|hs-toggle-looking-for current-line "#region" "#endregion"))
        :folded)
       ((j|hs-toggle-node-on-brace current-line)
        :folded)
       (:default
        (call-interactively #'hs-toggle-hiding)))))

  (defun j|hs-toggle-looking-for (current-line start-pattern end-pattern)
    "Toggle fold if CURRENT-LINE matches START-PATTERN and END-PATTERN.

Return :folded if folded, nil otherwise."
    (cond
     ((string-match-p start-pattern current-line)
      (save-excursion
        (end-of-line)
        (call-interactively #'hs-toggle-hiding))
      :folded)
     ((s-starts-with? end-pattern current-line)
      (save-excursion
        (end-of-line)
        (search-backward end-pattern)
        (backward-char 1)
        (call-interactively #'hs-toggle-hiding))
      :folded)
     (:no-fold nil)))

  (defun j|hs-toggle-node-on-brace (current-line)
    "Try to toggle node checking for { on the current line or the
very next line. Return :folded if folded, nil otherwise."
    (cond
     ((s-ends-with? "{" current-line)
      (save-excursion
        (end-of-line)
        (call-interactively #'hs-toggle-hiding))
      :folded)
     ((save-excursion
        (forward-line 1)
        (if (s-starts-with? "{" (s-trim (thing-at-point 'line t)))
            (progn
              (end-of-line)
              (call-interactively #'hs-toggle-hiding)
              t)
          nil))
      :folded)
     (:no-fold nil)))

  (with-eval-after-load 'evil
    (let ((shift-tab-keys (list [S-tab] [S-iso-lefttab] [(shift tab)] [backtab])))
      (mapc (lambda (key)
              (define-key evil-normal-state-map
                key #'j|hideshow-toggle-hideshow-all))
            shift-tab-keys))

    (define-key evil-normal-state-map (kbd "TAB") 'j|hs-toggle-node)
    (define-key evil-visual-state-map (kbd "TAB") 'j|hs-toggle-node)
    (define-key evil-motion-state-map (kbd "TAB") 'j|hs-toggle-node)
    ;; Set default Tab command.
    (define-key evil-insert-state-map (kbd "TAB") 'indent-for-tab-command))

  (mapc (lambda (alist)
          (let ((hook (car alist))
                (depth (cdr alist)))
            (add-hook
             hook
             (lambda ()
               (setq-local hs-hide-all-non-comment-function
                           (lambda ()
                             (hs-hide-level depth)
                             (forward-sexp depth)))))))
        '((csharp-mode-hook . 2)
          (java-mode-hook . 1)
          (kotlin-mode-hook . 1)
          (swift-mode-hook . 1)
          (typescript-mode-hook . 1)
          (web-mode-hook . 1))))

(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :init
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "M-m") #'er/expand-region)
    (define-key evil-visual-state-map (kbd "M-m") #'er/expand-region)))

(use-package emr
  :ensure t
  :commands (emr-show-refactor-menu)
  :config
  (emr-initialize))

(use-package so-long
  ;; Work with files that have long lines.
  :defer 5
  :ensure nil
  :config
  (setq so-long-threshold 50000)

  (push 'json-mode so-long-target-modes)
  (advice-add 'json-pretty-print-buffer :after
              (lambda ()
                (unless (bound-and-true-p json-mode)
                  (json-mode))))

  (defvar j|so-long-disable-mode-list
    '(show-paren-mode
      ws-butler-mode
      highlight-symbol-mode
      rainbow-delimiters-mode
      eldoc-mode
      flycheck-mode)
    "Extra modes to disable when `so-long' is active.")

  (when (<= emacs-major-version 25)
    (push 'nlinum-mode j|so-long-disable-mode-list))

  (defvar j|so-long-enable-mode-list
    '(visual-line-mode)
    "Extra modes to enable when `so-long' is active.")

  (defun j|so-long-disable-modes ()
    "Disable some modes when `so-long' is triggered."
    (dolist (mode j|so-long-enable-mode-list)
      (funcall mode 1))
    (dolist (mode j|so-long-disable-mode-list)
      (funcall mode -1))
    (so-long-revert-buffer-read-only)
    (j|so-long-redefine-evil nil))

  (defun j|so-long-reenable-modes ()
    "Reenable some modes when `so-long' is reverted."
    (dolist (mode j|so-long-enable-mode-list)
      (funcall mode -1))
    (dolist (mode j|so-long-disable-mode-list)
      (funcall mode 1))
    (j|so-long-redefine-evil t))

  (defun j|so-long-redefine-evil (&optional original)
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

  (add-hook 'so-long-hook #'j|so-long-disable-modes t)
  (add-hook 'so-long-revert-hook #'j|so-long-reenable-modes t)
  (so-long-enable))

(use-package paren
  ;; Highlight matching parentheses.
  :defer 5
  :ensure nil
  :config
  (defun j|set-show-paren-match-face ()
    "Set `show-paren-match' face."
    (set-face-attribute 'show-paren-match nil :underline t))
  (j|set-show-paren-match-face)
  (add-hook 'after-load-theme-hook #'j|set-show-paren-match-face)

  (show-paren-mode t))

(use-package whitespace
  ;; Show trailing whitespace, tabs and lines over 80 characters.
  :ensure nil
  :diminish whitespace-mode
  :init
  (defun j|whitespace-mode ()
    "Set up `whitespace-mode'."
    ;; (whitespace-mode)
    (if (memq major-mode '(go-mode))
        (setq-local whitespace-style '(face trailing lines-tail))
      (setq-local whitespace-style '(face trailing tabs lines-tail))))
  (add-hook 'prog-mode-hook #'j|whitespace-mode))

(use-package vlf
  :ensure t
  :commands (vlf)
  :config
  (require 'vlf-setup))

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
                (setq-local css-indent-offset (j|indent-offset))
                (rainbow-mode))))
  :diminish rainbow-mode
  :config
  (rainbow-mode 1))

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

          ;; `go-mode'
          (godoc-mode :align below :size 0.3 :select t)
          ("*Gofmt Errors" :regexp t :align below :size 0.2 :select nil)

          ;; `help'
          ("*Help*" :regexp t :align below :size 0.3 :select t)

          ;; `indium'
          (indium-debugger-locals-mode :align below :size 0.4 :select nil)

          ;; `lua-mode'
          ("*pdrun*" :regexp t :align below :size 0.3 :select nil)
          ("*lua test results*" :regexp t :align below :size 0.3 :select nil)

          ;; `magit'
          (magit-process-mode :align below :size 0.35 :select nil)

          ;; `mocha'
          (mocha-compilation-mode :align below :size 0.3 :select nil)

          ;; `omnisharp'
          ("OmniSharp" :regexp t :align below :size 0.3 :select t)

          ;; `p4'
          (p4-basic-mode :align below :size 0.35 :select nil)

          ;; `quickrun'
          ("*quickrun*" :regexp t :align below :size 0.3 :select nil)

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

          ;; `xref'
          (xref--xref-buffer-mode :align below :size 0.4 :select nil)

          ;; Misc
          ("*+shell" :regexp t :align below :size 0.3 :select t)
          ("*make*" :regexp t :align below :size 0.3 :select nil)
          ("*lein*" :regexp t :align below :size 0.3 :select nil)))
  (shackle-mode))

(use-package ace-jump-mode
  ;; Movement
  :ensure t
  :commands
  (ace-jump-mode))

(use-package super-save
  :ensure t
  :defer 5
  :diminish super-save-mode
  :config
  (setq super-save-remote-files nil)
  (defun j|super-save-command (f &rest args)
    "`super-save-command' advice to handle silently saving and disabling modes."
    (unless (or (memq major-mode '(emacs-lisp-mode
                                   snippet-mode
                                   go-mode))
                ;; Don't auto save in `eglot'.
                (bound-and-true-p eglot--managed-mode))
      (let ((save-silently t))
        (apply f args))))

  (advice-add 'super-save-command :around 'j|super-save-command)

  (setq super-save-idle-duration 1.5)
  (setq super-save-auto-save-when-idle t)
  (super-save-mode +1))

(provide 'jn-editing)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-editing.el ends here

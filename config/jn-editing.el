;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

(use-package smartparens
  ;; :load-path "~/.emacs.d/fork/smartparens/"
  :ensure t
  :diminish smartparens-mode
  :init
  (defun j-sp-change-sexp (&optional arg dont-kill)
    "Like `sp-kill-sexp' but enter insert state after."
    (interactive)
    (sp-kill-sexp arg dont-kill)
    (evil-insert-state))

  (defvar j-smartparens-bindings
    '(("M-t" . sp-transpose-sexp)
      ("C-<right>" . sp-forward-slurp-sexp)
      ("C-<left>" . sp-forward-barf-sexp)
      ("C-M-<left>" . sp-backward-slurp-sexp)
      ("C-M-<right>" . sp-backward-barf-sexp)
      ("M-d" . sp-kill-sexp)
      ("M-y" . sp-copy-sexp)
      ("M-<backspace>" . sp-backward-kill-sexp)
      ("M-c" . j-sp-change-sexp)))

  (dolist (hook (j-lisp-hooks))
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
    (dolist (alist j-smartparens-bindings)
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

  ;; (sp-with-modes 'org-mode
  ;;   (sp-local-pair "~" nil :actions :rem)
  ;;   (sp-local-pair "/" nil :actions :rem))

  (sp-with-modes (j-lisp-modes)
    ;; (sp-local-pair "'" nil :actions :rem)
    (sp-local-pair "(" nil :unless '(sp-in-string-p))
    (sp-local-pair "[" nil :unless '(sp-in-string-p))
    (sp-local-pair "{" nil :unless '(sp-in-string-p))
    (sp-local-pair "\"" nil :unless '(sp-point-before-word-p
                                      sp-point-after-word-p)))

  (sp-with-modes (j-standard-modes)
    (sp-local-pair "(" nil :unless '(sp-point-before-word-p sp-in-string-p))

    (sp-local-pair "/*" "*/" :when '(("RET" "<evil-ret>" "SPC"))
                   :unless '(sp-point-before-word-p sp-in-string-p)
                   :post-handlers '(j-reindent-and-position-middle))

    (sp-local-pair "{" "}" :when '(("RET" "<evil-ret>" "SPC"))
                   :unless '(sp-point-before-word-p sp-in-string-p)
                   :post-handlers '(j-reindent-and-position-middle))

    ;; Default `sp-pairs' adds a post-handler to single-quote.
    (sp-local-pair "'" nil :wrap "M-'"
                   :unless '(sp-in-string-quotes-p
                             sp-point-before-word-p
                             sp-point-after-word-p
                             sp-in-string-p)
                   :post-handlers nil))

  (defun j-reindent-and-position-middle (_id action _context)
    "Reindents and positions cursor in the middle."
    (cond
     ((eq 'skip-closing-pair action)
      t)
     ((let ((line (thing-at-point 'line t)))
        (or (string-match-p ".*{.*}.*" line) ;; { }
            (string-match-p "/\\* \\*/" line))) ;; /* */
      (insert " ")
      (backward-char))
     (:default
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode))))

  (sp-local-pair (remove 'objc-mode (j-standard-modes)) "[" nil
                 :unless '(sp-point-before-word-p sp-in-string-p))

  (sp-local-pair (remove 'objc-mode (j-standard-modes)) "\"" nil
                 :unless '(sp-point-before-word-p
                           sp-point-after-word-p))

  (require 'objc-bracket)
  (objc-bracket-evil-setup))

(use-package lispy
  :ensure t
  :diminish (lispy-mode)
  :init
  (dolist (hook (j-lisp-hooks))
    (unless (eq hook 'eshell-mode-hook)
      (add-hook hook (lambda ()
                       (lispy-mode)
                       (define-key lispy-mode-map (kbd "M-q") nil)
                       (define-key lispy-mode-map (kbd "[") nil)
                       (define-key lispy-mode-map (kbd "]") nil)
                       (define-key lispy-mode-map (kbd "}") nil)
                       (define-key lispy-mode-map (kbd "\"") nil)
                       (define-key lispy-mode-map (kbd "/") nil)))))
  (with-eval-after-load 'evil-collection-lispy
    (define-key evil-collection-lispy-mode-map (kbd "[") nil)
    (define-key evil-collection-lispy-mode-map (kbd "]") nil)
    (define-key evil-collection-lispy-mode-map-special (kbd "]") nil)
    (define-key evil-collection-lispy-mode-map-special (kbd "[") nil))
  :config
  (setq lispy-teleport-global t
        lispy-no-permanent-semantic t
        lispy-visit-method 'projectile
        lispy-eval-display-style 'message
        lispy-insert-space-after-wrap nil)
  (add-hook 'evil-collection-setup-hook
            (lambda (m _keymap)
              (when (eq m 'lispy)
                (evil-collection-define-key
                  'normal 'evil-collection-lispy-mode-map
                  (kbd "M-d") nil))))

  ;; Disable in favor of `smart-jump'.
  (define-key lispy-mode-map-lispy (kbd "M-.") nil)
  (define-key lispy-mode-map-lispy (kbd "M-,") nil))

(use-package lispyville
  :ensure t
  :diminish (lispyville-mode)
  :commands (lispyville-mode)
  :init
  (dolist (hook (j-lisp-hooks))
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

(unless YT-P
  (use-package ws-butler
    :defer 5
    :diminish ws-butler-mode
    :ensure t
    :config
    (setq ws-butler-keep-whitespace-before-point t)
    (ws-butler-global-mode)))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
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
  :if (not WINDOWS-P)
  :defer 4
  :ensure t
  :diminish flycheck-mode
  :commands (flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 1.2)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (defun j-flycheck-maybe-turn-on ()
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
  (add-hook 'prog-mode-hook #'j-flycheck-maybe-turn-on)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (or (derived-mode-p 'prog-mode)
                (derived-mode-p 'web-mode))
        (j-flycheck-maybe-turn-on)))))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (add-hook 'protobuf-mode-hook 'hs-minor-mode)
  :config
  (setq hs-hide-comments nil
        hs-isearch-open t
        hs-allow-nesting t)

  (defvar j-hideshow-hs-hide nil "Current state of hideshow for toggling all.")
  (defun j-hideshow-toggle-hideshow-all ()
    "Toggle hideshow all."
    (interactive)
    (setq j-hideshow-hs-hide (not j-hideshow-hs-hide))
    (if j-hideshow-hs-hide
        (hs-hide-all)
      (hs-show-all)))

  (defun j-hs-toggle-node ()
    "Toggle fold with a heuristic dependent on language."
    (interactive)
    (let ((current-line (s-trim (thing-at-point 'line t))))
      (cond
       ((and (eq major-mode 'lua-mode)
             (j-hs-toggle-looking-for current-line "function" "end"))
        :folded)
       ((and (eq major-mode 'csharp-mode)
             (j-hs-toggle-looking-for current-line "#region" "#endregion"))
        :folded)
       ((j-hs-toggle-node-on-brace current-line)
        :folded)
       (:default
        (call-interactively #'hs-toggle-hiding)))))

  (defun j-hs-toggle-looking-for (current-line start-pattern end-pattern)
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

  (defun j-hs-toggle-node-on-brace (current-line)
    "Try to toggle node checking for { on the current line or the
very next line. Return :folded if folded, nil otherwise."
    (cond
     ((s-ends-with? "{" current-line)
      (save-excursion
        (end-of-line)
        ;; Seems like end-of-line pops the cursor past the point where we can
        ;; toggle hide-show.
        (forward-char -1)
        (call-interactively #'hs-toggle-hiding))
      :folded)
     ((save-excursion
        (forward-line 1)
        (if (s-starts-with? "{" (s-trim (thing-at-point 'line t)))
            (progn
              (end-of-line)
              ;; Seems like end-of-line pops the cursor past the point where we can
              ;; toggle hide-show.
              (forward-char -1)
              (call-interactively #'hs-toggle-hiding)
              t)
          nil))
      :folded)
     (:no-fold nil)))

  (defun j-hs-bind-keys ()
    "Bind hide-show keys."
    (with-eval-after-load 'evil
      (define-key evil-normal-state-local-map
        (kbd "TAB") 'j-hs-toggle-node)
      (define-key evil-visual-state-local-map
        (kbd "TAB") 'j-hs-toggle-node)
      (define-key evil-motion-state-local-map
        (kbd "TAB") 'j-hs-toggle-node)
      (define-key evil-insert-state-local-map
        (kbd "TAB") 'indent-for-tab-command)
      (let ((shift-tab-keys
             (list [S-tab] [S-iso-lefttab] [(shift tab)] [backtab])))
        (mapc
         (lambda (key)
           (unless (derived-mode-p 'magit-mode)
             (define-key evil-normal-state-local-map
               key #'j-hideshow-toggle-hideshow-all)))
         shift-tab-keys))))

  (add-hook 'protobuf-mode-hook 'j-hs-bind-keys)
  (add-hook 'prog-mode-hook 'j-hs-bind-keys)

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

  (defvar j-so-long-disable-mode-list
    '(show-paren-mode
      ws-butler-mode
      highlight-symbol-mode
      rainbow-delimiters-mode
      eldoc-mode
      flycheck-mode)
    "Extra modes to disable when `so-long' is active.")

  (defvar j-so-long-enable-mode-list
    '(visual-line-mode)
    "Extra modes to enable when `so-long' is active.")

  (defun j-so-long-disable-modes ()
    "Disable some modes when `so-long' is triggered."
    (dolist (mode j-so-long-enable-mode-list)
      (funcall mode 1))
    (dolist (mode j-so-long-disable-mode-list)
      (funcall mode -1))
    (so-long-revert-buffer-read-only)
    (j-so-long-redefine-evil nil))

  (defun j-so-long-reenable-modes ()
    "Reenable some modes when `so-long' is reverted."
    (dolist (mode j-so-long-enable-mode-list)
      (funcall mode -1))
    (dolist (mode j-so-long-disable-mode-list)
      (funcall mode 1))
    (j-so-long-redefine-evil t))

  (defun j-so-long-redefine-evil (&optional original)
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

  (add-hook 'so-long-hook #'j-so-long-disable-modes t)
  (add-hook 'so-long-revert-hook #'j-so-long-reenable-modes t)
  (so-long-enable))

(use-package paren
  ;; Highlight matching parentheses.
  :defer 5
  :ensure nil
  :config
  (defun j-set-show-paren-match-face ()
    "Set `show-paren-match' face."
    (set-face-attribute 'show-paren-match nil :underline t))
  (j-set-show-paren-match-face)
  (add-hook 'after-load-theme-hook #'j-set-show-paren-match-face)

  (show-paren-mode t))

(use-package whitespace
  ;; Show trailing whitespace, tabs and lines over 80 characters.
  :ensure nil
  :diminish whitespace-mode
  :init
  (defun j-whitespace-mode ()
    "Set up `whitespace-mode'."
    ;; (whitespace-mode)
    (if (memq major-mode '(go-mode))
        (setq-local whitespace-style '(face trailing lines-tail))
      (setq-local whitespace-style '(face trailing tabs lines-tail))))
  (add-hook 'prog-mode-hook #'j-whitespace-mode))

(use-package vlf
  :ensure t
  :commands (vlf)
  :config
  (require 'vlf-setup))

(use-package rainbow-mode
  ;; Colors for various 'color codes' aka hex strings.
  :ensure t
  :diminish rainbow-mode
  :commands (rainbow-mode)
  :init
  (add-hook 'nxml-mode-hook #'rainbow-mode)
  :config
  (rainbow-mode 1))

(use-package super-save
  :ensure t
  :defer 5
  :diminish super-save-mode
  :config
  (setq super-save-remote-files nil)
  (defun j-super-save-command (f &rest args)
    "`super-save-command' advice to handle silently saving and disabling modes."
    (unless (or (memq major-mode '(emacs-lisp-mode
                                   snippet-mode
                                   go-mode))
                ;; Don't save in yt projects.
                YT-P
                ;; Don't auto save in `eglot'.
                (bound-and-true-p eglot--managed-mode)
                (and buffer-file-name
                     (string-equal
                      (file-name-extension buffer-file-name) "gpg")))
      (let ((save-silently t))
        (apply f args))))

  (advice-add 'super-save-command :around 'j-super-save-command)
  (setq super-save-idle-duration 1.5)
  (setq super-save-auto-save-when-idle t)
  (super-save-mode +1))

;; https://stackoverflow.com/questions/16090517/elisp-conditionally-change-keybinding/22863701
(defvar j-newline-or-indent-new-comment-line
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

(defun j-bind-newline-or-indent-comment-line ()
  "Bind `j-newline-or-indent-new-comment-line' to RET."
  (define-key (symbol-value (intern (format "%S-map" major-mode)))
    (kbd "RET") j-newline-or-indent-new-comment-line))

(dolist (hook (j-standard-mode-hooks))
  (add-hook hook #'j-bind-newline-or-indent-comment-line))

(provide 'jn-editing)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-editing.el ends here

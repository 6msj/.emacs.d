;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

(use-package smartparens
  ;; :load-path "~/.emacs.d/fork/smartparens/"
  :ensure t
  :diminish smartparens-mode
  :init
  (defun +sp-change-sexp (&optional arg dont-kill)
    "Like `sp-kill-sexp' but enter insert state after."
    (interactive)
    (sp-kill-sexp arg dont-kill)
    (evil-insert-state))

  (defvar +smartparens-bindings
    '(("C-M-f" . sp-forward-sexp)
      ("C-M-b" . sp-backward-sexp)
      ("C-M-d" . sp-down-sexp)
      ("C-M-e" . sp-up-sexp)
      ("C-M-u" . sp-backward-up-sexp)
      ("C-M-a" . sp-backward-down-sexp)
      ("C-M-n" . sp-next-sexp)
      ("C-M-p" . sp-previous-sexp)
      ("M-i" . sp-extract-before-sexp)
      ("M-a" . sp-extract-after-sexp)
      ("M-t" . sp-transpose-sexp)
      ("M-J" . sp-join-sexp)
      ("M-r" . sp-raise-sexp)
      ("M-s" . sp-splice-sexp)
      ("M-S" . sp-split-sexp)
      ("M-v" . sp-convolute-sexp)
      ("M-<up>" . sp-splice-sexp-killing-backward)
      ("M-<down>" . sp-splice-sexp-killing-forward)
      ("M-6" . sp-splice-sexp-killing-backward)
      ("M-4" . sp-splice-sexp-killing-forward)
      ("C-<right>" . sp-forward-slurp-sexp)
      ("C-<left>" . sp-forward-barf-sexp)
      ("C-M-<left>" . sp-backward-slurp-sexp)
      ("C-M-<right>" . sp-backward-barf-sexp)
      ("M-d" . sp-kill-sexp)
      ("M-y" . sp-copy-sexp)
      ("M-<backspace>" . sp-backward-kill-sexp)
      ("M-c" . +sp-change-sexp)))

  (dolist (hook (+lisp-hooks))
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

  (use-package smartparens-config :ensure nil)
  (smartparens-global-mode 1)

  (with-eval-after-load 'evil
    (dolist (alist +smartparens-bindings)
      (evil-define-key* '(normal motion insert evilified) smartparens-mode-map
                        (read-kbd-macro (car alist))
                        (cdr alist))))

  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "(" ")" :wrap "M-)")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "[" "]" :wrap "M-]")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "{" "}" :wrap "M-}")
  (sp-pair "\"" "\"" :wrap "M-\"")

  (sp-local-pair '(emacs-lisp-mode
                   lisp-interaction-mode
                   inferior-emacs-lisp-mode) "`" "\'" :wrap "M-`")

  (sp-with-modes (+lisp-modes)
    (sp-local-pair "(" nil :unless '(sp-in-string-p))
    (sp-local-pair "[" nil :unless '(sp-in-string-p))
    (sp-local-pair "{" nil :unless '(sp-in-string-p))
    (sp-local-pair "\"" nil :unless '(sp-point-before-word-p
                                      sp-point-after-word-p)))

  (sp-with-modes (+standard-modes)
    (sp-local-pair "/*" "*/" :when '(sp-point-in-empty-line-p))

    (sp-local-pair "(" nil :unless '(sp-point-before-word-p sp-in-string-p))

    (sp-local-pair "{" "}" :when '(("RET" "<evil-ret>" "SPC"))
                   :unless '(sp-point-before-word-p sp-in-string-p)
                   :post-handlers '(+reindent-and-position-middle))

    (sp-local-pair "'" nil :wrap "M-'"
                   :unless '(sp-point-before-word-p
                             sp-point-after-word-p
                             sp-in-string-p)))

  (sp-local-pair (remove 'objc-mode (+standard-modes)) "[" nil
                 :unless '(sp-point-before-word-p sp-in-string-p))

  (sp-local-pair (remove 'objc-mode (+standard-modes)) "\"" nil
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

  (defun objc-mode-insert-left-bracket ()
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
    (evil-define-key 'insert objc-mode-map "]" 'objc-mode-insert-left-bracket))

  (sp-with-modes '(objc-mode)
    (sp-local-pair  "[" "]"
                    :unless '(sp-in-string-p)
                    :post-handlers '(objc-mode-insert-right-bracket))
    (sp-local-pair "\"" "\""
                   :post-handlers '(objc-mode-handle-string-quotes)))

  (defun +reindent-and-position-middle (_id action _context)
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

(use-package lispyville
  :ensure t
  :diminish (lispyville-mode)
  :commands
  (lispyville-mode)
  :init
  (dolist (hook (+lisp-hooks))
    (add-hook hook (lambda ()
                     (lispyville-mode))))
  :config
  (lispyville-set-key-theme
   '(operators
     s-operators
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
  (defun +hungry-delete-if-enabled (f &rest args)
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

  (with-eval-after-load 'evil
    (advice-add 'evil-delete-backward-char
                :around #'+hungry-delete-if-enabled)
    (advice-add 'evil-delete-backward-char-and-join
                :around #'+hungry-delete-if-enabled))

  (with-eval-after-load 'smartparens
    ;; https://github.com/syl20bnr/spacemacs/issues/6584
    (defadvice hungry-delete-backward (before sp-delete-pair-advice activate)
      (save-match-data
        (sp-delete-pair (ad-get-arg 0)))))

  (global-hungry-delete-mode))

(use-package flycheck
  ;; Syntax Checking
  ;; :load-path "~/.emacs.d/fork/flycheck/"
  :if (not (eq system-type 'windows-nt))
  :defer 4
  :ensure t
  :diminish flycheck-mode
  :commands (flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 1.2)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (defun +flycheck-maybe-turn-on ()
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
  (add-hook 'prog-mode-hook #'+flycheck-maybe-turn-on)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (derived-mode-p 'prog-mode)
        (+flycheck-maybe-turn-on)))))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10)
  (flycheck-pos-tip-mode))

(use-package fold-dwim-org
  ;; Folding
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (add-hook 'prog-mode-hook 'fold-dwim-org/minor-mode)
  (defun +hs-toggle-node ()
    "Toggle fold with a heuristic dependent on language."
    (interactive)
    (let ((current-line (s-trim (thing-at-point 'line t))))
      (cond
       ((and (eq major-mode 'lua-mode)
             (+lua-hs-toggle-node current-line))
        :folded)
       ((and (eq major-mode 'csharp-mode)
             (+csharp-hs-toggle-node current-line))
        :folded)
       ((+hs-toggle-node-on-brace current-line)
        :folded)
       (:default
        (call-interactively #'hs-toggle-hiding)))))

  (defun +lua-hs-toggle-node (current-line)
    "Try to toggle node checking for specific `lua-mode' keywords.
Return :folded if folded, nil otherwise."
    (cond
     ((string-match-p "function")
      (save-excursion
        (end-of-line)
        (call-interactively #'hs-toggle-hiding))
      :folded)
     ((s-starts-with? "end" current-line)
      (save-excursion
        (end-of-line)
        (search-backward "end")
        (backward-char 1)
        (call-interactively #'hs-toggle-hiding))
      :folded)
     (:no-fold nil)))

  (defun +csharp-hs-toggle-node (current-line)
    "Try to toggle node checking for specific `csharp-mode' keywords.
Return :folded if folded, nil otherwise."
    (cond
     ((s-starts-with? "#region" current-line)
      (save-excursion
        (end-of-line)
        (call-interactively #'hs-toggle-hiding))
      :folded)
     ((s-starts-with? "#endregion" current-line)
      (save-excursion
        (end-of-line)
        (search-backward "#endregion")
        (backward-char 1)
        (call-interactively #'hs-toggle-hiding))
      :folded)
     (:no-fold nil)))

  (defun +hs-toggle-node-on-brace (current-line)
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

  (defun +hs-hide-level-1-folds ()
    "Toggle Fold All except top one level nodes."
    (hs-hide-level 1)
    (forward-sexp 1))

  (defun +hs-hide-level-2-folds ()
    "Toggle Fold All except top two level nodes."
    (hs-hide-level 2)
    (forward-sexp 2))

  (add-hook 'typescript-mode-hook
            (lambda ()
              (set (make-local-variable
                    'hs-hide-all-non-comment-function)
                   #'+hs-hide-level-1-folds)))

  (add-hook 'java-mode-hook
            (lambda ()
              (set (make-local-variable
                    'hs-hide-all-non-comment-function)
                   #'+hs-hide-level-1-folds)))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (set (make-local-variable
                      'hs-hide-all-non-comment-function)
                     #'+hs-hide-level-1-folds))))

  (add-hook 'csharp-mode-hook
            (lambda ()
              (set (make-local-variable
                    'hs-hide-all-non-comment-function)
                   #'+hs-hide-level-2-folds)))

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "TAB") '+hs-toggle-node)
    (define-key evil-visual-state-map (kbd "TAB") '+hs-toggle-node)
    (define-key evil-motion-state-map (kbd "TAB") '+hs-toggle-node)
    ;; Set default Tab command.
    (define-key evil-insert-state-map (kbd "TAB") 'indent-for-tab-command))
  (setq fold-dwim-org-strict nil))

(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :init
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "M-m") #'er/expand-region)
    (define-key evil-visual-state-map (kbd "M-m") #'er/expand-region)))

(provide 'jn-editing)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-editing.el ends here

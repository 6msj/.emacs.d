;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)
(require 'jn-dependencies)

(use-package evil
  ;; :load-path "~/.emacs.d/fork/evil"
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-move-beyond-eol nil)
  :config
  ;; `undo-tree' settings.
  (with-eval-after-load 'undo-tree
    (diminish 'undo-tree-mode)
    ;; Double undo limit
    (setq undo-limit 160000
          undo-strong-limit 240000
          undo-outer-limit 24000000))

  (define-key global-map (kbd "M-u") 'universal-argument)
  (define-key universal-argument-map (kbd "C-u") nil)
  (define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

  ;; Search by symbol.
  (setq-default evil-symbol-word-search t)

  ;; `isearch' is faster than `evil-search' so use it even though `evil-search'
  ;; has `evil-paste-from-register' support.
  (setq evil-search-module 'isearch)

  (setq evil-ex-search-persistent-highlight nil)

  (evil-ex-define-cmd "W" #'evil-write-all) ;; :wa[ll] also works.
  (evil-ex-define-cmd "al[ign]" #'align-regexp)
  (evil-ex-define-cmd "formata[ll]" #'j|format-open-buffers)
  (evil-ex-define-cmd "sh[ell]" #'j|open-shell)
  (evil-ex-define-cmd "git" #'magit-status)
  (evil-ex-define-cmd "gstage" #'magit-stage)
  (evil-ex-define-cmd "gu[nstage]" #'magit-unstage)
  (evil-ex-define-cmd "gb[lame]" #'magit-blame)
  (evil-ex-define-cmd "a" #'projectile-find-other-file)
  (evil-ex-define-cmd "er[rors]" #'flycheck-list-errors)
  (evil-ex-define-cmd "[colump]aste" #'j|paste-column)

  (setq evil-jumps-max-length 1000)

  ;; Set underscore to be a word.
  ;; https://github.com/emacs-evil/evil
  (modify-syntax-entry ?_ "w")

  ;; C-S-o jumps foward in jumplist, C-o goes the other way.
  (setq evil-want-C-i-jump nil)
  (define-key evil-motion-state-map (kbd "C-S-o") 'evil-jump-forward)

  (setq evil-mode-line-format 'after)
  (setq evil-normal-state-tag   nil
        evil-emacs-state-tag    (propertize " Emacs ")
        evil-insert-state-tag   (propertize " Insert ")
        evil-motion-state-tag   (propertize " Motion ")
        evil-visual-state-tag   (propertize " Visual ")
        evil-replace-state-tag  (propertize " Replace ")
        evil-operator-state-tag (propertize " Operator "))

  (evil-mode 1)

  ;; Reselect text after identing.
  ;; https://superuser.com/questions/469327/combining-two-operators-in-evil-mode-emacs
  (define-key evil-visual-state-map "g>" 'evil-shift-right)
  (define-key evil-visual-state-map "g<" 'evil-shift-left)
  (define-key evil-visual-state-map ">" (kbd "g>gv"))
  (define-key evil-visual-state-map "<" (kbd "g<gv"))

  ;; Clear this for M-. (Go To Definition).
  (define-key evil-normal-state-map (kbd "M-.") nil)

  (add-hook 'buffer-menu-mode-hook
            (lambda ()
              (define-key Buffer-menu-mode-map (kbd "g") nil)))

  ;; Fix black cursor.
  (setq evil-default-cursor t))

(use-package evil-collection
  :load-path "~/.emacs.d/fork/evil-collection"
  :ensure nil
  :after evil
  :config
  (push 'lispy evil-collection-mode-list)
  (evil-collection-init))

(use-package evil-matchit
  :ensure t
  :bind (:map
         evil-normal-state-map
         ("%" . evilmi-jump-items)
         :map
         evil-visual-state-map
         ("%" . evilmi-jump-items))
  :init
  (defun evilmi-customize-keybinding ()
    "Define `evilmi-customize-keybinding' so that `evil-matchit' doesn't bind
any keys on its own."
    nil)
  :config
  (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :ensure t
  :init
  ;; Advise star/pound search to be case insensitive.
  ;; This advice could also go into the evil package.
  ;; This probably causes the search highlight to flicker
  ;; while going to the next search candidate.
  (defun j|evil-visual-star-ignorecase (orig-fun &rest args)
    (interactive)
    (let ((evil-ex-search-case 'insensitive))
      (apply orig-fun args)))

  (advice-add 'evil-ex-search-word-backward :around #'j|evil-visual-star-ignorecase)
  (advice-add 'evil-ex-search-word-forward :around #'j|evil-visual-star-ignorecase)
  :bind (:map evil-visual-state-map
              ("*" . evil-visualstar/begin-search-forward)
              ("#" . evil-visualstar/begin-search-backward))
  :config
  (global-evil-visualstar-mode)
  (setq evil-visualstar/persistent t))

(use-package evil-magit
  ;; Magit integration
  :ensure t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

(use-package evil-surround
  :ensure t
  :config
  ;; FIXME: Add in Typescript/etc for these surrounds.
  (add-hook 'web-mode-hook
            (lambda ()
              ;; FIXME: Need to check if Typescript..
              (push '(?< . ("< " . " >")) evil-surround-pairs-alist)))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (push '(?< . ("< " . " >")) evil-surround-pairs-alist)))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-line)
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)
              ("gy" . evil-commentary-yank))
  :init
  :config
  (evil-commentary-mode))

(use-package evil-mc
  ;; :load-path "~/.emacs.d/fork/evil-mc"
  :after evil
  :diminish evil-mc-mode
  :ensure t
  :config
  ;; ESC quits multiple cursors.
  (defun j|evil-mc-undo-cursors-on-esc (&rest _)
    "Disable multiple cursors on ESC if they were in 'normal state
when escaping."
    (interactive)
    (when (and
           (bound-and-true-p evil-mc-mode)
           (eq evil-state 'normal))
      (evil-mc-undo-all-cursors)))

  (advice-add 'evil-force-normal-state :before #'j|evil-mc-undo-cursors-on-esc)

  ;; Ctrl-G quits multiple cursors.
  (setq evil-mc-undo-cursors-on-keyboard-quit t)

  (dolist (hook '(ag-mode-hook
                  prog-mode-hook
                  wdired-mode-hook))
    (add-hook hook #'evil-mc-mode)))

(use-package evil-numbers
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-c +" . evil-numbers/inc-at-pt)
              ("C-c -" . evil-numbers/dec-at-pt)))

(use-package evil-org
  :diminish evil-org-mode
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

(provide 'jn-evil)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-evil.el ends here

;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)
(require 'jn-dependencies)

(use-package undo-tree
  ;; Vim Style Undo
  :ensure t
  :diminish undo-tree-mode
  :config
  ;; Double undo limit
  (setq undo-limit 160000)
  (setq undo-strong-limit 240000)
  (setq undo-outer-limit 24000000)
  (global-undo-tree-mode))

(use-package evil
  ;; :load-path "~/.emacs.d/fork/evil"
  :ensure t
  :init
  (setq evil-want-integration nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-move-beyond-eol nil)
  :config
  ;; Search by symbol.
  (setq-default evil-symbol-word-search t)

  ;; `isearch' is faster than `evil-search' so use it even though `evil-search'
  ;; has `evil-paste-from-register' support.
  (setq evil-search-module 'isearch)

  (setq evil-ex-search-persistent-highlight nil)

  (evil-ex-define-cmd "W" #'evil-write-all) ;; :wa[ll] also works.
  (evil-ex-define-cmd "al[ign]" #'align-regexp)
  (evil-ex-define-cmd "formata[ll]" #'+format-open-buffers)
  (evil-ex-define-cmd "sh[ell]" #'+open-shell)
  (evil-ex-define-cmd "git" #'magit-status)
  (evil-ex-define-cmd "gstage" #'magit-stage)
  (evil-ex-define-cmd "gu[nstage]" #'magit-unstage)
  (evil-ex-define-cmd "gb[lame]" #'magit-blame)
  (evil-ex-define-cmd "a" #'projectile-find-other-file)
  (evil-ex-define-cmd "er[rors]" #'flycheck-list-errors)
  (evil-ex-define-cmd "[colump]aste" #'+paste-column)

  (setq evil-jumps-max-length 1000)

  ;; Set underscore to be a word.
  ;; https://github.com/emacs-evil/evil
  (modify-syntax-entry ?_ "w")

  ;; http://spacemacs.org/doc/FAQ
  ;; https://github.com/syl20bnr/spacemacs/issues/2032
  ;; https://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-flash-delay 8) ;; control the highlight time of searches

  ;; C-S-o jumps foward in jumplist, C-o goes the other way.
  (setq evil-want-C-i-jump nil)
  (define-key evil-motion-state-map (kbd "C-S-o") 'evil-jump-forward)

  (setq evil-normal-state-tag   (propertize " *Normal* ")
        evil-emacs-state-tag    (propertize " *Emacs* ")
        evil-insert-state-tag   (propertize " *Insert* ")
        evil-motion-state-tag   (propertize " *Motion* ")
        evil-visual-state-tag   (propertize " *Visual* ")
        evil-replace-state-tag  (propertize " *Replace* ")
        evil-operator-state-tag (propertize " *Operator* "))

  (evil-set-initial-state 'etags-select-mode 'motion)
  (evil-set-initial-state 'debugger-mode 'motion)
  (evil-set-initial-state 'messages-buffer-mode 'motion)
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
  (defun +evil-visual-star-ignorecase (orig-fun &rest args)
    (interactive)
    (let ((evil-ex-search-case 'insensitive))
      (apply orig-fun args)))

  (advice-add 'evil-ex-search-word-backward :around #'+evil-visual-star-ignorecase)
  (advice-add 'evil-ex-search-word-forward :around #'+evil-visual-star-ignorecase)
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

(use-package evil-ediff
  :ensure t
  :commands (evil-ediff-init)
  :init
  (defun +evil-ediff-init ()
    "Initialize with `evil-ediff-init' and remove the hook."
    (evil-ediff-init)
    (remove-hook 'ediff-mode-hook #'evil-ediff-init))
  (add-hook 'ediff-mode-hook #'+evil-ediff-init)
  :config
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package evil-mc
  ;; :load-path "~/.emacs.d/fork/evil-mc"
  :after evil
  :diminish evil-mc-mode
  :ensure t
  :config
  (defun +remove-evil-mc-keymap (&rest _args)
    "Brute force remove `evil-mc-key-map' from `evil-mode-map-alist'."
    (when-let* ((evil-mc-map (assq 'evil-mc-mode evil-mode-map-alist)))
      (setq evil-mode-map-alist
            (delq evil-mc-map evil-mode-map-alist))))

  (advice-add 'evil-normalize-keymaps :after '+remove-evil-mc-keymap)

  ;; https://github.com/gabesoft/evil-mc/issues/22
  (defun col-at-point (point)
    (save-excursion (goto-char point) (current-column)))

  (defun evil--mc-make-cursor-at-col-append (_startcol endcol orig-line)
    (end-of-line)
    (when (> endcol (current-column))
      (insert-char ?\s (- endcol (current-column))))
    (move-to-column (- endcol 1))
    (unless (= (line-number-at-pos) orig-line)
      (evil-mc-make-cursor-here)))

  (defun evil--mc-make-cursor-at-col-insert (startcol _endcol orig-line)
    (end-of-line)
    (move-to-column startcol)
    (unless (or (= (line-number-at-pos) orig-line) (> startcol (current-column)))
      (evil-mc-make-cursor-here)))

  (defun evil--mc-make-vertical-cursors (beg end func)
    (evil-mc-pause-cursors)
    (apply-on-rectangle func
                        beg end (line-number-at-pos (point)))
    (evil-mc-resume-cursors)
    (evil-normal-state))

  (defun evil-mc-insert-vertical-cursors (beg end)
    (interactive (list (region-beginning) (region-end)))
    (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-insert)
    (move-to-column (min (col-at-point beg) (col-at-point end))))

  (defun evil-mc-append-vertical-cursors (beg end)
    (interactive (list (region-beginning) (region-end)))
    (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-append)
    (move-to-column (- (max (col-at-point beg) (col-at-point end)) 1)))

  ;; https://github.com/gabesoft/evil-mc/issues/70
  (add-hook 'evil-mc-after-cursors-deleted
            (lambda ()
              (setq evil-was-yanked-without-register t)))

  ;; ESC quits multiple cursors.
  (defun +evil-mc-undo-cursors-on-esc (&rest _)
    "Disable multiple cursors on ESC if they were in 'normal state
when escaping."
    (interactive)
    (when (and
           (bound-and-true-p evil-mc-mode)
           (eq evil-state 'normal))
      (evil-mc-undo-all-cursors)))

  (advice-add 'evil-force-normal-state :before #'+evil-mc-undo-cursors-on-esc)

  ;; Ctrl-G quits multiple cursors.
  (setq evil-mc-undo-cursors-on-keyboard-quit t)

  (defun +evil-mc-make-cursors-in-defun ()
    "Like `evil-mc-make-all-cursors' but only in the current defun."
    (interactive)
    (if (evil-mc-has-cursors-p)
        (evil-mc-undo-all-cursors)
      (let ((window-start (window-start)))
        (save-restriction
          (widen)
          (narrow-to-defun)
          (evil-mc-make-all-cursors))
        (set-window-start nil window-start))))

  (defun +setup-evil-mc-keys-locally ()
    "Set up key bindings for `evil-mc' using `evil-normal-state-local-map'."
    (dolist (evil-map-symbol '(evil-normal-state-local-map
                               evil-visual-state-local-map
                               evil-motion-state-local-map))
      (let ((map (symbol-value evil-map-symbol)))
        (define-key map (kbd "C-=") '+evil-mc-make-cursors-in-defun)
        (define-key map (kbd "C-<") 'evil-mc-insert-vertical-cursors)
        (define-key map (kbd "C->") 'evil-mc-append-vertical-cursors)
        (define-key map (kbd "M-n") 'evil-mc-make-and-goto-next-cursor)
        (define-key map (kbd "M-p") 'evil-mc-make-and-goto-prev-cursor)
        (define-key map (kbd "C-n") 'evil-mc-make-and-goto-next-match)
        (define-key map (kbd "C-p") 'evil-mc-make-and-goto-prev-match)
        (define-key map (kbd "C-*") 'evil-mc-make-all-cursors))))

  (defun +setup-evil-mc-mode ()
    "Set up `evil-mc-mode'."
    (evil-mc-mode)
    (+setup-evil-mc-keys-locally))

  (add-hook 'ag-mode-hook #'+setup-evil-mc-mode)
  (add-hook 'prog-mode-hook #'+setup-evil-mc-mode)
  (add-hook 'wdired-mode-hook #'+setup-evil-mc-mode))

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

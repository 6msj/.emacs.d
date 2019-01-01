;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :commands (org-agenda org-capture org-store-link)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :init
  (defhydra hydra-org-space (:color blue :hint nil)
    "

    Org:
  ------------------------------------------------------------------------------
    _a_ Agenda    _c_ Capture    _r_ Refile    _t_ Todo

    _l_ Store Link   _i_ Insert Link

"
    ("a" org-agenda)
    ("c" org-capture)
    ("l" org-store-link)
    ("i" org-insert-link)
    ("r" org-refile)
    ("o" org-todo)
    ("t" org-todo))

  (defun j|org-maybe-setup-writing ()
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

  (defun j|org-japanese-toggle-input-method ()
    "Toggle input method for Japanese."
    (interactive)
    (cond
     ((eq default-input-method 'japanese-hiragana)
      (set-input-method 'japanese-katakana t))
     ((eq default-input-method 'japanese-katakana)
      (set-input-method nil t))
     (:default
      (set-input-method 'japanese-hiragana t)))
    (if default-input-method
        (message (format "Current input method is %S." default-input-method))
      (message (format "Current input method is English."))))

  (defun j|org-maybe-setup-japanese ()
    "Set up Japanese input mode toggling."
    (when (string-equal (buffer-name) "japan.org")
      (set-fill-column 80)
      (setq-local buffer-face-mode-face `(:family "Helvetica" :height
                                                  ,(if (j|desktop-p) 160 140)))
      (buffer-face-mode)
      (define-key org-mode-map (kbd "C-\\") 'j|org-japanese-toggle-input-method)))

  (defun j|org-customize-ui ()
    "Customize various Org Mode UI Elements"
    (set-face-attribute 'org-document-title nil :weight 'bold :height 1.4)
    (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.3 :weight 'bold)
    (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.2 :weight 'bold)
    (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.1)
    (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :height 1.0))

  (add-hook 'org-mode-hook
            (lambda ()
              (j|org-maybe-setup-writing)
              (j|org-maybe-setup-japanese)
              (j|org-customize-ui)))
  :config
  (require 'ox-odt) ;; Open Document Format

  (require 'ob-dot) ;; Graphviz
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  (require 'org-notmuch)
  (with-eval-after-load 'evil
    (evil-define-key 'normal org-mode-map
      (kbd "M-.") 'org-open-at-point
      [tab] #'j|org-indent-block-automatically-or-cycle
      (kbd "TAB") #'j|org-indent-block-automatically-or-cycle))

  (when (eq system-type 'darwin)
    (setq org-directory "~/Dropbox/Notes")
    (setq org-agenda-files '("~/Dropbox/Notes")))
  (when (eq system-type 'windows-nt)
    (setq org-directory "C:/Users/james/Dropbox/Notes")
    (setq org-agenda-files '("C:/Users/james/Dropbox/Notes")))

  (setq org-capture-templates
        '((;; Standard Todo
           "t" "Todo" entry
           (file+headline (concat org-directory "/mine.org") "Tasks")
           "* TODO %u %a %?\n")
          (;; Add as Productivity task.
           "p" "Productivity" entry
           (file+headline (concat org-directory "/mine.org") "Productivity")
           "* TODO %u %?\n")
          (;; Add as Work task.
           "w" "Work" entry
           (file+headline (concat org-directory "/work.org") "Work")
           "* TODO %u %?\n")
          (;; Handle this message in the next two days.
           "h" "High Priority" entry
           (file+headline (concat org-directory "/mine.org") "High Priority")
           "* TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
          (;; Wait for an E-mail reply.
           "r" "Wait for Reply"
           entry (file+headline (concat org-directory "/mine.org") "Waiting")
           "* WAIT %u %a %?\n")))

  (defun j|org-indent-block-automatically-or-cycle ()
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
      (kbd "<tab>") 'j|org-indent-block-automatically-or-cycle))

  (setq org-export-backends '(ascii html icalendar latex md))
  (setq org-src-fontify-natively t)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-goto-interface 'outline-path-completion
        org-goto-max-level 10))

(use-package htmlize
  :commands (htmlize-buffer
             htmlize-region
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired)
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t
  :mode
  ("\\.dot\\'" . graphviz-dot-mode)
  ("\\.gv\\'" . graphviz-dot-mode)
  :init
  (setq default-tab-width 4))

(use-package ox-pandoc
  :ensure t
  :after org-mode)

(provide 'jn-org)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-org.el ends here

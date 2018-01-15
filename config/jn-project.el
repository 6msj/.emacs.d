;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

(use-package ivy
  ;; :load-path "~/.emacs.d/fork/swiper"
  :ensure t
  :bind (:map ivy-minibuffer-map
              ("M-x" . ivy-dispatching-done))
  :config
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))
  (with-eval-after-load 'dumb-jump
    (setq dumb-jump-selector 'ivy))
  (with-eval-after-load 'mu4e
    (setq mu4e-completing-read-function 'ivy-completing-read))
  (with-eval-after-load 'rtags
    (setq rtags-display-result-backend 'ivy))

  (setq ivy-use-virtual-buffers nil)
  (setq ivy-flx-limit 100)
  (setq ivy-re-builders-alist
        '((counsel-git-log . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-multi . ivy--regex-plus)
          (projectile-completing-read . ivy--regex-fuzzy)
          (counsel-fzf . regexp-quote)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)

  ;; swapping behavior
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

  (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)

  ;; Escape quits.
  (with-eval-after-load 'evil
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit))

  (setq ivy-count-format "")
  (setq ivy-height 15)
  (ivy-mode))

(use-package counsel
  ;; :load-path "~/.emacs.d/fork/swiper"
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-y" . counsel-yank-pop))
  :commands (counsel-ag
             counsel-find-file
             counsel-fzf
             counsel-git
             counsel-rg
             counsel-yank-pop)
  :init
  (setq projectile-switch-project-action 'counsel-fzf)
  :config
  (setq counsel-yank-pop-height 15)
  (setq counsel-evil-registers-height 15)
  (ivy-set-prompt 'counsel-fzf (lambda () "> "))

  (defun counsel-configure-fzf (&rest _)
    "Configure command to be piped into `counsel-fzf'."
    (cond
     ((eq major-mode 'dired-mode)
      (if (executable-find "fd")
          (setenv "FZF_DEFAULT_COMMAND" "fd .")
        (setenv "FZF_DEFAULT_COMMAND" "find .")))
     (:default
      (setenv "FZF_DEFAULT_COMMAND"
              "(git ls-files --recurse-submodules --exclude-standard --cached ||
        find . -maxdepth 2 -path \"*/\\.*\" -prune -o -print -o -type l -print |
           sed s/^..//) 2> /dev/null"))))

  (advice-add 'counsel-fzf :before 'counsel-configure-fzf)

  (setq ivy-use-selectable-prompt t)
  (setq counsel-async-filter-update-time 10000)
  (setq ivy-dynamic-exhibit-delay-ms 20)

  (setq counsel-git-cmd
        "git ls-files --exclude-standard --full-name --others --cached --")
  (setq counsel-rg-base-command
        "rg --max-columns 80 -i --no-heading --line-number --color never %s .")
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- ."))

(use-package swiper
  ;; :load-path "~/.emacs.d/fork/swiper"
  :ensure t
  :commands (swiper)
  :init
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "C-s") 'swiper))
  :diminish ivy-mode)

(use-package flx
  :ensure t
  :defer t)

(use-package smex :ensure t :defer t)

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup)
  :init
  (add-hook 'ag-mode-hook #'wgrep-ag-setup)
  (add-hook 'rg-mode-hook #'wgrep-ag-setup)
  :config
  ;; Match `wdired-mode' keybinding.
  (setq wgrep-enable-key "\C-x\C-q")
  (setq wgrep-auto-save-buffer t))

(use-package ag
  :ensure t
  :commands (ag ag-project)
  :init
  ;; Redefine function so that it works when autoloading.
  (defun ag/dwim-at-point ()
    "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
    (cond ((use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))
          ((symbol-at-point)
           (substring-no-properties
            (symbol-name (symbol-at-point))))))
  :config
  (setq ag-ignore-list '(;; Binary Directories
                         "obj/"
                         "bin/"
                         ;; Elisp
                         "backups/*"
                         "elpa/archives/*"
                         "elpa/25/archives/*"
                         "elpa/26/archives/*"
                         "elpa/archives/melpa/archive-contents"
                         "elpa/25/archives/melpa/archive-contents"
                         "elpa/26/archives/melpa/archive-contents"
                         "elpa/25/archives/melpa-stable/archive-contents"
                         "elpa/26/archives/melpa-stable/archive-contents"
                         "company-cache.el"
                         "company-cache.el~"
                         "company-statistics-cache.el"
                         "project-explorer-cache/*"
                         "projectile.cache"
                         ;; Xamarin
                         "assets/"
                         ;; Android
                         "build/"
                         "target/"
                         ;; iOS
                         "compile_commands.json"
                         ;; CSharp
                         "Resources/Resource.designer.cs"))
  (add-to-list 'ag-arguments "-U") ;; ignore .ignore files by default
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(use-package projectile
  :ensure t
  :commands (projectile-project-p
             projectile-project-root
             projectile-find-file
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-ag
             projectile-recentf
             +projectile-find)
  :diminish projectile-mode
  :init
  (setq projectile-file-exists-local-cache-expire (* 5 60))
  (when (eq system-type 'windows-nt)
    (setq projectile-indexing-method 'alien)
    (setq projectile-enable-caching t))
  :config
  (defvar +projectile-invalidate-cache-timer nil)
  (defun +projectile-schedule-invalidate-cache (orig-fun &rest args)
    "Schedule invalidating project cache when idle."
    (when +projectile-invalidate-cache-timer
      (cancel-timer +projectile-invalidate-cache-timer))
    (let ((result (apply orig-fun args))
          (+projectile-invalidate-cache-timer
           (run-with-idle-timer 3 nil
                                (lambda ()
                                  (call-interactively #'projectile-invalidate-cache)))))
      result))
  (advice-add 'alchemist-mix :around #'+projectile-schedule-invalidate-cache)

  (setq projectile-enable-caching t)
  (projectile-mode))

(use-package recentf
  :config
  (setq recentf-max-saved-items 800
        ;; https://www.reddit.com/r/emacs/comments/3g468d/stop_recent_files_showing_elpa_packages/
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; git contents
                              "/elpa/.*\\'"   ; package files
                              ".*\\.gz\\'"
                              "TAGS"
                              "company-statistics-cache.el"
                              "company-cache.el"
                              ".*-autoloads\\.el\\'"
                              ;; https://github.com/hlissner/.emacs.d/blob/master/core/core-editor.el
                              "^/tmp/"
                              "^/ssh:"
                              "/TAGS$"
                              "^/var/folders/.+$"
                              ;; John Wiegley
                              "~\\'"
                              "\\`out\\'"
                              "\\.log\\'"
                              "^/[^/]*:"
                              "\\.el\\.gz\\'"))
  (recentf-mode 1))

(use-package tramp
  :ensure nil
  :defer t
  :config
  (setq tramp-persistency-file-name (concat user-emacs-directory ".tramp"))
  (setq tramp-auto-save-directory "~/.backups"))

(use-package bookmark
  :ensure nil
  :commands (bookmark-set bookmark-save bookmark-jump bookmark-delete)
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file "~/Dropbox/.emacs_bookmarks"))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind ("C-x C-j" . dired-jump)
  :init
  (with-eval-after-load 'evil
    (evil-set-initial-state 'wdired-mode 'normal))

  (add-hook 'dired-mode-hook
            (lambda ()
              (with-eval-after-load 'evil
                (evil-define-key 'normal dired-mode-map
                  ;; FIXME: Figure out what to do with `dired-hide-subdir'.
                  ;; (kbd "TAB") 'dired-hide-subdir
                  ;; (kbd "<tab>") 'dired-hide-subdir
                  (kbd "TAB") 'dired-subtree-toggle
                  (kbd "<backtab>") 'dired-subtree-cycle
                  " " nil
                  "F" 'find-name-dired))))

  :config
  ;; `dired'
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)

  ;; Emacs 26 can create directories by default in wdired.
  (unless (< emacs-major-version 26)
    (setq wdired-create-parent-directories t))

  ;; Make Dired create directories if non-existing.
  ;; TODO: Rewrite this using new Advices.
  ;; https://stackoverflow.com/questions/12994164/allow-dired-do-copy-and-dired-do-rename-to-create-new-dir-on-the-fly
  (defadvice dired-mark-read-file-name
      (after rv:dired-create-dir-when-needed (prompt
                                              dir
                                              op-symbol
                                              arg files
                                              &optional default)
             activate)
    (when (member op-symbol '(copy move))
      (let ((directory-name (if (< 1 (length files))
                                ad-return-value
                              (file-name-directory ad-return-value))))
        (when (and (not (file-directory-p directory-name))
                   (y-or-n-p
                    (format
                     "directory %s doesn't exist, create it?" directory-name)))
          (make-directory directory-name t)))))

  (defun +dired-find-file ()
    "Like `find-file' but with `default-directory' set to the
one specified by listing header."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      (call-interactively #'find-file)))

  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map
      (kbd "C-x C-f") '+dired-find-file)))

(use-package dired-subtree
  :ensure t
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :config
  (setq dired-subtree-line-prefix " ")
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-collapse
  :ensure t
  :commands (dired-collapse-mode)
  :init
  (add-hook 'dired-mode-hook #'dired-collapse-mode))

(use-package dired-sidebar
  :load-path "~/.emacs.d/fork/dired-sidebar"
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure nil
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-subtree-line-prefix " .")
  (cond
   ((eq system-type 'darwin)
    (if (display-graphic-p)
        (setq dired-sidebar-theme 'icons)
      (setq dired-sidebar-theme 'nerd))
    (setq dired-sidebar-face '(:family "Helvetica" :height 140)))
   ((eq system-type 'windows-nt)
    (setq dired-sidebar-theme 'nerd)
    (setq dired-sidebar-face '(:family "Lucida Sans Unicode" :height 110)))
   (:default
    (setq dired-sidebar-theme 'nerd)
    (setq dired-sidebar-face '(:family "Arial" :height 140))))

  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)

  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :ensure t
    :commands (all-the-icons-dired-mode)))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer))
  :commands (ibuffer))

(use-package ibuffer-projectile
  :ensure t
  :commands (ibuffer-projectile-set-filter-groups
             ibuffer-projectile-generate-filter-groups)
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  :config
  (setq ibuffer-projectile-prefix "Project: "))

(provide 'jn-project)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-project.el ends here

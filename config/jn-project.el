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
        '((counsel-fzf . regexp-quote)
          (counsel-git-log . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-multi . ivy--regex-plus)
          (projectile-completing-read . ivy--regex-fuzzy)
          (t . ivy--regex-fuzzy)))

  (setq ivy-height-alist '((counsel-yank-pop . 15)
                           (counsel-evil-registers . 15)))

  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-selectable-prompt t)

  ;; Swap RET behavior.
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

  ;; C-M-j or C-RET to exit with current input.
  (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)

  (setq ivy-count-format "")
  (setq ivy-height 8)
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

  (setq counsel-async-filter-update-time 10000)
  (setq ivy-dynamic-exhibit-delay-ms 20)

  (setq counsel-git-cmd
        "git ls-files --exclude-standard --full-name --others --cached --")
  (setq counsel-rg-base-command
        "rg --max-columns 140 -i --no-heading --line-number --color never %s .")
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- ."))

(use-package counsel-tramp
  :ensure t
  :commands (counsel-tramp))

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
  (defun j|ag-set-ignore-list (&rest _)
    (cond
     ((memq major-mode '(lisp-interaction-mode emacs-lisp-mode))
      ;; (add-to-list 'ag-arguments "-U") ;; ignore .ignore files by default
      (setq-local ag-arguments '("-U" "--smart-case" "--stats")) ; -U is really slow.
      (setq-local ag-ignore-list
                  '("backups/*"
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
                    "projectile.cache")))
     (t
      (setq-local ag-arguments '("--smart-case" "--stats"))
      (setq-local ag-ignore-list
                  '(;; Binary Directories
                    "obj/"
                    "bin/"
                    ;; Xamarin
                    "assets/"
                    ;; Android
                    "build/"
                    "target/"
                    ;; iOS
                    "compile_commands.json"
                    ;; CSharp
                    "Resources/Resource.designer.cs")))))

  (advice-add 'ag/search :before #'j|ag-set-ignore-list)

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
             j|projectile-find)
  :diminish projectile-mode
  :init
  (setq projectile-file-exists-local-cache-expire (* 5 60))
  :config
  (setq projectile-enable-caching t
        projectile-dynamic-mode-line nil)
  (projectile-mode))

(use-package recentf
  :defer 1
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
  (defun j|ignore-if-remote (f &rest args)
    "Don't do anything if on a remote connection."
    (if (file-remote-p default-directory)
        nil
      (apply f args)))

  (advice-add 'projectile-project-p :around 'j|ignore-if-remote)
  (advice-add 'eglot--maybe-activate-editing-mode :around 'j|ignore-if-remote)
  (advice-add 'vc-state-refresh :around 'j|ignore-if-remote)
  (advice-add 'vc-refresh-state :around 'j|ignore-if-remote)

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
  (defun j|find-name-dired ()
    "Wrapper over `find-name-dired' and `fd-dired'.
Use `fd-dired' if fd is available. Fallback to `find-name-dired'."
    (interactive)
    (if (executable-find "fd")
        (call-interactively #'fd-dired)
      (call-interactively #'find-name-dired)))

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
                  "F" 'j|find-name-dired))))

  :config
  ;; `dired'
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)

  ;; Emacs 26 can create directories by default in wdired.
  (unless (< emacs-major-version 26)
    (setq wdired-create-parent-directories t))

  ;; Emacs 27 can create non-existent directories.
  (if (>= emacs-major-version 27)
      (setq dired-create-destination-dirs 'always)
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
            (make-directory directory-name t))))))

  (defun j|dired-find-file ()
    "Like `find-file' but with `default-directory' set to the
one specified by listing header."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      (call-interactively #'find-file)))

  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map
      (kbd "C-x C-f") 'j|dired-find-file)))

(use-package dired-subtree
  :ensure t
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :config
  (setq dired-subtree-line-prefix " ")
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-collapse
  :ensure t
  ;; `dired-collapse' uses `f' under the hood which collects all entries
  ;; under the directory. Some of these directories will have permission
  ;; errors which blocks `dired' from running.
  :if (not (eq window-system 'w32))
  :commands (dired-collapse-mode)
  :init
  (add-hook 'dired-mode-hook (lambda ()
                               (unless (file-remote-p default-directory)
                                 (dired-collapse-mode)))))

(use-package vscode-icon
  :load-path "~/.emacs.d/fork/vscode-icon-emacs/"
  :after 'dired-sidebar
  :ensure nil)

(use-package dired-sidebar
  :load-path "~/.emacs.d/fork/dired-sidebar"
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure nil
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-mode-line-format nil)

  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :ensure t
    :commands (all-the-icons-dired-mode)))

(use-package fd-dired
  :ensure t
  :commands (fd-dired))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer))
  :commands (ibuffer))

(use-package ibuffer-projectile
  :ensure t
  :commands (ibuffer-projectile-set-filter-groups
             ibuffer-projectile-generate-filter-groups)
  :init
  (defun j|ibuffer-projectile-run ()
    "Set up `ibuffer-projectile'."
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  (add-hook 'ibuffer-sidebar-mode-hook #'j|ibuffer-projectile-run)
  (add-hook 'ibuffer-hook #'j|ibuffer-projectile-run)
  :config
  (setq ibuffer-projectile-prefix "Project: "))

(use-package ibuffer-sidebar
  :load-path "~/.emacs.d/fork/ibuffer-sidebar"
  :ensure nil
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t))

(provide 'jn-project)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-project.el ends here

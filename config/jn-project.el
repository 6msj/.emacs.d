;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

;; (use-package selectrum
;;   :ensure t
;;   :config
;;   (selectrum-mode))

;; (use-package prescient
;;   :ensure t
;;   :config
;;   (prescient-persist-mode +1)
;;   (setq prescient-filter-method '(literal fuzzy)))

;; (use-package selectrum-prescient
;;   :ensure t
;;   :config
;;   (selectrum-prescient-mode))

;; (use-package consult
;;   :ensure t
;;   :config
;;   (setq consult-project-root-function #'projectile-project-root)
;;   (setq consult-async-min-input 0))

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
          (counsel-git-grep . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
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
  :config
  (defun counsel-FZF ()
    "Wrapper over `counsel-fzf'."
    (cond
     ((derived-mode-p 'dired-mode)
      (if (executable-find "fd")
          (setenv "FZF_DEFAULT_COMMAND" "fd .")
        (setenv "FZF_DEFAULT_COMMAND" "find ."))
      (counsel-fzf nil (dired-current-directory)
                   (format "FZF: (%s) " (dired-current-directory))))
     (:default
      (setenv "FZF_DEFAULT_COMMAND"
              "(git ls-files --recurse-submodules --exclude-standard --cached ||
        find . -maxdepth 2 -path \"*/\\.*\" -prune -o -print -o -type l -print |
           sed s/^..//) 2> /dev/null")
      (counsel-fzf))))

  (setq counsel-async-filter-update-time 10000)
  (setq ivy-dynamic-exhibit-delay-ms 20)

  (setq counsel-git-cmd
        "git ls-files --exclude-standard --full-name --others --cached --")
  (setq counsel-rg-base-command
        "rg --max-columns 180 -i --no-heading --line-number --color never %s .")
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- .")

  (when WINDOWS-P
    ;; Download a copy of RG from https://github.com/BurntSushi/ripgrep/releases.
    (defvar counsel-rg-win-exe
      (format
       "%s/Documents/ripgrep-12.1.1-x86_64-pc-windows-msvc/rg.exe"
       (user-windows-directory)))
    (setq
     counsel-rg-base-command
     (split-string
      (concat
       counsel-rg-win-exe
       " -M 240 --with-filename --no-heading --line-number --color never %s --path-separator / .")))))

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

(use-package projectile
  :ensure t
  :commands (projectile-project-p
             projectile-project-root
             projectile-find-file
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-ag
             projectile-recentf
             j-projectile-find)
  :diminish projectile-mode
  :init
  (setq projectile-file-exists-local-cache-expire (* 5 60))
  (setq projectile-switch-project-action 'j-search)
  :config
  (setq projectile-enable-caching t
        projectile-dynamic-mode-line nil)
  (projectile-mode))

(use-package recentf
  :defer 1
  :ensure nil
  :config
  (setq recentf-max-saved-items 2000
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
                              "\\.el\\.gz\\'")
        recentf-menu-filter 'recentf-sort-ascending)

  (defun recentf-track-switch-to-buffer (&rest _args)
    "Update `recentf' list with new buffer."
    (and buffer-file-name
         (recentf-add-file buffer-file-name)))

  (advice-add 'switch-to-buffer :after 'recentf-track-switch-to-buffer)
  (advice-add 'find-file :after 'recentf-track-switch-to-buffer)
  (recentf-mode 1))

(use-package tramp
  :ensure nil
  :defer t
  :config
  (defun j-ignore-if-remote (f &rest args)
    "Don't do anything if on a remote connection."
    (if (file-remote-p default-directory)
        nil
      (apply f args)))

  (advice-add 'projectile-project-p :around 'j-ignore-if-remote)
  (advice-add 'eglot--maybe-activate-editing-mode :around 'j-ignore-if-remote)
  (advice-add 'vc-state-refresh :around 'j-ignore-if-remote)
  (advice-add 'vc-refresh-state :around 'j-ignore-if-remote)

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
  ;; NOTE for OSX Catalina:
  ;; https://emacs.stackexchange.com/questions/53026/how-to-restore-file-system-access-in-macos-catalina
  ;; Restore disk access by:o
  ;; Open General Settings -> Security & Privacy -> Privacy
  ;; Select Full Disk Access in the left pane, then click + and add /usr/bin/ruby.
  :config
  ;; `dired'
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-create-destination-dirs 'always)

  ;; TODO
  ;; `find-grep-dired'

  (defun j-find-name-dired ()
    "Wrapper over `find-name-dired' and `fd-dired'.
Use `fd-dired' if fd is available. Fallback to `find-name-dired'."
    (interactive)
    (if (executable-find "fd")
        (call-interactively #'fd-dired)
      (call-interactively #'find-name-dired)))

  (defun j-dired-find-file ()
    "Like `find-file' but with `default-directory' set to the
one specified by listing header."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      (call-interactively #'find-file)))

  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map
      (kbd "C-x C-f") 'j-dired-find-file
      "F" 'j-find-name-dired)))

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
                               (unless (or
                                        YT-P
                                        (file-remote-p default-directory))
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

  (setq dired-sidebar-subtree-line-prefix " ")
  (if LINUX-P
      (setq dired-sidebar-theme 'none)
    (setq dired-sidebar-theme 'vscode))
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-mode-line-format nil))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer))
  :commands (ibuffer))

(use-package ibuffer-projectile
  :ensure t
  :commands (ibuffer-projectile-set-filter-groups
             ibuffer-projectile-generate-filter-groups)
  :init
  (defun j-ibuffer-projectile-run ()
    "Set up `ibuffer-projectile'."
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  (add-hook 'ibuffer-sidebar-mode-hook #'j-ibuffer-projectile-run)
  (add-hook 'ibuffer-hook #'j-ibuffer-projectile-run)
  :config
  (setq ibuffer-projectile-prefix "Project: "))

(use-package ibuffer-sidebar
  :load-path "~/.emacs.d/fork/ibuffer-sidebar"
  :ensure nil
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t))

(use-package terminal-here
  :ensure t
  :commands (terminal-here))

(provide 'jn-project)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-project.el ends here

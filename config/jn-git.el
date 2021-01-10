;;;; -*- lexical-binding: t; -*-
(require 'jn-functions)

(use-package transient
  :ensure t
  :config
  (define-key transient-base-map (kbd "<escape>") 'transient-quit-all)
  (define-key transient-sticky-map (kbd "<escape>")'transient-quit-seq)
  (transient-bind-q-to-quit))

(use-package magit
  :ensure t
  :commands (magit-toplevel
             magit-status
             magit-blame-addition
             magit-log-other
             magit-find-file
             magit-find-file-other-window)
  :init
  (when YT-P
    (require 'magit))
  :config
  (setq magit-clone-default-directory "~/Code")
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-section-visibility-indicator nil)

  (transient-replace-suffix 'magit-branch 'magit-checkout
    '("b" "checkout" magit-branch-or-checkout))

  (setq magit-diff-refine-hunk 'all)

  (defun j-magit-git-amend-reset-commit-date ()
    "Amend commit and reset commit date."
    (interactive)
    (magit-run-git-async "commit" "--amend" "--no-edit" "--date=now"))

  (transient-append-suffix 'magit-commit "S"
    '("d" "Reset Commit Date" j-magit-git-amend-reset-commit-date))

  (defun j-magit-git-submodule-update--init--recursive ()
    "Run $ git submodule update --init --recursive."
    (interactive)
    (magit-run-git-async "submodule" "update" "--init" "--recursive"))

  (transient-append-suffix 'magit-submodule "k"
    '("U" "Update Init Recursive" j-magit-git-submodule-update--init--recursive))

  (setq magit-bury-buffer-function 'magit-mode-quit-window)

  (add-hook 'git-rebase-mode-hook
            (lambda ()
              (local-set-key (kbd "g") nil)))
  (with-eval-after-load 'evil
    (evil-define-key 'normal git-rebase-mode-map
      (kbd "gr") 'revert-buffer
      (kbd "g") nil)
    (evil-define-key 'normal magit-refs-mode-map
      (kbd "`") 'magit-process-buffer
      (kbd "$") 'evil-end-of-line)
    (evil-define-key 'normal magit-process-mode-map
      "0" 'evil-digit-argument-or-evil-beginning-of-line
      (kbd "`") 'quit-window
      (kbd "$") 'evil-end-of-line)
    (evil-define-key 'normal magit-revision-mode-map
      (kbd "q") 'magit-mode-bury-buffer
      (kbd "$") 'evil-end-of-line)
    (evil-define-key 'normal magit-log-mode-map
      (kbd "q") 'magit-log-bury-buffer
      (kbd "`") 'magit-process-buffer
      (kbd "~") 'magit-diff-default-context
      (kbd "0") 'evil-digit-argument-or-evil-beginning-of-line
      (kbd "$") 'evil-end-of-line)
    (evil-define-key 'normal magit-status-mode-map
      (kbd "q") 'magit-mode-bury-buffer
      (kbd "`") 'magit-process-buffer
      (kbd "~") 'magit-diff-default-context
      (kbd "0") 'evil-digit-argument-or-evil-beginning-of-line
      (kbd "$") 'evil-end-of-line
      (kbd "Q") 'delete-window)
    (evil-define-key 'normal magit-repolist-mode-map
      (kbd "q") 'magit-mode-bury-buffer
      (kbd "Q") 'delete-window
      (kbd "RET") 'magit-repolist-status
      (kbd "gr") 'magit-list-repositories))

  ;; Save buffers automatically instead of asking.
  (setq magit-save-repository-buffers 'dontask)

  (setq magit-repository-directories '(("~/Code" . 3)
                                       ("~/.emacs.d" . 3)
                                       ("~/.vim" . 1)
                                       ("~/.zsh" . 1)))
  (setq magit-refresh-status-buffer nil)

  (defadvice magit-show-commit (around dont-select-commit-window activate)
    "magit-show-commit selects the window it opens unless magit-display-buffer-noselect is set.
Setting magit-display-buffer-noselect changes the selection logic for other parts of magit though.
Instead, advise magit-show-commit by setting magit-show-commit to t
before calling magit-show-commit and set it back to nil afterwards."
    (setq magit-display-buffer-noselect t)
    (setq ad-return-value ad-do-it)
    (setq magit-display-buffer-noselect nil))

  ;; https://github.com/magit/magit/issues/2541 (tweaked)
  (defun j-magit-display-buffer-function (buffer)
    "Display buffer in another window if single window or special magit mode."
    (cond
     ((eq (count-windows) 1)
      (display-buffer buffer nil)) ;; Display in another window.
     ((with-current-buffer buffer
        (eq major-mode 'magit-status-mode))
      (display-buffer buffer '(display-buffer-same-window)))
     (:default
      (magit-display-buffer-traditional buffer))))

  (setq magit-display-buffer-function #'j-magit-display-buffer-function)

  (setq magit-bisect-show-graph t)

  (defun j-magit-submodule-remove (path &optional leave-in-work-tree)
    "Remove the submodule at PATH.

https://stackoverflow.com/questions/1260748/how-do-i-remove-a-submodule"
    (interactive
     (list (magit-completing-read "Remove module" (magit-list-module-paths)
                                  nil t nil nil nil)))
    (magit-with-toplevel
     ;; 0. mv a/submodule a/submodule_tmp
     (shell-command (format "mv %s %s_tmp" path path))

     ;; 1. git submodule deinit -f -- a/submodule
     (magit-run-git "submodule" "deinit" "-f" "--" path)

     ;; 2. rm -rf .git/modules/a/submodule
     (shell-command (format "rm -rf .git/modules/%s" path))

     (if (not leave-in-work-tree)
         ;; 3. git rm -f a/submodule
         (magit-run-git "rm" "-f" path)
       ;; # If you want to leave it in your working tree and have done step 0.
       ;; 3b. git rm --cached a/submodule
       ;; 3b. mv a/submodule_tmp a/submodule
       (magit-run-git "rm" "--cached" path)
       (shell-command-to-string (format "mv %s_tmp %s" path path)))))

  (transient-append-suffix 'magit-submodule "k"
    '("X" "Remove Completely" j-magit-submodule-remove))

  (defun j-magit-branch-and-checkout-and-squash-all-commits (branch
                                                             start-point
                                                             &optional args)
    "Create and checkout BRANCH at branch or revision START-POINT and squash all\
 commits on the new branch.

Check out `magit-branch-and-checkout'.

https://stackoverflow.com/questions/1657017/how-to-squash-all-git-commits-into-one"
    (interactive (magit-branch-read-args
                  "Create and checkout branch squashing all commits"))
    (if (string-match-p "^stash@{[0-9]+}$" start-point)
        (magit-run-git "stash" "branch" branch start-point)
      (magit-call-git "checkout" args "-b" branch start-point)
      (magit-branch-maybe-adjust-upstream branch start-point)
      (shell-command "git reset $(git commit-tree HEAD^{tree} -m \"2017\")")
      (magit-refresh))))

(use-package smerge-mode
  :ensure nil
  :commands (smerge-start-session)
  :config
  (when (< emacs-major-version 26)
    (defalias 'smerge-keep-upper 'smerge-keep-mine)
    (defalias 'smerge-keep-lower 'smerge-keep-other)))

(use-package vc-hgcmd
  :ensure t
  :defer t
  :init
  (push 'Hgcmd vc-handled-backends)
  :config
  (setq vc-hgcmd-dir-show-shelve t))

(use-package hg-histedit
  :ensure nil
  :load-path "~/.emacs.d/fork/hg-histedit/"
  :defer 5
  :config
  (hg-histedit-setup))

(use-package vc-defer
  :ensure t
  :diminish vc-defer-mode
  :defer 5
  :config
  (add-to-list 'vc-defer-backends 'Hg)
  (add-to-list 'vc-defer-backends 'Fig)
  (add-to-list 'vc-defer-backends 'Hgcmd)
  (vc-defer-mode))

(provide 'jn-git)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-git.el ends here

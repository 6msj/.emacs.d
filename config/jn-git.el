;;;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :commands (magit-toplevel
             magit-status
             magit-blame-addition
             magit-log-other
             magit-find-file
             magit-find-file-other-window)
  :config
  (setq magit-clone-default-directory "~/Code")
  (setq magit-ediff-dwim-show-on-hunks t)

  (setq magit-section-visibility-indicator nil)

  (magit-remove-popup-key 'magit-branch-popup :action ?b)
  (magit-define-popup-action 'magit-branch-popup
    ?b "Checkout" 'magit-branch-or-checkout
    'magit-branch t)

  (setq magit-rebase-arguments '("--autostash"))
  (setq magit-diff-refine-hunk 'all)

  (defun j|magit-git-amend-reset-commit-date ()
    "Amend commit and reset commit date."
    (interactive)
    (magit-run-git-async "commit" "--amend" "--no-edit" "--date=now"))

  (magit-define-popup-action 'magit-commit-popup
    ?d "Reset Date" #'j|magit-git-amend-reset-commit-date)

  (defun j|magit-git-submodule-update--init--recursive ()
    "Run $ git submodule update --init --recursive."
    (interactive)
    (magit-run-git-async "submodule" "update" "--init" "--recursive"))

  (magit-define-popup-action 'magit-submodule-popup ?U
    "Update Init Recursive" #'j|magit-git-submodule-update--init--recursive)

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
  (defun j|magit-display-buffer-function (buffer)
    "Display buffer in another window if single window or special magit mode."
    (cond
     ((eq (count-windows) 1)
      (display-buffer buffer nil)) ;; Display in another window.
     ((with-current-buffer buffer
        (eq major-mode 'magit-status-mode))
      (display-buffer buffer '(display-buffer-same-window)))
     (:default
      (magit-display-buffer-traditional buffer))))

  (setq magit-display-buffer-function #'j|magit-display-buffer-function)

  ;; Add rebase argument to pull
  ;; https://github.com/magit/magit/issues/2597
  (magit-define-popup-switch 'magit-pull-popup ?R "Rebase" "--rebase")

  (setq magit-bisect-show-graph t)

  (defun j|magit-submodule-remove (path &optional leave-in-work-tree)
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

  (magit-define-popup-action
    'magit-submodule-popup ?x "Remove Completely" #'j|magit-submodule-remove)

  (defun j|magit-branch-and-checkout-and-squash-all-commits (branch
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
      (magit-refresh)))

  ;;; Improve Bisect

  ;; Clear bisect variables on these hooks
  (add-hook 'magit-status-mode-hook #'j|magit-bisect-reset-state)
  (add-hook 'magit-log-mode-hook #'j|magit-bisect-reset-state)

  (defvar j|magit-bad-revision nil "The bad revision to use in git bisect")
  (defvar j|magit-good-revision nil "The good revision to use in git bisect")

  (defun j|magit-bisect-mark-commit (revision good)
    "Mark a commit for bisecting.

REVISION: The commit to mark.
GOOD: If this is true, commit is marked as good, else bad."
    (if good
        (setq j|magit-good-revision revision)
      (setq j|magit-bad-revision revision))
    (j|magit-bisect-remove-popup-actions)
    (j|magit-add-bisect-mark-popup-actions))

  (defun j|magit-bisect-mark-bad-commit (revision)
    "Mark REVISION as bad for bisecting."
    (interactive (list (magit-read-other-branch-or-commit "Mark bad commit")))
    (j|magit-bisect-mark-commit revision nil))

  (defun j|magit-bisect-mark-good-commit (revision)
    "Mark REVISION as good for bisecting."
    (interactive (list (magit-read-other-branch-or-commit "Mark good commit")))
    (j|magit-bisect-mark-commit revision t))

  (defun j|magit-bisect-reset-state ()
    "Reset bad and good git bisect variables."
    (interactive)
    (j|magit-bisect-remove-popup-actions)
    (setq j|magit-good-revision nil)
    (setq j|magit-bad-revision nil)
    (j|magit-add-bisect-mark-popup-actions))

  (defun j|magit-bisect-remove-popup-actions ()
    "Remove popup actions to select bad and good commits."
    (magit-remove-popup-key 'magit-bisect-popup :action ?X)
    (magit-remove-popup-key 'magit-bisect-popup :action ?Z)
    (when (and j|magit-good-revision j|magit-bad-revision)
      (magit-remove-popup-key 'magit-bisect-popup :action ?C)))

  (defun j|magit-add-bisect-mark-popup-actions ()
    "Add popup actions to select bad and good commits."
    (let ((bad-str (if j|magit-bad-revision
                       (concat "Marked bad commit: " j|magit-bad-revision)
                     "Mark bad commit"))
          (good-str (if j|magit-good-revision
                        (concat "Marked good commit: " j|magit-good-revision)
                      "Mark good commit")))
      (magit-define-popup-action
        'magit-bisect-popup ?X bad-str #'j|magit-bisect-mark-bad-commit)
      (magit-define-popup-action
        'magit-bisect-popup ?Z good-str #'j|magit-bisect-mark-good-commit)
      (when (and j|magit-good-revision j|magit-bad-revision)
        (magit-define-popup-action 'magit-bisect-popup
          ?C
          (format "Start: Bad commit: %s - Good Commit: %s"
                  j|magit-bad-revision j|magit-good-revision)
          #'j|magit-bisect-with-good-bad-commits))))

  (defun j|magit-bisect-with-good-bad-commits ()
    "Run magit-bisect with `j|magit-bad-revision' and `j|magit-good-revision'"
    (interactive)
    (magit-bisect-start j|magit-bad-revision j|magit-good-revision))

  (j|magit-add-bisect-mark-popup-actions))

(use-package smerge-mode
  :ensure nil
  :commands (smerge-start-session)
  :config
  (when (< emacs-major-version 26)
    (defalias 'smerge-keep-upper 'smerge-keep-mine)
    (defalias 'smerge-keep-lower 'smerge-keep-other)))

(provide 'jn-git)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-git.el ends here

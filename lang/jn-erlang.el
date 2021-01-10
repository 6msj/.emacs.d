;;;; -*- lexical-binding: t; -*-

(use-package erlang
  :ensure t
  :mode
  ("\\.erl\\'" . erlang-mode)
  ("\\.hrl\\'" . erlang-mode)
  ("\\.xrl\\'" . erlang-mode)
  :init
  (defun j-setup-inferior-erlang ()
    "Launch erlang shell in background"
    (when (null (inferior-erlang-running-p))
      (save-selected-window
        (inferior-erlang)
        (quit-window))))
  (add-hook 'erlang-mode-hook #'j-setup-inferior-erlang)
  :config
  ;; http://erlang.org/pipermail/erlang-questions/2003-June/009103.html
  (setq hs-special-modes-alist
        (cons '(erlang-mode
                "^\\([a-z][a-zA-Z0-9_]*\\|'[^\n']*[^\\]'\\)\\s *(" nil "%"
                erlang-end-of-clause) hs-special-modes-alist))
  (setq tab-width 4)
  (setq erlang-indent-level 4)
  (erlang-font-lock-level-4))

(use-package distel
  :ensure nil
  :commands (erlang-extended-mode)
  :load-path "~/.emacs.d/fork/distel/elisp"
  :init
  (add-hook 'erlang-mode-hook #'erlang-extended-mode)
  :config
  ;; http://bob.ippoli.to/archives/2007/03/16/distel-and-erlang-mode-for-emacs-on-mac-os-x/
  ;; prevent annoying hang-on-compile
  (defvar inferior-erlang-prompt-timeout t)
  ;; default node name to emacs@localhost
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; tell distel to default to that node
  (setq erl-nodename-cache
        (make-symbol
         (concat
          "emacs@"
          ;; Mac OS X uses "name.local" instead of "name", this should work
          ;; pretty much anywhere without having to muck with NetInfo
          ;; ... but I only tested it on Mac OS X.
          (car (split-string (shell-command-to-string "hostname"))))))

  (evil-define-key 'normal erlang-mode-map
    (kbd "K") 'erl-find-doc-under-point
    (kbd "gz") 'erl-ie-show-session
    (kbd "M-?") 'erl-who-calls))

(use-package company-distel
  :ensure nil
  :commands (company-distel)
  :load-path "~/.emacs.d/fork/company-distel/"
  :init
  (add-hook 'erlang-mode-hook
            (lambda ()
              (j-company-push-backend 'company-distel t))))

;;;###autoload
(defun j-erlang-mode ()
  "Bootstrap `jn-erlang'."
  ;; (setq auto-mode-alist (rassq-delete-all #'j-erlang-mode auto-mode-alist))
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) 'j-erlang-mode)
      (setf (cdr alist) 'erlang-mode)))
  (erlang-mode))

(provide 'jn-erlang)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-erlang.el ends here

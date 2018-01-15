;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

(use-package elixir-mode
  :ensure t
  :mode
  ("\\.elixir\\'" . elixir-mode)
  ("\\.ex\\'" . elixir-mode)
  ("\\.exs\\'" . elixir-mode)
  :init
  (add-hook 'elixir-mode-hook
            (lambda ()
              (setq tab-width 2)
              (with-eval-after-load 'evil
                (setq evil-shift-width 2))))
  :config
  (with-eval-after-load 'smartparens
    (require 'smartparens-elixir)))

(use-package alchemist
  :ensure t
  :commands alchemist-mode
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up `alchemist' if Elixir.
              (when (string-equal (file-name-extension buffer-file-name) "eex")
                (alchemist-mode))))

  (add-hook 'elixir-mode-hook #'alchemist-mode)
  (add-hook 'alchemist-mode-hook
            (lambda ()
              (+company-merge-backends)) t)
  :config
  (setq alchemist-test-ask-about-save nil)
  (setq alchemist-goto-elixir-source-dir "~/.source/elixir/elixir-1.3.4")
  ;; (setq alchemist-goto-elixir-source-dir "~/.source/elixir/elixir-1.0.0")
  (setq alchemist-goto-erlang-source-dir "~/.source/erlang/otp_src_19.2")

  ;; Erlang synergy
  (defun +elixir-erlang-pop-back ()
    "Pop back definition function for Erlang mode."
    (interactive)
    (if (ring-empty-p erl-find-history-ring)
        (alchemist-goto-jump-back)
      (erl-find-source-unwind)))

  (defun +alchemist-erlang-mode-hook ()
    (with-eval-after-load 'evil
      (evil-define-key 'normal erlang-mode-map
        (kbd "M-,") #'+elixir-erlang-pop-back)))

  (add-hook 'erlang-mode-hook '+alchemist-erlang-mode-hook)

  (with-eval-after-load 'evil
    (evil-define-key 'normal alchemist-mode-map
      (kbd "gD") 'alchemist-goto-list-symbol-definitions
      (kbd "gt") 'alchemist-project-find-test
      (kbd "go") 'alchemist-project-toggle-file-and-tests
      (kbd "gO") 'alchemist-project-toggle-file-and-tests-other-window)))

(use-package flycheck-mix
  :ensure t
  :commands flycheck-mix-setup
  :init
  (defun +elixir-flycheck-initialize ()
    "Initialize flycheck mix."
    (eval-after-load 'flycheck
      (lambda ()
        (flycheck-mix-setup)))
    (remove-hook 'elixir-mode-hook #'+elixir-flycheck-initialize))
  (add-hook 'elixir-mode-hook #'+elixir-flycheck-initialize))

(use-package elixir-yasnippets
  :ensure t
  :config
  (elixir-snippets-initialize))

;;;###autoload
(defun +elixir-mode ()
  "Bootstrap `jn-elixir'."
  (setq auto-mode-alist (rassq-delete-all #'+elixir-mode auto-mode-alist))
  (elixir-mode))

(provide 'jn-elixir)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-elixir.el ends here

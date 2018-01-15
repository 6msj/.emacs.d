;;;; -*- lexical-binding: t; -*-

(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'" . swift-mode)
  :config
  (define-key swift-repl-mode-map [(shift return)] 'evil-jump-forward))

(use-package company-sourcekit
  :ensure t
  :mode ("\\.swift\\'" . swift-mode)
  :init
  (add-hook 'swift-mode-hook #'+company-setup-sourcekit)
  (defun +company-setup-sourcekit ()
    "Set up `company-sourcekit'."
    (setq-local company-backends
                '((company-sourcekit)
                  (company-dabbrev-code company-keywords :with company-yasnippet))))
  :config
  (defun +company-sourcekit--prefix (f &rest args)
    "Only use `company-sourcekit' for dot completions."
    (when (save-excursion
            (catch 'found-period
              (while (not (eq (point) (line-beginning-position)))
                (forward-char -1)
                (when (looking-at-p "\\.")
                  (throw 'found-period t)))))
      (apply f args)))

  (advice-add 'company-sourcekit--prefix :around '+company-sourcekit--prefix)
  ;; (advice-remove 'company-sourcekit--prefix '+company-sourcekit--prefix)
  (setq company-sourcekit-verbose nil
        sourcekit-verbose nil))

(use-package flycheck-swift
  :ensure t
  :config
  (setq flycheck-swift-executable
        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/swiftc")
  (setq flycheck-swift-sdk-path
        "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk")
  (setq flycheck-swift-target "x86_64-apple-ios11.2")
  (with-eval-after-load 'flycheck
    (flycheck-swift-setup)))

(use-package flycheck-swiftlint
  :load-path "~/.emacs.d/fork/flycheck-swiftlint"
  :ensure nil
  :config
  (with-eval-after-load 'flycheck
    (flycheck-swiftlint-setup)))

;;;###autoload
(defun +swift-mode ()
  "Bootstrap `jn-swift'."
  (setq auto-mode-alist (rassq-delete-all #'+swift-mode auto-mode-alist))
  (swift-mode))

(provide 'jn-swift)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-swift.el ends here

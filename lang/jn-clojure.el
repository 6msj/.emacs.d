;;;; -*- lexical-binding: t; -*-

(require 'jn-dependencies)

(use-package clojure-mode
  :ensure t
  :mode
  ("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
  ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)
  ("\\.clj\\'" . clojure-mode)
  ("\\.edn\\'" . clojure-mode)
  ("\\.cljs\\'" . clojurescript-mode)
  ("\\.cljc\\'" . clojurec-mode))

(use-package cider
  :ensure t
  :commands (cider cider-mode cider-jack-in)
  :diminish cider-auto-test-mode
  :init
  (defun j|cider-setup ()
    (cider-mode)
    (clj-refactor-mode)
    (j|company-merge-backends))
  (add-hook 'clojurescript-mode-hook #'j|cider-setup)
  (add-hook 'clojure-mode-hook #'j|cider-setup)
  (add-hook 'cider-repl-mode-hook #'j|cider-setup)
  :config
  (setq cider-mode-line
        '(:eval
          (let ((info (cider--modeline-info)))
            (if (string-equal info "not connected")
                " !Cider"
              (format " Cider[%s]" info)))))

  ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
  (defun cider-figwheel-repl ()
    "Start figwheel and cljs-repl in cider.
Use this in conjunction with `cider-jack-in'.

Unless you remember what you're doing, use `cider-jack-in-clojurescript'
along with `cider-cljs-lein-repl' instead."
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl-buffer)
      (goto-char (point-max))
      (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
      (cider-repl-return)))

  ;; This can be used in conjunction with `cider-jack-in-clojurescript'.
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

  ;; Call cider-run immediately after cider-jack-in connects.
  (add-hook 'cider-connected-hook #'cider-run)

  (evil-define-key 'normal cider-test-report-mode-map
    (kbd "M-.") #'cider-test-jump)

  ;; TODO:
  ;; https://github.com/clojure-emacs/cider/issues/784
  ;; if ns has the string 'test' in it, return the ns
  ;; otherwise do some magic here
  ;; probably need to account for luminus and how they set up the test namespaces
  ;; or maybe just chance luminus namespace
  (defun j|cider-determine-test-ns (ns)
    "Figuring out which namespace to test given the current namespace."
    (if (locate-dominating-file default-directory ".luminus")
        ns
      (cider-test-default-test-ns-fn ns)))
  (setq cider-test-infer-test-ns #'j|cider-determine-test-ns)

  (cider-auto-test-mode)
  (setq cider-test-show-report-on-success nil)
  ;; attempt to automatically look up symbol first
  (setq cider-prompt-for-symbol nil)
  ;; use shift return to get a new line in repl
  (define-key cider-repl-mode-map (kbd "C-j") nil)
  (define-key cider-repl-mode-map [(shift return)] 'cider-repl-newline-and-indent)

  (setq nrepl-log-messages t
        nrepl-prompt-to-kill-server-buffer-on-quit nil
        nrepl-hide-special-buffers t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package flycheck-clojure
  :ensure t
  :after cider
  :config
  (flycheck-clojure-setup))

(use-package cider-eval-sexp-fu
  :ensure t
  :after eval-sexp-fu)

(use-package clj-refactor
  :ensure t
  :defer t
  :diminish clj-refactor-mode)

;;;###autoload
(defun j|clojure-mode ()
  "Bootstrap `jn-clojure'."
  (setq auto-mode-alist
        (rassq-delete-all #'j|clojure-mode auto-mode-alist))
  (clojure-mode))

;;;###autoload
(defun j|clojurescript-mode ()
  "Bootstrap `jn-clojure'."
  (setq auto-mode-alist
        (rassq-delete-all #'j|clojurescript-mode auto-mode-alist))
  (clojurescript-mode))

;;;###autoload
(defun j|clojurec-mode ()
  "Bootstrap `jn-clojure'."
  (setq auto-mode-alist
        (rassq-delete-all #'j|clojurec-mode auto-mode-alist))
  (clojurec-mode))

(provide 'jn-clojure)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
;;; jn-clojure.el ends here

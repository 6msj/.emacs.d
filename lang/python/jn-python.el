;;;; -*- lexical-binding: t; -*-

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (with-eval-after-load 'python
    ;; https://github.com/jorgenschaefer/elpy/issues/887#issuecomment-281521965
    (defun python-shell-completion-native-try ()
      "Return non-nil if can trigger native completion."
      (let ((python-shell-completion-native-enable t)
            (python-shell-completion-native-output-timeout
             python-shell-completion-native-try-output-timeout))
        (python-shell-completion-native-get-completions
         (get-buffer-process (current-buffer))
         nil "_"))))

  (defun +setup-inferior-python ()
    "Launch python shell in background."
    (when (and (eq system-type 'darwin)
               (executable-find "python2"))
      (setq python-shell-interpreter "python2"))
    (if-let (python-shell-buffer (python-shell-get-buffer))
        (save-selected-window
          (switch-to-buffer-other-window
           python-shell-buffer)
          (quit-window))
      (run-python (python-shell-calculate-command) nil nil)))

  (add-hook 'python-mode-hook #'+setup-inferior-python)
  :config
  ;; pdb setup
  (when (eq system-type 'darwin)
    (setq pdb-path '/usr/lib/python2.7/pdb.py)
    (setq gud-pdb-command-name (symbol-name pdb-path))

    (defadvice pdb (before gud-query-cmdline activate)
      "Provide a better default command line when called interactively."
      (interactive
       (list (gud-query-cmdline pdb-path
                                (file-name-nondirectory buffer-file-name))))))

  ;; https://github.com/emacsmirror/python-mode - see troubleshooting
  ;; https://bugs.launchpad.net/python-mode/+bug/963253
  ;; http://pswinkels.blogspot.com/2010/04/debugging-python-code-from-within-emacs.html
  (when (eq system-type 'windows-nt)
    (setq windows-python-pdb-path "c:/python27/python -i c:/python27/Lib/pdb.py")
    (setq pdb-path 'C:/Python27/Lib/pdb.py)
    (setq gud-pdb-command-name (symbol-name pdb-path))
    (setq gud-pdb-command-name windows-python-pdb-path)

    (defun +set-pdb-command-path ()
      (setq gud-pdb-command-name
            (concat windows-python-pdb-path " " buffer-file-name)))

    ;; Everytime we enter a new python buffer, set the command path
    ;; to include the buffer filename.
    (add-hook 'python-mode-hook '+set-pdb-command-path)))

(use-package anaconda-mode
  :ensure t
  :commands (anaconda-mode anaconda-eldoc-mode)
  :init
  (add-hook #'python-mode-hook (lambda ()
                                 (anaconda-mode)
                                 (anaconda-eldoc-mode)))
  :config
  (setq anaconda-mode-installation-directory
        (format "%sservers/%s/anaconda/"
                user-emacs-directory emacs-major-version))

  (with-eval-after-load 'evil
    (evil-define-key 'normal anaconda-mode-map
      (kbd "gv") 'anaconda-mode-find-assignments)))

(use-package company-anaconda
  :ensure t
  :commands (company-anaconda)
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (+company-push-backend
               '(company-anaconda :with company-capf) t))))

;;;###autoload
(defun +python-mode ()
  "Bootstrap `jn-python'."
  (setq auto-mode-alist (rassq-delete-all #'+python-mode auto-mode-alist))
  (python-mode))

(provide 'jn-python)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-python.el ends here

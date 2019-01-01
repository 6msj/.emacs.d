;;;; -*- lexical-binding: t; -*-
(require 'jn-functions)

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (with-eval-after-load 'lispy
    (with-eval-after-load 'evil
      (evil-define-key 'normal python-mode-map (kbd "C-j") 'lispy-eval-and-insert)))

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

  (defun j|python-setup-inferior-shell ()
    "Launch python shell in background."
    ;; Check for `buffer-file-name' because evaluating python code
    ;; (e.g. `python-shell-send-buffer') creates a temporary buffer
    ;; which retriggers this function which will then hide the *Python*
    ;; process buffer.
    ;; For temporary buffers with `python-mode' enabled, we don't try to
    ;; set up the interpreter again.
    (when buffer-file-name
      (when (and (eq system-type 'darwin)
                 (executable-find "python2"))
        ;; https://github.com/jorgenschaefer/elpy/issues/887#issuecomment-281521965
        ;; The default /usr/bin/python interpreter for OSX has readline problems.
        (setq python-shell-interpreter "python2"))
      ;; Launch `python' process.
      (save-selected-window
        (run-python (python-shell-calculate-command) nil nil))))

  (add-hook 'python-mode-hook (lambda ()
                                "Set fill column to pycodestyle's 79."
                                (setq-local fill-column 79)))
  (add-hook 'python-mode-hook #'j|python-setup-inferior-shell)
  :config
  (defvar pdb-path (intern "python3 -m pdb")
    "Path to pdb. Must be a symbol to work with `pdb'.")

  (when (eq system-type 'darwin)
    (when (executable-find "python2")
      (setq pdb-path '/usr/lib/python2.7/pdb.py))
    (setq gud-pdb-command-name (symbol-name pdb-path)))

  (defadvice pdb (before gud-query-cmdline activate)
    "Add a filename argument to `pdb'.

Before: pdb
 After: pdb file.py"
    (interactive
     (list (gud-query-cmdline pdb-path
                              (file-name-nondirectory buffer-file-name)))))

  ;; https://github.com/emacsmirror/python-mode - see troubleshooting
  ;; https://bugs.launchpad.net/python-mode/+bug/963253
  ;; http://pswinkels.blogspot.com/2010/04/debugging-python-code-from-within-emacs.html
  (when (eq system-type 'windows-nt)
    (setq windows-python-pdb-path "c:/python27/python -i c:/python27/Lib/pdb.py")
    (setq pdb-path 'C:/Python27/Lib/pdb.py)
    (setq gud-pdb-command-name windows-python-pdb-path)

    (defun j|python-mode-set-pdb-command ()
      (setq-local gud-pdb-command-name
                  (concat windows-python-pdb-path " " buffer-file-name)))

    ;; Everytime we enter a new python buffer, set the command path
    ;; to include the buffer filename.
    (add-hook 'python-mode-hook 'j|python-mode-set-pdb-command)))

(use-package anaconda-mode
  :ensure t
  :commands (anaconda-mode anaconda-eldoc-mode)
  :init
  (add-hook #'python-mode-hook
            (lambda ()
              ;; Don't turn `anaconda-mode' on if `eglot' will be available.
              (unless (executable-find "pyls")
                (anaconda-mode)
                (anaconda-eldoc-mode))))
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
              (unless (executable-find "pyls")
                (j|company-push-backend
                 '(company-anaconda :with company-capf) t)))))

(use-package pydoc
  :ensure t
  :after python
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal python-mode-map (kbd "g?") 'pydoc)))

;;;###autoload
(defun j|python-mode ()
  "Bootstrap `jn-python'."
  (setq auto-mode-alist (rassq-delete-all #'j|python-mode auto-mode-alist))
  (python-mode))

(provide 'jn-python)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-python.el ends here

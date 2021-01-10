;;;; -*- lexical-binding: t; -*-
(require 'jn-functions)

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (add-hook 'python-mode-hook (lambda ()
                                "Set fill column to pycodestyle's 79."
                                (setq-local fill-column 79)))
  :config
  ;; https://github.com/jorgenschaefer/elpy/issues/887#issuecomment-281521965
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_")))

  (defvar pdb-path (intern "python3 -m pdb")
    "Path to pdb. Must be a symbol to work with `pdb'.")

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

    (defun j-python-mode-set-pdb-command ()
      (setq-local gud-pdb-command-name
                  (concat windows-python-pdb-path " " buffer-file-name)))

    ;; Everytime we enter a new python buffer, set the command path
    ;; to include the buffer filename.
    (add-hook 'python-mode-hook 'j-python-mode-set-pdb-command)))

;;;###autoload
(defun j-python-mode ()
  "Bootstrap `jn-python'."
  (setq auto-mode-alist (rassq-delete-all #'j-python-mode auto-mode-alist))
  (python-mode))

(provide 'jn-python)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-python.el ends here

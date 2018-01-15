;;;; -*- lexical-binding: t; -*-

(use-package groovy-mode
  :mode
  ("\\.gradle\\'" . groovy-mode)
  ("\\.groovy\\'" . groovy-mode))

;;;###autoload
(defun +groovy-mode ()
  "Bootstrap `jn-groovy'."
  (setq auto-mode-alist (rassq-delete-all #'+groovy-mode auto-mode-alist))
  (groovy-mode))

(provide 'jn-groovy)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-groovy.el ends here

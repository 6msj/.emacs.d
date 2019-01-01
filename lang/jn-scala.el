;;;; -*- lexical-binding: t; -*-

(use-package scala-mode :ensure t)

(defun j|scala-mode ()
  "Bootstrap `jn-scala'."
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) 'j|scala-mode)
      (setf (cdr alist) 'scala-mode)))
  (scala-mode))

(provide 'jn-scala)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-scala.el ends here

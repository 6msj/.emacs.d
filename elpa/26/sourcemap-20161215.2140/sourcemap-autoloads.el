;;; sourcemap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sourcemap" "sourcemap.el" (0 0 0 0))
;;; Generated autoloads from sourcemap.el

(autoload 'sourcemap-goto-corresponding-point "sourcemap" "\
hook function of coffee-mode. This function should not be used directly.
This functions should be called in generated Javascript file.

\(fn PROPS)" nil nil)

(autoload 'sourcemap-from-file "sourcemap" "\


\(fn FILE)" t nil)

(autoload 'sourcemap-from-string "sourcemap" "\


\(fn STR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sourcemap" '("sourcemap-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sourcemap-autoloads.el ends here

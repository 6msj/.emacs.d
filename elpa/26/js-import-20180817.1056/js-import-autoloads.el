;;; js-import-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "js-import" "js-import.el" (0 0 0 0))
;;; Generated autoloads from js-import.el

(autoload 'js-import "js-import" "\
Import Javascript files from your current project or dependencies.

\(fn)" t nil)

(autoload 'js-import-dev "js-import" "\
Import Javascript files from your current project or devDependencies.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js-import" '("js-import-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; js-import-autoloads.el ends here

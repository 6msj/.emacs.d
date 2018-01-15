;;; edebug-x-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "edebug-x" "edebug-x.el" (0 0 0 0))
;;; Generated autoloads from edebug-x.el

(autoload 'edebug-x-modify-breakpoint-wrapper "edebug-x" "\
Set a breakpoint from an Elisp file.
The current function that pointer is in will be instrumented if
not already. When called with a prefix argument a conditional
breakpoint is set.

\(fn ARG)" t nil)

(autoload 'edebug-x-evaluate-function "edebug-x" "\
Evaluate function on line.
This removes all breakpoints in this function.

\(fn)" t nil)

(autoload 'edebug-x-show-data "edebug-x" "\
Display instrumented functions and edebug breakpoints.
Frame is split into two vertically showing the tabluated buffers
for each.

\(fn)" t nil)

(autoload 'edebug-x-show-breakpoints "edebug-x" "\
Display breakpoints in a tabulated list buffer.

\(fn)" t nil)

(autoload 'edebug-x-show-instrumented "edebug-x" "\
Display instrumented functions in a tabluated list buffer.

\(fn)" t nil)

(autoload 'edebug-x-mode "edebug-x" "\
A minor mode that makes it easier to use Edebug

\(fn &optional ARG)" t nil)

(add-hook 'emacs-lisp-mode-hook 'edebug-x-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "edebug-x" '("list-edebug-x-" "edebug-x-" "instrumented")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; edebug-x-autoloads.el ends here

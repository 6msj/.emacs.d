;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

(when (eq system-type 'darwin)
  ;; Delete moving to OSX Trash Bin.
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")

  ;; https://stackoverflow.com/questions/4076360/error-in-dired-sorting-on-os-x
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)

  ;; Set the shell environment properly.
  (use-package exec-path-from-shell
    :ensure t
    :if (memq window-system '(mac ns x))
    :config
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize))

  ;; Don't let osx swallow Meta key.
  (setq mac-pass-command-to-system nil)

  ;; Don't swap Command and Option if on a Desktop.
  (unless (+desktop-p)
    (setq mac-option-modifier 'super)
    (setq mac-command-modifier 'meta))

  ;; Right Alt -> Control
  (setq ns-right-option-modifier 'control)

  ;; Set Command+q to quit.
  (global-set-key (kbd "M-q") 'save-buffers-kill-terminal)

  ;; Copy and paste in OSX Terminal.
  (use-package pbcopy
    :ensure t
    :if (not (display-graphic-p))
    :config
    (turn-on-pbcopy))

  ;; Reveal in finder.
  (use-package reveal-in-osx-finder
    :ensure t
    :commands (reveal-in-osx-finder)))

(when (eq system-type 'gnu/linux)
  (defun +update-path-var ()
    "Update $PATH with list of directories."
    (interactive)
    (setenv "PATH"
            (concat (mapconcat
                     (lambda (path)
                       (expand-file-name path))
                     '("~/Code/love/src") ":")
                    ":" (getenv "PATH"))))

  (+update-path-var)

  (use-package xclip
    :if (executable-find "xclip")
    :ensure t
    :config
    (xclip-mode 1))
  ;; Work with stumpwm if it exists.
  ;; This probably needs testing on Linux where stumpwm is on the path
  ;; but not being used.
  ;; http://code.ryuslash.org/dot/tom/emacs/tree/.emacs.d/init.org
  (defvar oni:stumpish-program
    (expand-file-name (concat user-emacs-directory "lang/commonlisp/stumpish"))
    "The location of the stumpish executable.")

  (defmacro oni:stumpwm (&rest body)
    "Execute BODY in stumpwm."
    (declare (indent 0))
    `(call-process oni:stumpish-program nil nil nil
                   ,(format "eval '%S'" `(progn ,@body))))

  (defun oni:stumpwm-command (cmd)
    "Execute CMD in stumpwm."
    (call-process oni:stumpish-program nil nil nil cmd))

  (defun oni:stumpwm-echo (message)
    (call-process oni:stumpish-program nil nil nil (format "echo %s" message)))

  (defun +windmove-do-window-select (orig-fun &rest args)
    "Integrate with Stumpwm."
    (interactive)
    (condition-case _err
        (apply orig-fun args)
      (error
       (when (executable-find "stumpwm")
         (oni:stumpwm-command
          (format "move-focus %s" (nth 0 args)))))))
  (advice-add 'windmove-do-window-select :around #'+windmove-do-window-select))

(when (eq system-type 'windows-nt)
  (with-current-buffer "*scratch*"
    (cd (format "c:/Users/%s/" user-login-name)))

  ;; Add git path for windows.
  (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin/")
  (add-to-list 'exec-path "C:/Program Files/Git/bin/")

  ;; Image support
  ;; https://www.imagemagick.org/script/download.php
  ;; https://emacs.stackexchange.com/questions/7293/why-cannot-thumbnails-be-created-by-image-dired
  ;; https://sourceforge.net/projects/ezwinports/files/
  (setq image-dired-cmd-rotate-thumbnail-program "magick mogrify")
  (setq image-dired-cmd-create-thumbnail-program "magick convert")

  (defun explorer ()
    (interactive)
    (cond
     ;; in buffers with file name
     ((buffer-file-name)
      (shell-command
       (concat "start explorer /e,/select,\""
               (replace-regexp-in-string "/" "\\\\" (buffer-file-name)) "\"")))
     ;; in dired mode
     ((eq major-mode 'dired-mode)
      (shell-command
       (concat "start explorer /e,\""
               (replace-regexp-in-string "/" "\\\\" (dired-current-directory)) "\"")))
     ;; in eshell mode
     ((eq major-mode 'eshell-mode)
      (shell-command
       (concat "start explorer /e,\""
               (replace-regexp-in-string "/" "\\\\" (eshell/pwd)) "\"")))
     ;; use default-directory as last resource
     (t
      (shell-command
       (concat "start explorer /e,\""
               (replace-regexp-in-string "/" "\\\\" default-directory) "\""))))))

(provide 'jn-platform)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-platform.el ends here

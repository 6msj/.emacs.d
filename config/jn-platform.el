;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

(when (eq system-type 'darwin)
  ;; Delete moving to OSX Trash Bin.
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")

  ;; https://stackoverflow.com/questions/4076360/error-in-dired-sorting-on-os-x
  (with-eval-after-load 'dired
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))

  (use-package exec-path-from-shell
    :ensure t
    :defer
    :if (memq window-system '(mac ns x))
    :init
    ;; Set the shell environment properly.
    (defun exec-path-from-shell-copy-envs-async (names)
      "Run `exec-path-from-shell-copy-envs' asynchronously."
      (async-start
       `(lambda ()
          (load ,(locate-library "exec-path-from-shell"))
          (require 'exec-path-from-shell)
          (exec-path-from-shell-getenvs ',names))
       (lambda (pairs)
         (when pairs
           (require 'exec-path-from-shell)
           (mapc (lambda (pair)
                   (exec-path-from-shell-setenv (car pair) (cdr pair)))
                 pairs)))))

    (exec-path-from-shell-copy-envs-async '("PATH"
                                            "MANPATH"
                                            "GOPATH"
                                            "RUST_SRC_PATH")))

  ;; Don't let osx swallow Meta key.
  (setq mac-pass-command-to-system nil)

  ;; Don't swap Command and Option if on a Desktop.
  (unless (j|desktop-p)
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
  (use-package xclip
    :if (executable-find "xclip")
    :ensure t
    :config
    (xclip-mode 1)))

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

  ;; FIXME: Look to see if Git directory in Program Files has 'find' and 'sort'.
  ;; We might be able to use that to get 'find' working on Windows.
  ;; Work with gpg4win.
  ;; https://emacs.stackexchange.com/questions/21699/how-to-configure-easypg-with-gpg4win-or-any-other-windows-gpg-version
  (setq epg-gpg-home-directory "c:/Users/jangu/AppData/Roaming/gnupg"
        epg-gpg-program "c:/Program Files (x86)/GnuPG/bin/gpg.exe"
        epg-gpgconf-program "c:/Program Files (x86)/GnuPG/bin/gpgconf.exe")

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

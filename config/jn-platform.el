;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

(when (or BSD-P
          MAC-P
          LINUX-P)
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
          (setq exec-path-from-shell-shell-name "/bin/zsh")
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
                                            "RUST_SRC_PATH"))))

(when MAC-P
  ;; Delete moving to OSX Trash Bin.
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")

  ;; https://stackoverflow.com/questions/4076360/error-in-dired-sorting-on-os-x
  (with-eval-after-load 'dired
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))

  ;; Don't let osx swallow Meta key.
  (setq mac-pass-command-to-system nil)

  ;; Don't swap Command and Option if on a Desktop.
  (unless DESKTOP-P
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
    :commands (reveal-in-osx-finder))

  ;; https://emacs.stackexchange.com/questions/29441/how-do-i-disable-menu-bar-mode-only-for-tty-frames
  (defun j-osx-contextual-menubar (&optional frame)
    "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
    (interactive)
    (set-frame-parameter frame 'menu-bar-lines
                         (if (display-graphic-p frame)
                             1
                           0)))

  (add-hook 'after-make-frame-functions #'j-osx-contextual-menubar))

(when LINUX-P
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

  (use-package xclip
    :if (executable-find "xclip")
    :ensure t
    :config
    ;; ;; https://emacs.stackexchange.com/questions/7191/copy-and-paste-to-system-clipboard-in-tmux
    ;; ;; Note: Didn't add the tmux part.
    ;; (setq select-enable-clipboard t)
    ;; (setq select-enable-primary t)

    (xclip-mode 1)))

;; Windows Notes
;; 1. Need to install git for git binary.
;; 2. Windows find is not the same as Emacs find.
;; 3. gpg4win is needed for gpg.
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

  ;; Work with gpg4win.
  ;; https://emacs.stackexchange.com/questions/21699/how-to-configure-easypg-with-gpg4win-or-any-other-windows-gpg-version
  (setq epg-gpg-home-directory (format "c:/Users/%s/AppData/Roaming/gnupg" user-login-name)
        epg-gpg-program "c:/Program Files (x86)/GnuPG/bin/gpg.exe"
        epg-gpgconf-program "c:/Program Files (x86)/GnuPG/bin/gpgconf.exe")

  (defun user-windows-directory ()
    "Return the directory of C:/Users/username/ in Windows."
    (format "c:/Users/%s/" user-login-name))

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
               (replace-regexp-in-string "/" "\\\\" default-directory) "\"")))))

  (defun foobar-convert-playlist ()
    "Copy foobar playlists to HiBy device, modifying drive prefix."
    (interactive)
    (let* ((playlist-directory (format "%sDesktop/Playlists/" (user-windows-directory)))
           (playlist-directory-tmp (format "%stmp/" playlist-directory))
           (default-directory playlist-directory)
           (playlists
            (seq-filter
             (lambda (x)
               (not (seq-contains-p '("Filter Results"
                                      "Replay Gain Missing"
                                      "LOSSLESS"
                                      "LOSSY"
                                      ;; "Library Viewer Selection"
                                      "MaybeToZero"
                                      "RANDOM"
                                      "TODO_Indie"
                                      "TODO_J"
                                      "TODO_K"
                                      "i_1"
                                      "i_2")
                                    x 'string-match-p)))
             (directory-files playlist-directory :full-match ".m3u" nil))))
      (message (format "Creating directory %s..." playlist-directory-tmp))
      (unless (file-exists-p playlist-directory-tmp)
        (make-directory playlist-directory-tmp))
      ;; Copy playlists to a temporary directory first.
      (dolist (playlist playlists)
        (message (format "Copying playlist: %s to directory: %s ..."
                         playlist playlist-directory-tmp))
        (copy-file playlist playlist-directory-tmp))
      ;; Modify the temporary playlists.
      (let ((playlist-data-dir "I:\\playlist_data\\")
            (tmp-playlists (directory-files playlist-directory-tmp :full-match ".m3u" nil)))
        (when (file-exists-p playlist-data-dir)
          ;; Make sure we have a fresh directory to work with.
          (delete-directory playlist-data-dir :recursive)
          (make-directory playlist-data-dir))
        ;; For each playlist, replace drive letter and copy to target directory.
        (dolist (tmp-playlist tmp-playlists)
          (find-file tmp-playlist)
          (with-current-buffer (current-buffer)
            (while (re-search-forward "J:\\\\" nil t)
              (replace-match "I:\\\\"))
            (save-buffer)
            (kill-buffer))
          (when (file-exists-p playlist-data-dir)
            (message (format "Copying playlist: %s to directory: %s ..."
                             tmp-playlist playlist-data-dir))
            (copy-file tmp-playlist playlist-data-dir))))
      ;; Final cleanup of original temporary playlist directory
      (message (format "Deleting directory %s..." playlist-directory-tmp))
      (delete-directory playlist-directory-tmp :recursive))))

(when BSD-P
  ;; https://www.reddit.com/r/emacs/comments/68xtx1/reliable_way_of_setting_up_gpg_password_input_in/
  (setq epa-pinentry-mode 'loopback))

(provide 'jn-platform)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-platform.el ends here

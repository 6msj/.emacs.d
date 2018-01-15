;;;; -*- lexical-binding: t; -*-

(require 'jn-dependencies)
(require 'jn-functions)

(+update-c-style '(c-mode . "k&r"))

(use-package dummy-h-mode
  :ensure t
  :init
  :mode ("\\.h$" . dummy-h-mode))

(use-package clang-format
  :ensure t
  :commands (clang-format-buffer clang-format-region)
  :init
  ;; https://eklitzke.org/smarter-emacs-clang-format
  (defun clang-format-buffer-smart ()
    "Reformat buffer if .clang-format exists in the projectile root."
    (condition-case nil
        (when (f-exists?
               (expand-file-name ".clang-format"
                                 (projectile-project-root)))
          (clang-format-buffer))
      (error nil)))

  (dolist (hook '(c-mode-hook c++-mode-hook objc-mode-hook))
    (add-hook
     hook
     (lambda ()
       (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))))

  (defun clang-format-region-or-buffer()
    "If clang-format is not available, do the default indenting.
Otherwise try to use clang-format. Indents region if there's a selection,
otherwise buffer is formatted."
    (interactive)
    (if (and (executable-find "clang-format")
             (locate-dominating-file default-directory ".clang-format"))
        (if (region-active-p)
            (call-interactively 'clang-format-region)
          (clang-format-buffer))
      (indent-region-or-buffer)))

  (+add-indent-command #'clang-format-region-or-buffer
                       '(c-mode c++-mode objc-mode)))

(use-package cc-mode
  :mode
  ("\\.c\\'" . c-mode)
  :init
  (with-eval-after-load 'evil
    (add-hook 'c-mode-common-hook
              (lambda ()
                (+evil-bind-key
                 (normal visual)
                 (c++-mode-map c-mode-map)
                 (kbd "go") #'ff-find-other-file
                 (kbd "gr") '+c-find-references
                 (kbd "gd") '+c-find-symbol
                 (kbd "gv") '+c-find-virtuals
                 (kbd "gf") '+c-find-file))))
  :config
  (+make-async-shell-cmd
   ("cocos run -p ios"
    "cocos run -p android -j 4"
    "cocos run -p mac"))
  (defvar +c-navigation-stack '() "Stack used to navigate tags.")
  (+make-async-shell-cmd
   ("make all" "make install")
   'c-mode)
  (defhydra hydra-c-mode
    (:color teal :hint nil)
    "C Mode"
    ("u" +c-mode-make-all "Make")
    ("i" +c-mode-make-install "Install")
    ("t" hydra-rtags-mode/body "RTags")
    ("d" hydra-gud-lldb/body "LLDB")
    ("q" nil "Cancel"))

  (+add-mode-command #'hydra-c-mode/body '(c-mode c++-mode))
  (+add-debug-command #'hydra-gud-lldb/body '(c-mode c++-mode)))

(use-package dtrt-indent
  :ensure t
  :diminish dtrt-indent-mode
  :commands (dtrt-indent-mode)
  :init
  ;; Linux coding style
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
         c-basic-offset)))

  (c-add-style
   "linux-tabs-only"
   '("linux" (c-offsets-alist
              (arglist-cont-nonempty
               c-lineup-gcc-asm-reg
               c-lineup-arglist-tabs-only))))

  (defun +setup-c-format ()
    "Set up indentation, format, etc."
    (if (or
         (derived-mode-p 'c++-mode)
         (derived-mode-p 'c-mode))
        (progn
          (c-set-style (+project-c-style))
          (cond
           ((string-equal c-indentation-style "k&r")
            ;; It's set to 5 in cc-styles.el.
            (setq c-basic-offset 4)
            (dtrt-indent-mode))
           ((string-equal c-indentation-style "linux-tabs-only")
            (setq dtrt-indent-mode nil)
            (setq indent-tabs-mode t))
           ((string-equal c-indentation-style "gnu")
            ;; These are Emacs defaults, and by extension
            ;; the gnu style.
            (setq indent-tabs-mode t)
            (setq tab-width 8)
            (setq indent-line-function 'c-indent-line)
            (setq c-basic-offset 2))
           (:else
            (dtrt-indent-mode))))
      (setq dtrt-indent-mode nil)))

  (add-hook 'prog-mode-hook #'+setup-c-format))

(use-package rtags
  :ensure t
  :commands (rtags-start-process-unless-running
             rtags-install)
  :init
  (defun +rtags-setup ()
    "Set up rtags."
    (+company-push-backend 'company-rtags t t)
    (rtags-diagnostics))
  (add-hook 'c-mode-hook #'+rtags-setup)
  (add-hook 'c++-mode-hook #'+rtags-setup)

  (defun +use-rtags (&optional useFileManager)
    "Return whether or not to use `rtags'."
    (and
     (fboundp 'rtags-executable-find)
     (rtags-executable-find "rc")
     (cond (useFileManager (rtags-has-filemanager))
           (t (rtags-is-indexed)))))
  :config
  (use-package ivy-rtags :ensure t :defer t)
  (use-package company-rtags :ensure t
    :commands (company-rtags))

  (setq rtags-jump-to-first-match nil
        rtags-tracking t
        rtags-tracking-timer-interval 3
        rtags-autostart-diagnostics t)
  :config
  (setq rtags-install-path
        (format "%sservers/%s/rtags/"
                user-emacs-directory emacs-major-version)))

(use-package gud-lldb
  :load-path "~/.emacs.d/fork/gud-lldb"
  :commands (lldb)
  :ensure nil)

(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :commands
  (modern-c++-font-lock-mode)
  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package malinka
  :load-path "~/.emacs.d/fork/malinka"
  :ensure t
  :commands (malinka-mode)
  :init
  (defun +malinka-maybe-start ()
    "Set up `malinka-mode' unless it isn't appropriate."
    ;; Using a heuristic here to check if `malinka-mode' should be started.
    (unless (s-ends-with? "~" (buffer-name))
      (malinka-mode)))
  (add-hook 'c++-mode-hook '+malinka-maybe-start)
  :config
  (setq malinka-print-info? t)
  (setq malinka-print-debug? nil)
  (setq malinka-completion-system 'ivy-completing-read)
  (setq malinka-idle-project-check-seconds 3)

  (malinka-define-project
   :name "flappy2"
   :root-directory "~/Code/flappyworld/c/flappy"
   :build-directory "~/Code/flappyworld/c/flappy/build"
   :configure-cmd "cmake .. -DCMAKE_BUILD_TYPE=Debug -DHEADLESS=1"
   :compile-cmd "make -j4"
   :watch-file "~/Code/flappyworld/c/flappy/CMakeLists.txt"))

(defun +c-find-references-at-point (&optional prefix)
  "DWIM Find References."
  (interactive "P")
  (cond
   ((and (+use-rtags)
         (rtags-find-references-at-point prefix)
         (not rtags-last-request-not-indexed))
    (push 'rtags +c-navigation-stack))
   ((bound-and-true-p ggtags-mode)
    (push 'ggtags +c-navigation-stack)
    (call-interactively 'ggtags-find-reference))
   (t
    (+find-references))))

(defun +c-find-symbol ()
  "DWIM Find Symbol."
  (interactive)
  (cond
   ((+use-rtags)
    (push 'rtags +c-navigation-stack)
    (call-interactively 'rtags-find-symbol))
   ((bound-and-true-p ggtags-mode)
    (call-interactively 'ggtags-find-tag-dwim))
   (t
    (push #'dumb-jump-go +c-navigation-stack)
    (call-interactively #'dumb-jump-go))))

(defun +c-find-references ()
  "DWIM Find References."
  (interactive)
  (cond
   ((+use-rtags)
    (push 'rtags +c-navigation-stack)
    (call-interactively 'rtags-find-references))
   ((bound-and-true-p ggtags-mode)
    (push 'ggtags +c-navigation-stack)
    (call-interactively 'ggtags-find-reference))
   (t
    (+find-references))))

(defun +c-find-file ()
  "DWIM Find File."
  (interactive)
  (cond
   ((+use-rtags)
    (call-interactively 'rtags-find-file))
   ((bound-and-true-p ggtags-mode)
    (call-interactively 'ggtags-find-file))
   (t
    (call-interactively 'find-file-at-point))))

(defun +c-find-virtuals ()
  "DWIM Find Virtuals."
  (interactive)
  (rtags-find-virtuals-at-point))

;; Objective-C

;; TODO:
;; Look at ggtags
;; https://github.com/leoliu/ggtags/wiki/Use-ggtags-to-browse-Objective-C-projects

;; Look at YCMD when iOS integration is in for completion.
;; https://github.com/Valloric/ycmd/issues/534

(use-package etags-select
  :ensure t
  :commands (etags-select-find-tag-at-point)
  :config
  (setq etags-select-use-short-name-completion t))

(use-package cc-mode
  :mode
  ("\\.m\\'" . objc-mode)
  ("\\.mm\\'" . objc-mode)
  ("\\.xctool.args\\'" . objc-mode)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal objc-mode-map
      (kbd "go") #'ff-find-other-file)))

(use-package xcode-mode
  ;; https://github.com/phonegap/ios-sim
  ;; https://github.com/facebook/xctool
  :commands (xcode-mode)
  :load-path "~/.emacs.d/fork/xcode-mode/"
  :ensure nil
  :init
  (add-hook 'objc-mode-hook #'xcode-mode)
  (add-hook 'swift-mode-hook #'xcode-mode)
  :config
  (setq xcode-completing-read-function 'ivy-read))

(defun +common-c-clear-bootstrap ()
  "Remove entries in `auto-mode-alist' that were set in this file."
  (setq auto-mode-alist (rassq-delete-all #'+objc-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'+c++-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'+c-mode auto-mode-alist)))

;;;###autoload
(defun +objc-mode ()
  "Bootstrap `jn-c'."
  (+common-c-clear-bootstrap)
  (objc-mode))

;;;###autoload
(defun +c++-mode ()
  "Bootstrap `jn-c'."
  (+common-c-clear-bootstrap)
  (c++-mode))

;;;###autoload
(defun +c-mode ()
  "Bootstrap `jn-c'."
  (+common-c-clear-bootstrap)
  (c-mode))

(provide 'jn-c)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-c.el ends here

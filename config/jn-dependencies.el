;;;; -*- lexical-binding: t; -*-

(use-package diminish
  :ensure t
  :config
  (diminish 'subword-mode)
  (diminish 'visual-line-mode)
  (diminish 'abbrev-mode)
  (diminish 'buffer-face-mode)

  (with-eval-after-load 'eldoc
    (diminish 'eldoc-mode))
  (with-eval-after-load 'autorevert
    (diminish 'auto-revert-mode)))

(use-package alert :ensure t
  :commands (alert)
  :config
  (when (eq system-type 'darwin)
    (setq alert-default-style 'osx-notifier))
  (when (eq system-type 'windows-nt)
    (setq alert-default-style 'message)))

(use-package async
  :ensure t
  :init
  (defun j|async-dired-on ()
    "Turn on `dired-async-mode' if remote."
    (when (file-remote-p default-directory)
      (dired-async-mode)))
  (add-hook 'dired-sidebar-mode-hook #'j|async-dired-on)
  (add-hook 'dired-mode-hook #'j|async-dired-on))

(use-package dumb-jump
  :ensure t
  :commands (dumb-jump-go dumb-jump-back))

(use-package smart-jump
  :load-path "~/.emacs.d/fork/smart-jump"
  :ensure nil
  :config
  (smart-jump-setup-default-registers))

(use-package ace-window
  :ensure t
  :commands (ace-delete-window
             ace-swap-window
             ace-delete-other-windows
             ace-window
             aw-select))

(use-package general
  :ensure t
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-override-mode)
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "SPC" 'hydra-space/body))

(use-package hydra
  :ensure t
  :init
  ;; Hacky!
  ;; Defining `hydra-base-map' so that the ESC keys are there when all the
  ;; macros are expanded at the beginning.
  (defvar hydra-base-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "ESC") 'hydra-keyboard-quit)
      (define-key map (kbd "<ESC>") 'hydra-keyboard-quit)
      (define-key map (kbd "<escape>") 'hydra-keyboard-quit)
      (define-key map (kbd "q") 'hydra-keyboard-quit)

      (define-key map [?\C-u] 'hydra--universal-argument)
      (define-key map [?-] 'hydra--negative-argument)
      (define-key map [?0] 'hydra--digit-argument)
      (define-key map [?1] 'hydra--digit-argument)
      (define-key map [?2] 'hydra--digit-argument)
      (define-key map [?3] 'hydra--digit-argument)
      (define-key map [?4] 'hydra--digit-argument)
      (define-key map [?5] 'hydra--digit-argument)
      (define-key map [?6] 'hydra--digit-argument)
      (define-key map [?7] 'hydra--digit-argument)
      (define-key map [?8] 'hydra--digit-argument)
      (define-key map [?9] 'hydra--digit-argument)
      (define-key map [kp-0] 'hydra--digit-argument)
      (define-key map [kp-1] 'hydra--digit-argument)
      (define-key map [kp-2] 'hydra--digit-argument)
      (define-key map [kp-3] 'hydra--digit-argument)
      (define-key map [kp-4] 'hydra--digit-argument)
      (define-key map [kp-5] 'hydra--digit-argument)
      (define-key map [kp-6] 'hydra--digit-argument)
      (define-key map [kp-7] 'hydra--digit-argument)
      (define-key map [kp-8] 'hydra--digit-argument)
      (define-key map [kp-9] 'hydra--digit-argument)
      (define-key map [kp-subtract] 'hydra--negative-argument)
      map)
    "Keymap that all Hydras inherit.  See `universal-argument-map'.")
  :config
  (defhydra hydra-space (:color blue :hint nil :idle .2)
    "

   Space: %s`default-directory

    Find           Manage         ^^^^Do               Mode
  ------------------------------------------------------------------------------
    _f_ File       _w_ Window      _s_ Search           _m_ Mode
    _b_ Buffer     _g_ Git         _R_ Refactor         _d_ Debug
    _r_ Recent     _p_ Project     _v_ Edit Init.el     _e_ Eval
    _n_ Sidebar    _y_ System      _o_ Org              _t_ Test
    _SPC_ Any      _U_ Undo        _u_ Universal Arg..  _=_ Format

"
    ("1" digit-argument)
    ("2" digit-argument)
    ("3" digit-argument)
    ("4" digit-argument)
    ("5" digit-argument)
    ("6" digit-argument)
    ("7" digit-argument)
    ("8" digit-argument)
    ("9" digit-argument)
    ("0" digit-argument)
    ("u" universal-argument)
    ("f" j|find-file-dwim)
    ("b" j|buffers-dwim)
    ("r" j|recentf-dwim)
    ("w" hydra-window/body)
    ("-" split-window-below)
    ("|" split-window-right)
    ("\\" split-window-right)
    ("h" evil-window-left)
    ("l" evil-window-right)
    ("k" evil-window-up)
    ("j" evil-window-down)
    ("." evil-next-buffer)
    (">" evil-prev-buffer :color red)
    ("," evil-prev-buffer)
    ("<" evil-prev-buffer :color red)
    ("<backspace>" delete-window)
    ("DEL" delete-window) ;; For terminals.
    ("s" hydra-search/body)
    ("v" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
    ("U" undo-tree-visualize)
    ("x" kill-buffer)
    ("y" hydra-system/body)
    ("n" dired-sidebar-toggle-sidebar)
    ("p" matcha-projectile/body)
    ("c" matcha-evil-mc/body)
    ("g" matcha-magit/body)
    ("SPC" counsel-fzf)
    ("RET" quickrun)
    (";" counsel-M-x)
    (":" eval-expression)
    ("'" eval-expression)
    ("o" hydra-org-space/body)
    ("S" j|save-all-buffers)
    ("R" matcha-run-refactor-command)
    ("=" matcha-run-format-command)
    ("d" matcha-run-debug-command)
    ("m" matcha-run-mode-command)
    ("e" matcha-run-eval-command)
    ("t" matcha-run-test-command))

  (defhydra hydra-system (:color blue :hint nil :idle .2)
    "

     System: %s`default-directory

      ^^Shell^^             ^^External^^      ^^Bookmarks^^
  ------------------------------------------------------------------------------
    _y_ Terminal        _f_ Finder         _bb_ Add
    _e_ Eshell          _t_ Terminal       _bd_ Remove
    _s_ Shell                            ^^_bj_ Jump
    _._ Next                             ^^_bl_ List
    _,_ Previous                         ^^_bs_ Save

     ^^Misc^^                ^^Processes^^
  ------------------------------------------------------------------------------
    _i_ IRC              _pp_ Prodigy
    _m_ Mail             _ps_ Start Profiler
    _w_ Passwords        _pk_ Stop Profiler
                       ^^_pr_ Report Profiler
                       ^^_pl_ List Processes

"
    ("y" j|open-shell)
    ("." multi-term-next :color red)
    ("," multi-term-prev :color red)
    ("s" shell)
    ("e" eshell)
    ("f" j|explorer-finder)
    ("t" j|open-terminal)
    ("i" j|start-irc)
    ("m" j|notmuch)
    ("w" pass)
    ("bb" bookmark-set)
    ("bd" bookmark-delete)
    ("bj" bookmark-jump)
    ("bl" bookmark-bmenu-list)
    ("bs" bookmark-save)
    ("ps" profiler-start)
    ("pr" profiler-report)
    ("pk" profiler-stop)
    ("pl" list-processes)
    ("pp" prodigy))

  (defhydra hydra-window (:color blue :hint nil)
    "

   Window: %s`default-directory

    ^Misc^             ^Split^          ^Text Scale^         ^Winner^
  ------------------------------------------------------------------------------
    _=_ Balance         _-_ Split --     _+_ Increase      _._ Redo
    _r_ Resize          _|_ Split ||     ___ Decrease      _,_ Undo
    _s_ Rotate
    _t_ Swap

    ^Resizing^            ^Narrowing^         ^Frames^
  ------------------------------------------------------------------------------
    _<right>_  ->         _n_ Narrow          _m_ Maximize
     _<left>_  <-         _w_ Widen           _F_ Fullscreen
     _<down>_  Down                         ^^_2_ Make Frame
       _<up>_  Up                           ^^_1_ Delete Other Frames
                                          ^^^^_0_ Delete Current Frame
                                          ^^^^_o_ Other Frame

"
    ("=" balance-windows)
    ("r" j|resize-window)
    ("s" toggle-window-split)
    ("t" rotate-windows)
    ("<right>" shrink-window-horizontally :color red)
    ("<left>" enlarge-window-horizontally :color red)
    ("<down>" shrink-window :color red)
    ("<up>" enlarge-window :color red)
    ("-" split-window-below)
    ("|" split-window-right)
    ("\\" split-window-right)
    ("h" split-window-below)
    ("v" split-window-right)
    ("+" text-scale-increase :color red)
    ("_" text-scale-decrease :color red)
    ("." winner-redo :color red)
    ("," winner-undo :color red)
    ("n" narrow-to-region)
    ("w" widen)
    ("m" toggle-frame-maximized)
    ("F" toggle-frame-fullscreen)
    ("0" delete-frame)
    ("1" delete-other-frames)
    ("2" make-frame-command)
    ("o" other-frame))

  (defhydra hydra-search (:color blue :hint nil)
    "

   Search: %s(matcha-projectile-root)

    ^^Counsel              ^^Occur            ^^Ag
  ------------------------------------------------------------------------------
    _s_ Swiper       _o_ Occur             _a_ Ag
    _S_ Swiper All   _O_ Multi-Occur       _p_ Project
    _c_ Ag           _m_ Matching Mode
    _f_ File         _M_ Matching Regex
    _r_ Rg           _P_ Projectile
    _g_ Git

"
    ("R" j|counsel-rg)
    ("r" counsel-rg)
    ("s" swiper)
    ("S" swiper-all)
    ("C" j|counsel-ag)
    ("c" counsel-ag)
    ("f" counsel-find-file)
    ("g" counsel-git)
    ("o" occur)
    ("O" multi-occur)
    ("m" j|multi-occur-in-this-mode)
    ("M" multi-occur-in-matching-buffers)
    ("P" projectile-multi-occur)
    ("a" ag)
    ("p" ag-project))

  (setq lv-use-separator t))

(use-package matcha
  :load-path "~/.emacs.d/fork/matcha/"
  :ensure nil
  :config
  (matcha-setup))

(use-package ggtags
  :ensure t
  ;; :diminish ggtags-mode
  :commands (ggtags-mode
             ggtags-find-tag-dwim
             ggtags-prev-mark
             ggtags-find-reference
             ggtags-find-file)
  :init
  (mapcar
   (lambda (x)
     (add-hook x #'ggtags-mode))
   '(java-mode-hook c-mode-hook c++-mode-hook)))

(use-package flycheck-xcode
  :load-path "~/.emacs.d/fork/flycheck-xcode"
  :ensure nil
  :commands (flycheck-xcode-setup)
  :init
  (mapc
   (lambda (x)
     (add-hook x #'flycheck-xcode-setup))
   '(c-mode-hook c++-mode-hook objc-mode-hook swift-mode-hook)))

(use-package flymake-gradle
  :ensure nil
  :load-path "~/.emacs.d/fork/flymake-gradle/"
  :commands (flymake-gradle-setup flymake-gradle-add-hook)
  :init
  (defun j|flymake-gradle-setup ()
    (flycheck-mode -1)
    (flymake-gradle-add-hook))
  (add-hook 'kotlin-mode-hook #'j|flymake-gradle-setup)
  (add-hook 'java-mode-hook #'j|flymake-gradle-setup))

(use-package flycheck-gradle
  :load-path "~/.emacs.d/fork/flycheck-gradle"
  :ensure nil
  :commands (flycheck-gradle-setup)
  :init
  (mapc
   (lambda (x)
     (add-hook x #'flycheck-gradle-setup))
   '(java-mode-hook kotlin-mode-hook))
  :config
  (setq flycheck-gradle-java-compile-function
        #'flycheck-gradle-java-compile->android)
  (setq flycheck-gradle-adjust-log-level-automatically t))

(use-package editorconfig
  :ensure t
  :commands (editorconfig-mode editorconfig-mode-apply)
  :init
  (mapcar
   (lambda (hook)
     (add-hook
      hook
      (lambda ()
        (when (and
               (executable-find "editorconfig")
               (projectile-project-p)
               (file-exists-p
                (expand-file-name ".editorconfig"
                                  (projectile-project-root))))
          (editorconfig-mode-apply)))))
   '(typescript-mode-hook web-mode-hook)))

(use-package realgud
  :ensure t
  :commands (realgud:pdb)
  :config
  (setq realgud:pdb-command-name "/usr/lib/python2.7/pdb.py"))

(use-package quickrun
  :ensure t
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region)
  :config
  (setq quickrun-timeout-seconds 30))

(use-package which-key
  :ensure t
  :commands (which-key-show-top-level
             which-key-show-major-mode)
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location '(right bottom)))

(when (> emacs-major-version 25)
  (use-package flymake-diagnostic-at-point
    :ensure t
    :commands (flymake-diagnostic-at-point-mode)
    :init
    (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
    :config
    (setq flymake-diagnostic-at-point-error-prefix nil))

  (use-package eglot
    ;; :load-path "~/Code/eglot/"
    :ensure t
    :commands (eglot eglot-ensure)
    :init
    (defun eglot-disable-other-modes ()
      "Disable other modes that would conflict with `eglot'."
      (mapc (lambda (mode)
              (when (and
                     (boundp mode)
                     (symbol-value mode))
                (funcall mode -1)))
            '(flycheck-mode
              ggtags-mode)))
    (add-hook 'eglot--managed-mode-hook #'eglot-disable-other-modes)

    (add-hook 'python-mode-hook #'eglot-ensure)
    (add-hook 'go-mode-hook #'eglot-ensure)
    (add-hook 'kotlin-mode-hook #'eglot-ensure)
    (add-hook 'c-mode-hook #'eglot-ensure)
    (add-hook 'c++-mode-hook #'eglot-ensure)
    :config
    (setq eglot-events-buffer-size 0)
    (setq eglot-sync-connect 0)
    ;; https://github.com/joaotavora/eglot/issues/8#issuecomment-414149077
    (advice-add 'eglot-eldoc-function :around
                (lambda (oldfun)
                  (let ((help (help-at-pt-kbd-string)))
                    (if help (message "%s" help) (funcall oldfun)))))))

(provide 'jn-dependencies)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-dependencies.el ends here

;;;; -*- lexical-binding: t; -*-

;; Set up font.
(cond
 ((eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :family "Ubuntu Mono")
  (set-face-attribute 'default nil :height 110)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding")))
 ((eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Consolas-11"))
 (:osx
  ;; Use the osx emoji font for emoticons.
  (when (fboundp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      '(#x1F600 . #x1F64F)
                      (font-spec :name "Apple Color Emoji") nil 'prepend))

  (defun find-and-set-font (&rest candidates)
    "Set the first font found in CANDIDATES."
    (let ((font (cl-find-if (lambda (f)
                              (find-font (font-spec :name f)))
                            candidates)))
      (when font
        (set-face-attribute 'default nil :font font))
      font))

  (defun +setup-font (fonts)
    "Set font by looking through list of FONTS.
FONTS is in this format
  '\(\(\"Hack\" . 11\)
    \(\"Consolas\" . 12\)
    \(\"Menlo\" . 12 \)\)."
    (interactive)
    (apply #'find-and-set-font
           (mapcar (lambda (font)
                     (concat (car font) "-" (number-to-string
                                             (if (and (fboundp '+desktop-p)
                                                      (+desktop-p))
                                                 (+ 1 (cdr font))
                                               (cdr font)))))
                   fonts)))

  ;; Set font only if in GUI.
  ;; If started from a terminal (through --daemon), we can't tell if it's a
  ;; graphical frame yet so check in after-make-frame-functions.
  (let ((fonts '(("Oxygen Mono" . 13)
                 ("Noto Mono" . 13)
                 ("Ibm Plex Mono" . 13)
                 ("Fira Mono" . 13)
                 ("Hasklig" . 13)
                 ("Menlo" . 12)
                 ("SF Mono" . 11)
                 ("Input Mono" . 13)
                 ("Consolas" . 12)
                 ("M+ 1mn" . 12)
                 ("Hack" . 11)
                 ("Source Code Pro" . 13)
                 ("DejaVu Sans Mono" . 12)
                 ("Mononoki" . 12)
                 ("PragmataPro" . 13))))
    (if (display-graphic-p)
        (+setup-font fonts)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (when (display-graphic-p frame)
                    (with-selected-frame frame
                      (+setup-font fonts)))))))))

(defun +disable-ui-fluff ()
  "Disable ui fluff."
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (unless (and (eq system-type 'darwin) window-system)
    (menu-bar-mode -1)))
(+disable-ui-fluff)

(use-package theme-changer
  :ensure t
  :init
  ;; Dallas
  (setq calendar-latitude 32.85)
  (setq calendar-longitude -96.85)
  ;; Japan
  ;; (setq calendar-latitude 36.3147)
  ;; (setq calendar-longitude 139.8)
  :config
  (defvar after-load-theme-hook nil
    "Hook run after a color theme is loaded using `change-theme'.")

  (defun +make-modeline-taller ()
    "Make the mode line taller."
    (interactive)
    (dolist (sym '(mode-line mode-line-inactive))
      (cond
       ((eq system-type 'windows-nt)
        (set-face-attribute sym nil :family "Lucida Sans Unicode"))
       (:default
        (set-face-attribute sym nil :family "Lucida Grande")))
      (set-face-attribute
       sym nil
       :box `(:line-width 5 :color ,(face-attribute `,sym :background)))))

  (defun +update-theme (&rest _args)
    "Update various UI elements when theme changes"
    (+make-modeline-taller)
    (with-eval-after-load 'org-mode
      (+customize-org-ui))
    (when (fboundp 'org-reload)
      (with-eval-after-load 'org-faces
        (set-face-foreground 'org-hide (face-attribute 'default :background)))
      (when (let (has-org-mode)
              (dolist (b (buffer-list) has-org-mode)
                (with-current-buffer b
                  (when (eq major-mode 'org-mode)
                    (setq has-org-mode t)))))
        (org-reload)))
    (with-eval-after-load 'company
      (set-face-attribute
       'company-preview
       nil
       :background
       (face-attribute 'company-preview-common :background)))

    ;; Run a custom hook to latch on elsewhere.
    (run-hooks 'after-load-theme-hook))

  (advice-add 'load-theme :after #'+update-theme)

  (defvar +current-theme nil "Current theme.")

  (defun +theme-changer-switch-theme (f &rest args)
    "Wrap `theme-changer-switch-theme' and track current theme."
    (mapc #'disable-theme custom-enabled-themes)
    (setq +current-theme (nth 1 args))
    (apply f args))

  (advice-add 'theme-changer-switch-theme :around '+theme-changer-switch-theme)

  ;; Set up transparent titlebar on OSX.
  (when (eq system-type 'darwin)
    (defun +osx-set-up-ui-polish (&optional frame)
      "Set up transparency in the titlebar and associated themes."
      (when (boundp 'ns-use-thin-smoothing)
        (setq ns-use-thin-smoothing nil))

      (set-frame-parameter frame 'ns-appearance nil)
      (set-frame-parameter frame 'ns-transparent-titlebar t)
      (if (or
           (string-match-p "-light" (symbol-name +current-theme))
           (memq +current-theme '(solarized-light)))
          (set-frame-parameter frame 'ns-appearance nil)
        (set-frame-parameter frame 'ns-appearance 'dark)))
    (+osx-set-up-ui-polish)
    (add-hook 'after-load-theme-hook #'+osx-set-up-ui-polish)
    (add-hook 'after-make-frame-functions #'+osx-set-up-ui-polish))

  (cond
   ((null window-system)
    ;; Setting the theme too early can make some UI elements look weird
    ;; when launching from emacsclient. For example, the mode line borders
    ;; will be colored weirdly.
    (if (not (daemonp))
        (change-theme 'gruvbox 'spacemacs-dark)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (change-theme 'gruvbox 'spacemacs-dark))))))
   ((eq system-type 'darwin)
    (change-theme 'farmhouse-dark 'solarized-light))
   ((eq system-type 'gnu/linux)
    (change-theme 'spacemacs-light 'spacemacs-dark))
   (t
    (change-theme 'solarized-light 'solarized-light))))

(use-package rainbow-delimiters
  ;; Use colorful parens.
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  (defun +bold-parens ()
    "Bold parentheses."
    (dotimes (i 9)
      (set-face-attribute
       (intern (format "rainbow-delimiters-depth-%d-face" (+ i 1))) nil :bold t)))
  (add-hook 'after-load-theme-hook #'+bold-parens)

  (dolist (hook (+lisp-hooks))
    (add-hook hook #'rainbow-delimiters-mode))
  :config
  (+bold-parens))

;; Let Emacs color Ansi symbols.
;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(defun +colorize-compilation-buffer ()
  (ignore-errors
    (require 'ansi-color)
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))

(add-hook 'compilation-filter-hook #'+colorize-compilation-buffer)

(use-package xterm-color
  :ensure t
  :commands (xterm-color-filter)
  :init
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :defer 5
  :config
  (setq highlight-symbol-idle-delay .5)

  (defun +match-highlight-symbol-face ()
    "Make `highlight-symbol-face' look like `highlight'."
    (set-face-attribute 'highlight-symbol-face nil
                        :background nil
                        :foreground nil
                        :underline t)
    (with-eval-after-load 'tide
      (set-face-attribute 'tide-hl-identifier-face nil
                          :background nil
                          :foreground nil
                          :underline t
                          :inherit nil)))

  ;; Match for existing buffers.
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (and (derived-mode-p 'prog-mode)
                 (not (member major-mode '(typescript-mode))))
        (+match-highlight-symbol-face)
        (highlight-symbol-mode 1))))

  ;; Match after theme changes.
  (add-hook 'after-load-theme-hook #'+match-highlight-symbol-face)

  (add-hook 'prog-mode-hook
            (lambda ()
              ;; `tide-mode' already supplies a highlight.
              (unless (member major-mode '(typescript-mode))
                (+match-highlight-symbol-face)
                (highlight-symbol-mode 1)))))

;; Themes
(use-package gotham-theme :defer :ensure t)
(use-package farmhouse-theme :defer :ensure t)
(use-package color-theme-solarized :defer :ensure t)
(use-package base16-theme :ensure t :defer)
(use-package doom-themes :ensure t :defer)
(use-package gruvbox-theme :ensure t :defer)

(use-package solarized-theme :defer :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-less-bold t))

(use-package github-theme :defer :ensure t
  :config
  (set-face-attribute 'escape-glyph nil :bold t)
  (set-face-attribute 'font-lock-constant-face nil :bold t)
  (set-face-attribute 'font-lock-keyword-face nil :bold t))

(use-package spacemacs-theme
  :defer
  :ensure t
  :init
  (setq spacemacs-theme-comment-bg nil))

(use-package moe-theme
  :ensure t
  :defer
  :init
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory
    (file-name-directory
     (locate-library (symbol-name 'moe-theme))))))

(use-package fruity-theme
  :load-path "~/.emacs.d/fork/fruity-theme"
  :ensure nil
  :defer
  :init
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory
    (file-name-directory
     (locate-library (symbol-name 'fruity-theme))))))

(use-package zerodark-theme
  :defer t
  :ensure t
  :init
  (add-hook 'after-load-theme-hook
            (lambda ()
              (when (eq +current-theme 'zerodark)
                ;; (zerodark-setup-modeline-format)
                (set-face-attribute 'mode-line nil :height .96)
                (set-face-attribute 'mode-line-inactive nil :height .96)
                (set-face-attribute 'header-line nil :height .96)))))

(provide 'jn-theme)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-theme.el ends here

;;;; -*- lexical-binding: t; -*-

;;;; Minimal UI
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(unless (and (eq system-type 'darwin)
             window-system)
  (menu-bar-mode -1))

;;;; Helper Functions

(defun j|theme-get-variable-pitch-font ()
  "Return variable pitch font to be used."
  (cond
   ((eq system-type 'darwin)
    (if (member "Fira Sans" (font-family-list))
        "Fira Sans"
      "Lucida Grande"))
   ((eq system-type 'windows-nt)
    "Lucida Sans Unicode")
   (:default
    "Arial")))

(defun j|theme-update (&rest _args)
  "Update various UI elements when theme changes"
  (let* ((variable-pitch-font (j|theme-get-variable-pitch-font))
         (font-size (cond
                     ((eq system-type 'windows-nt) 110)
                     ((j|desktop-p) 150)
                     (:default 130)))
         (family `(:family ,variable-pitch-font :height ,font-size)))
    (setq dired-sidebar-face family)
    (setq ibuffer-sidebar-face family))
  (j|theme-setup-modeline)
  (with-eval-after-load 'org-mode
    (j|org-customize-ui))
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

(advice-add 'load-theme :after #'j|theme-update)

;;;; Font

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

  (defun j|theme-setup-font (fonts)
    "Set font by looking through list of FONTS.
FONTS is in this format
  '\(\(\"Hack\" . 11\)
    \(\"Consolas\" . 12\)
    \(\"Menlo\" . 12 \)\)."
    (apply #'find-and-set-font
           (mapcar (lambda (font)
                     (concat (car font) "-" (number-to-string
                                             (if (and (fboundp 'j|desktop-p)
                                                      (j|desktop-p))
                                                 (+ 2 (cdr font))
                                               (cdr font)))))
                   fonts)))

  ;; Set font only if in GUI.
  ;; If started from a terminal (through --daemon), we can't tell if it's a
  ;; graphical frame yet so check in after-make-frame-functions.
  (let ((fonts '(("Fira Code" . 13)
                 ("Fira Mono" . 13)
                 ("Input Mono" . 13)
                 ("M+ 1mn" . 12)
                 ("Mononoki" . 12)
                 ("Oxygen Mono" . 13)
                 ("Noto Mono" . 13)
                 ("Ibm Plex Mono" . 13)
                 ("Hasklig" . 13)
                 ("Menlo" . 12)
                 ("SF Mono" . 11)
                 ("Consolas" . 12)
                 ("Hack" . 11)
                 ("Source Code Pro" . 13)
                 ("DejaVu Sans Mono" . 12)
                 ("PragmataPro" . 13)
                 ("Inconsolata-g for Powerline" . 14))))
    (if (display-graphic-p)
        (j|theme-setup-font fonts)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (when (display-graphic-p frame)
                    (with-selected-frame frame
                      (j|theme-setup-font fonts)))))))))

;;;; Modeline

(defun j|modeline-fontsize ()
  "Return font size of modeline."
  (cond
   ((eq system-type 'windows-nt) 110)
   ((j|desktop-p) 150)
   (:default 130)))

(defun j|modeline-background-color (sym)
  "Return background color of modeline."
  ;; If `mode-line-inactive' doesn't specify a background, use
  ;; `mode-line''s instead.
  (let* ((frame (selected-frame))
         (background (face-attribute sym :background frame)))
    (if (and
         (eq sym 'mode-line-inactive)
         (eq background 'unspecified))
        (face-attribute 'mode-line :background frame)
      background)))

(defun j|theme-setup-modeline ()
  "Theme modeline."
  (interactive)
  (setq underline-minimum-offset 999)
  (set-frame-parameter (selected-frame) 'right-divider-width 1)
  (unless (member '(right-divider-width . 1) default-frame-alist)
    (push '(right-divider-width . 1) default-frame-alist))
  (let* ((font (j|theme-get-variable-pitch-font))
         (font-size (j|modeline-fontsize))
         (border-color (face-foreground 'window-divider (selected-frame) t))
         (underline `(:color ,border-color))
         (overline border-color))
    (dolist (sym '(mode-line mode-line-inactive))
      (set-face-attribute
       sym
       nil
       :family font
       :height font-size
       :box `(:line-width 5 :color ,(j|modeline-background-color sym))
       :underline underline
       :overline overline))))

;;;; Packaging

(use-package rainbow-delimiters
  ;; Use colorful parens.
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  ;; This function is not used currently.
  (defun j|rainbow-delimiters-bold-parens ()
    "Bold parentheses."
    (dotimes (i 9)
      (set-face-attribute
       (intern (format "rainbow-delimiters-depth-%d-face" (+ i 1))) nil :bold t)))
  (dolist (hook (j|lisp-hooks))
    (add-hook hook #'rainbow-delimiters-mode)))

;; Let Emacs color Ansi symbols.
;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(defun j|theme-color-compilation-buffer ()
  (ignore-errors
    (require 'ansi-color)
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))

(add-hook 'compilation-filter-hook #'j|theme-color-compilation-buffer)

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

  (defun j|highlight-symbol-match-highlight-face ()
    "Make `highlight-symbol-face' look like `highlight'."
    (set-face-attribute 'highlight-symbol-face nil
                        :background nil
                        :foreground nil
                        :underline t)
    (with-eval-after-load 'go-guru
      (set-face-attribute 'go-guru-hl-identifier-face nil
                          :background nil
                          :foreground nil
                          :underline t
                          :inherit nil))
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
                 (not (memq major-mode '(typescript-mode
                                         go-mode))))
        (j|highlight-symbol-match-highlight-face)
        (highlight-symbol-mode 1))))

  (defun j|highlight-symbol-mode-setup ()
    "Bootstrap `highlight-symbol-mode'."
    ;; `tide-mode' already supplies a highlight.
    (unless (memq major-mode '(typescript-mode
                               go-mode))
      (j|highlight-symbol-match-highlight-face)
      (highlight-symbol-mode 1)))

  ;; Match after theme changes.
  (add-hook 'after-load-theme-hook #'j|highlight-symbol-match-highlight-face)

  (add-hook 'mhtml-mode-hook #'j|highlight-symbol-mode-setup)
  (add-hook 'prog-mode-hook #'j|highlight-symbol-mode-setup))

;; Themes
(use-package gotham-theme :defer :ensure t)
(use-package doom-themes :ensure t :defer
  :init
  (setq doom-solarized-light-brighter-comments t
        doom-solarized-light-comment-bg nil))

(use-package solarized-theme :defer :ensure t
  :config
  (setq solarized-distinct-fringe-background t
        solarized-use-less-bold t))

(use-package github-theme :defer :ensure t
  :config
  (set-face-attribute 'escape-glyph nil :bold t)
  (set-face-attribute 'font-lock-constant-face nil :bold t)
  (set-face-attribute 'font-lock-keyword-face nil :bold t))

(use-package spacemacs-theme
  :defer
  :ensure t
  :init
  (add-hook 'after-load-theme-hook
            (lambda ()
              (when (memq (car custom-enabled-themes)
                          '(spacemacs-light
                            spacemacs-dark))
                (let ((background (face-attribute 'default :background)))
                  (set-face-attribute
                   'line-number-current-line nil :background background)
                  (set-face-attribute
                   'line-number nil :background background)))))
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

(use-package habamax-theme :defer t :ensure t)

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

  (defun j|theme-changer-switch-theme (_ new)
    "Override `theme-changer-switch-theme' to disable all themes."
    (let ((new (if (listp new)
                   (elt new (random (length new)))
                 new))
          (enable (if (not (string= theme-changer-mode "deftheme"))
                      (lambda () (apply (symbol-function new) '()))
                    (lambda () (load-theme new t)))))
      (mapc #'disable-theme custom-enabled-themes)
      (if new (funcall enable))
      new))
  (advice-add 'theme-changer-switch-theme :override 'j|theme-changer-switch-theme)

  ;; Set up transparent titlebar on OSX.
  (when (eq system-type 'darwin)
    (defun j|theme-osx-setup (&optional frame)
      "Set up transparency in the titlebar and associated themes."
      (when (boundp 'ns-use-thin-smoothing)
        (setq ns-use-thin-smoothing nil))

      (set-frame-parameter frame 'ns-appearance nil)
      (set-frame-parameter frame 'ns-transparent-titlebar t)
      (if (or
           (string-match-p "-light$" (symbol-name (car custom-enabled-themes)))
           (memq (car custom-enabled-themes) '(solarized-light
                                               habamax)))
          (set-frame-parameter frame 'ns-appearance nil)
        (set-frame-parameter frame 'ns-appearance 'dark)))
    (j|theme-osx-setup)
    (add-hook 'after-load-theme-hook #'j|theme-osx-setup)
    (add-hook 'after-make-frame-functions #'j|theme-osx-setup))

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
    (change-theme 'spacemacs-light 'solarized-light))
   ((eq system-type 'gnu/linux)
    (change-theme 'spacemacs-light 'spacemacs-dark))
   (t
    (change-theme 'solarized-light 'solarized-light))))

(provide 'jn-theme)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-theme.el ends here

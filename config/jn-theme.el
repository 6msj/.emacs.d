;;;; -*- lexical-binding: t; -*-
(require 'jn-functions)

;;;; Minimal UI
(unless (and MAC-P
             window-system)
  (menu-bar-mode -1))

;;; Bootstrap

;; Add some advice for dynamic changing of theme.
(advice-add 'load-theme :after 'j-theme-run-hook)
(add-hook 'after-load-theme-hook #'j-theme-update)

;;;; Variables

(defvar j-font)

;;;; Helper Functions

(defun j-skip-loading-theme ()
  "Skip loading a theme and use the default theme."
  (j-theme-run-hook))

(defun j-theme-run-hook (&rest _args)
  "Run a theme hook."
  (run-hooks 'after-load-theme-hook))

(defun find-and-set-font (&rest candidates)
  "Set the first font found in CANDIDATES."
  (let ((font (cl-find-if (lambda (f)
                            (find-font (font-spec :name f)))
                          candidates)))
    (when font
      (setq j-font font)
      (set-face-attribute 'default nil :font font))
    font))

(defun j-theme-setup-font (fonts)
  "Set font by looking through list of FONTS.
FONTS is in this format
  '\(\(\"Hack\" . 11\)
    \(\"Consolas\" . 12\)
    \(\"Menlo\" . 12 \)\)."
  (apply #'find-and-set-font
         (mapcar (lambda (font)
                   (concat (car font) "-" (number-to-string
                                           (if DESKTOP-P
                                               (+ 0 (cdr font))
                                             (cdr font)))))
                 fonts)))

(defun j-theme-get-variable-pitch-font ()
  "Return variable pitch font to be used."
  (cond
   (MAC-P
    (if (string-match-p "Fira Code" j-font)
        (if (member "Fira Sans" (font-family-list))
            "Fira Sans"
          "Lucida Grande")
      (car (split-string j-font "-"))))
   ((eq system-type 'windows-nt)
    "Lucida Sans Unicode")
   (:default
    "Arial")))

(defun j-theme-update (&rest _args)
  "Update various UI elements when theme changes"
  (let* ((variable-pitch-font (j-theme-get-variable-pitch-font))
         (font-size (+ 5 (j-modeline-fontsize)))
         (family `(:family ,variable-pitch-font :height ,font-size)))
    (setq dired-sidebar-face family)
    (setq ibuffer-sidebar-face family))
  (j-theme-setup-modeline)
  (with-eval-after-load 'org-mode
    (j-org-customize-ui))
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
     (face-attribute 'company-preview-common :background))))

(defun j-theme-osx-setup (&optional frame)
  "Set up transparency in the titlebar and associated themes."
  (when (boundp 'ns-use-thin-smoothing)
    (setq ns-use-thin-smoothing nil))

  (set-frame-parameter frame 'ns-appearance nil)
  (set-frame-parameter frame 'ns-transparent-titlebar t)
  (if (or
       (null (car custom-enabled-themes)) ;; Default theme.
       (string-match-p "-light$" (symbol-name (car custom-enabled-themes)))
       (memq (car custom-enabled-themes) '(solarized-light
                                           habamax
                                           dichromacy)))
      (set-frame-parameter frame 'ns-appearance nil)
    (set-frame-parameter frame 'ns-appearance 'dark)))

;;;; Font

;; Double check this sytem type.
(when MAC-P
  ;; Use the osx emoji font for emoticons.
  (when (fboundp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      '(#x1F600 . #x1F64F)
                      (font-spec :name "Apple Color Emoji") nil 'prepend)))

(cond
 ((eq system-type 'windows-nt)
  (setq j-font "Consolas-11")
  (set-face-attribute 'default nil :font "Consolas-11")

  ;; Set font for CJK characters.
  ;; https://www.reddit.com/r/emacs/comments/84spld/emacs_not_playing_well_with_chinese_characters/
  (let ((font-pair
         ;; Some default fonts on Windows 10.
         ;; '("BIZ UDGothic" . 16)
         ;; '("Meiryo UI" . 16)
         ;; '("Meiryo" . 15)
         ;; '("Microsoft Yahei" . 15)
         ;; '("BIZ UDPGothic" . 16)
         ;; '("MS Gothic" . 16) ;; Too thin
         ;; '("MS Mincho" . 16) ;; Too jaggedy
         ;; '("MS PGothic" . 16) ;; Too jaggedy
         ;; '("MS UI Gothic" . 16) ;; Too jaggedy
         ;; '("UD Digi Kyokasho N-b" . 17) ;; Bolded
         ;; '("UD Digi Kyokasho NK-b" . 17) ;; Bolded
         ;; '("UD Digi Kyokasho NK-R" . 17)
         ;; '("UD Digi Kyokasho NP-B" . 17) ;; Bolded
         '("UD Digi Kyokasho NP-R" . 17)
         ;; '("UD Digi Kyokasho N-R" . 17)
         ;; '("Yu Gothic" . 17)
         ;; '("Yu Mincho" . 17)
         ))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset (font-spec :family (car font-pair)
                                           :size (cdr font-pair))))))
 (BSD-P
  (ignore))
 (LINUX-P
  (setq j-font "DejaVu Sans Mono-10")
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-10"))
 (:default
  ;; Set font only if in GUI.
  ;; If started from a terminal (through --daemon), we can't tell if it's a
  ;; graphical frame yet so check in after-make-frame-functions.
  (let ((fonts '(("Fira Code" . 14)
                 ("Fira Mono" . 14)
                 ("Ubuntu Mono" . 15)
                 ("M+ 1mn" . 12)
                 ("Monoisome" . 11)
                 ("Mononoki" . 12)
                 ("Oxygen Mono" . 13)
                 ("Noto Mono" . 9)
                 ("Ibm Plex Mono" . 13)
                 ("Hasklig" . 13)
                 ("Menlo" . 13)
                 ("SF Mono" . 11)
                 ("Consolas" . 12)
                 ("Hack" . 11)
                 ("Source Code Pro" . 13)
                 ("DejaVu Sans Mono" . 10)
                 ("PragmataPro" . 13)
                 ("Inconsolata-g for Powerline" . 14))))
    (if (display-graphic-p)
        (j-theme-setup-font fonts)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (when (display-graphic-p frame)
                    (with-selected-frame frame
                      (j-theme-setup-font fonts)))))))))

;; When inside tmux inside emacs on FreeBSD, colors are not set up correctly.
;; https://stackoverflow.com/questions/7617458/terminal-emacs-colors-only-work-with-term-xterm-256color
;; Set frame-background-mode to 'light to support light terminals.
(when BSD-P
  (setq frame-background-mode 'light)
  (frame-set-background-mode nil))

;;;; Modeline

(defun j-modeline-fontsize ()
  "Return font size of modeline."
  (+ 0 (* 10 (string-to-number (cadr (split-string j-font "-"))))))

(defun j-modeline-background-color (sym)
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

(defun j-theme-setup-modeline ()
  "Theme modeline."
  (interactive)
  (setq underline-minimum-offset 999)
  (set-frame-parameter (selected-frame) 'right-divider-width 1)
  (unless (member '(right-divider-width . 1) default-frame-alist)
    (push '(right-divider-width . 1) default-frame-alist))
  (let* ((font (j-theme-get-variable-pitch-font))
         (font-size (j-modeline-fontsize))
         (border-color (face-foreground 'window-divider (selected-frame) t))
         (underline `(:color ,border-color))
         (overline border-color))
    (dolist (sym '(mode-line mode-line-inactive))
      (set-face-attribute
       sym
       nil
       :family font
       :height font-size
       :box `(:line-width 5 :color ,(j-modeline-background-color sym))
       :underline underline
       :overline overline))))

;;;; Packaging

(use-package rainbow-delimiters
  ;; Use colorful parens.
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  ;; This function is not used currently.
  (defun j-rainbow-delimiters-bold-parens ()
    "Bold parentheses."
    (dotimes (i 9)
      (set-face-attribute
       (intern (format "rainbow-delimiters-depth-%d-face" (+ i 1))) nil :bold t)))
  (dolist (hook (j-lisp-hooks))
    (add-hook hook #'rainbow-delimiters-mode)))

;; Let Emacs color Ansi symbols.
;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(defun j-theme-color-compilation-buffer ()
  (ignore-errors
    (require 'ansi-color)
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))

(add-hook 'compilation-filter-hook #'j-theme-color-compilation-buffer)

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
  :init
  (defun j-highlight-symbol-mode-setup ()
    "Bootstrap `highlight-symbol-mode'."
    ;; `tide-mode' already supplies a highlight.
    (unless (memq major-mode '(typescript-mode
                               go-mode))
      (j-highlight-symbol-match-highlight-face)
      (highlight-symbol-mode 1)))
  :config
  (setq highlight-symbol-idle-delay .5)

  (defun j-highlight-symbol-match-highlight-face ()
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
        (j-highlight-symbol-match-highlight-face)
        (highlight-symbol-mode 1))))

  ;; Match after theme changes.
  (add-hook 'after-load-theme-hook #'j-highlight-symbol-match-highlight-face)

  (add-hook 'mhtml-mode-hook #'j-highlight-symbol-mode-setup)
  (add-hook 'prog-mode-hook #'j-highlight-symbol-mode-setup)
  (add-hook 'protobuf-mode-hook #'j-highlight-symbol-mode-setup))

;; Themes
(use-package kaolin-themes :defer :ensure t)
(use-package doom-themes
  :ensure t
  :defer t
  :config
  (setq doom-solarized-light-brighter-comments t
        doom-solarized-light-comment-bg nil))

(use-package solarized-theme :defer :ensure t
  :config
  (setq solarized-distinct-fringe-background t
        solarized-use-less-bold t))

(use-package spacemacs-theme
  :defer
  :ensure t
  :init
  (setq spacemacs-theme-comment-bg nil)
  (defun spacemacs-theme-wipe-display-line-number-color (&optional frame)
    "Wipe the gray from `spacemacs-theme'."
    (when (memq (car custom-enabled-themes)
                '(spacemacs-light spacemacs-dark))
      (let ((fr (or frame (selected-frame)))
            (background (face-attribute 'default :background)))
        (set-face-attribute
         'line-number-current-line fr :background background)
        (set-face-attribute
         'line-number fr :background background))))
  (add-hook 'after-make-frame-functions
            #'spacemacs-theme-wipe-display-line-number-color)
  (add-hook 'after-load-theme-hook
            #'spacemacs-theme-wipe-display-line-number-color))

(cond
 ((null window-system)
  (ignore))
 (MAC-P
  (add-hook 'after-load-theme-hook #'j-theme-osx-setup)
  (load-theme 'dichromacy t))
 (LINUX-P
  (j-skip-loading-theme))
 (t
  (load-theme 'solarized-light t)))

;; Make `diff-mode' look like `magit'.
(with-eval-after-load 'diff-mode
  (with-eval-after-load 'magit
    (set-face-attribute
     'diff-indicator-added nil
     :foreground nil :background nil :inherit 'magit-diff-added)
    (set-face-attribute
     'diff-indicator-removed nil
     :foreground nil :background nil :inherit 'magit-diff-removed)
    (set-face-attribute
     'diff-added nil
     :foreground nil :background nil :inherit 'magit-diff-added)
    (set-face-attribute
     'diff-removed nil
     :foreground nil :background nil :inherit 'magit-diff-removed)
    (set-face-attribute
     'diff-context nil
     :foreground nil :background nil :inherit 'magit-diff-context)
    (set-face-attribute
     'diff-file-header nil
     :foreground nil :background nil :inherit 'magit-diff-file-heading)
    (set-face-attribute
     'diff-header nil
     :foreground nil :background nil :inherit 'magit-diff-hunk-heading)
    (set-face-attribute
     'diff-changed nil
     :foreground nil :background nil :inherit 'magit-diff-hunk-region)))

;; Add this at the very end so we don't call `j-theme-update' twice.
(advice-add 'j-theme-setup-font :after #'j-theme-update)

(provide 'jn-theme)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-theme.el ends here

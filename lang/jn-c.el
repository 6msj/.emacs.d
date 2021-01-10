;;;; -*- lexical-binding: t; -*-

(require 'jn-dependencies)
(require 'jn-functions)

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
      (error nil))))

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

  (defun j-project-c-style ()
    "Determine c style of project."
    "k&r")

  (defun j-c-set-style ()
    "Set up indentation, format, etc."
    (if (or
         (derived-mode-p 'c++-mode)
         (derived-mode-p 'c-mode))
        (progn
          (c-set-style (j-project-c-style))
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

  (add-hook 'prog-mode-hook #'j-c-set-style))

(use-package cc-mode
  :ensure nil
  :mode
  ("\\.m\\'" . objc-mode)
  ("\\.mm\\'" . objc-mode)
  ("\\.xctool.args\\'" . objc-mode)
  :init
  ;; (add-hook 'objc-mode-hook
  ;;           (lambda ()
  ;;             (setq-local jit-lock-context-time .7)
  ;;             (setq-local jit-lock-defer-time .15)
  ;;             (setq-local jit-lock-stealth-nice .6)
  ;;             (setq-local jit-lock-stealth-time .6)))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal objc-mode-map
      (kbd "go") #'ff-find-other-file)))

(defun j-common-c-clear-bootstrap ()
  "Remove entries in `auto-mode-alist' that were set in this file."
  (setq auto-mode-alist (rassq-delete-all #'j-objc-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j-c++-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j-c-mode auto-mode-alist))
  (setq auto-mode-alist (rassq-delete-all #'j-java-mode auto-mode-alist)))

;;;###autoload
(defun j-objc-mode ()
  "Bootstrap `jn-c'."
  (j-common-c-clear-bootstrap)
  (objc-mode))

;;;###autoload
(defun j-c++-mode ()
  "Bootstrap `jn-c'."
  (j-common-c-clear-bootstrap)
  (c++-mode))

;;;###autoload
(defun j-c-mode ()
  "Bootstrap `jn-c'."
  (j-common-c-clear-bootstrap)
  (c-mode))

;;;###autoload
(defun j-java-mode ()
  "Bootstrap `jn-java'."
  (j-common-c-clear-bootstrap)
  (java-mode))

;; http://emacs.stackexchange.com/questions/508/how-to-configure-specific-java-indentation
(c-add-style
 "intellij"
 '("Java"
   (c-basic-offset . 4)
   (c-offsets-alist
    (inline-open . 0)
    (topmost-intro-cont    . +)
    (statement-block-intro . +)
    (knr-argdecl-intro     . 5)
    (substatement-open     . +)
    (substatement-label    . +)
    (label                 . +)
    (statement-case-open   . +)
    (statement-cont        . ++)
    (arglist-intro  . +)
    (arglist-close . c-lineup-arglist)
    (access-label   . 0)
    (inher-cont     . ++)
    (func-decl-cont . ++))))

(eval-after-load 'cc-vars
  (lambda ()
    (push '(java-mode . "intellij") c-default-style)
    (push '(c-mode . "k&r") c-default-style)))

(provide 'jn-c)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-c.el ends here

;;; objc-bracket.el --- Bracket handling for objective-c. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/objc-bracket
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Bracket handling for objective-c.

;;; Code:
(require 'smartparens)
(eval-when-compile (require 'subr-x))

(eval-and-compile
  (with-no-warnings
    (if (version< emacs-version "26")
        (progn
          (defalias 'objc-bracket-if-let* #'if-let)
          (defalias 'objc-bracket-when-let* #'when-let)
          (function-put #'objc-bracket-if-let* 'lisp-indent-function 2)
          (function-put #'objc-bracket-when-let* 'lisp-indent-function 1))
      (defalias 'objc-bracket-if-let* #'if-let*)
      (defalias 'objc-bracket-when-let* #'when-let*))))

(defun objc-bracket-handle-string-quotes (id action context)
  "Remove quotes when they were entered before or after a word.

If quotes were entered after a word, check if that word is a @.

Since @ is a string literal, don't remove the quote."
  (when (eq action 'insert)
    (save-excursion
      (forward-char 1)
      (when (sp-point-before-word-p id action context)
        (delete-char -1)))
    (save-excursion
      (when (sp-point-after-word-p id action context)
        (unless (save-excursion
                  (forward-char -1)
                  (sp--looking-back-p (regexp-quote "@")))
          (delete-char 1))))))

(defun objc-bracket-insert-right-bracket (id action context)
  (interactive)
  (save-excursion
    (forward-char 1)
    (when (or
           (sp-point-before-same-p id action context)
           (sp-point-before-word-p id action context))
      (delete-char -1)
      (forward-word 1)
      (insert "]"))))

(defun objc-bracket-maybe-insert-left-bracket-filter (_)
  "Return CMD if at start of unmodified snippet field.
Use as a `:filter' argument for a conditional keybinding."
  (unless (looking-at-p "]") #'objc-bracket-insert-left-bracket))

(defconst objc-bracket-maybe-insert-left-bracket
  '(menu-item "" nil
              :filter objc-bracket-maybe-insert-left-bracket-filter)
  "A conditional key definition.

Apply `objc-bracket-maybe-insert-left-bracket-filter'.")

(defun objc-bracket-evil-setup ()
  "Set up `objc-bracket' for `evil'."
  ;; Left bracket handling.
  (with-eval-after-load 'evil
    (evil-define-key 'insert objc-mode-map "]" objc-bracket-maybe-insert-left-bracket))

  (sp-with-modes '(objc-mode)
    ;; Right bracket handling.
    (sp-local-pair  "[" "]"
                    :unless '(sp-in-string-p)
                    :post-handlers '(objc-bracket-insert-right-bracket))
    ;; Quotation handling.
    (sp-local-pair "\"" "\""
                   :post-handlers '(objc-bracket-handle-string-quotes))))

(defun objc-bracket-insert-left-bracket ()
  "Add a matching left bracket to pair with right bracket.

Try to find the first left bracket of a string of brackets and
insert left bracket there.

*insert here*[[[]] *original point*]"
  (interactive)
  (let ((inserted-bracket-blank-or-bracket nil))
    (when (objc-bracket-not-in-string-p)
      (save-excursion
        (forward-char -1)
        (let ((current-char (string (char-after))))
          (message current-char)
          (when (or
                 (string-equal current-char "]")
                 (string-equal current-char " "))
            (setq inserted-bracket-blank-or-bracket t)))))
    (if inserted-bracket-blank-or-bracket
        (insert " ]")
      (insert "]"))
    (when (objc-bracket-not-in-string-p)
      (catch 'done
        (save-excursion
          (forward-char -1)
          (while t
            (objc-bracket-if-let*
             ((equal-position (objc-bracket-equal-in-line-p)))
             (progn
               (beginning-of-line)
               (forward-char (+ equal-position 2))
               (insert "[")
               (indent-according-to-mode)
               (throw 'done t))
             (if (objc-bracket-should-check-previous-line-p)
                 (forward-line -1)
               (back-to-indentation)
               (insert "[")
               (indent-according-to-mode)
               (throw 'done t))))))
      (if inserted-bracket-blank-or-bracket
          (forward-char -1)
        (unless (point-at-eol)
          (forward-char 1))))))

(defun objc-bracket-current-line ()
  "Return current line."
  (string-trim-right (thing-at-point 'line t)))

(defun objc-bracket-not-in-string-p ()
  "Return true if point is not in a string."
  (not (nth 3 (syntax-ppss))))

(defun objc-bracket-current-line-number ()
  "Return current line number."
  (string-to-number (format-mode-line "%l")))

(defun objc-bracket-equal-in-line-p ()
  "Check if current line contains an equal sign.

Return the character position if so. "
  (string-match "=" (objc-bracket-current-line)))

(defun objc-bracket-should-check-previous-line-p ()
  "Check if previous line should be checked for a bracket.

If previous line doesn't end with ;, the current line is considered a
continuation of the previous line."
  (save-excursion
    (forward-line -1)
    (let ((line (objc-bracket-current-line)))
      (and
       (not (string-match-p "^\\s-*$" line))
       (not (string-match-p ";$" line))))))

(provide 'objc-bracket)
;;; objc-bracket.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:

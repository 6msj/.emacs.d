;;;; -*- lexical-binding: t; -*-
;;; notmuch-modeline.el --- Display notmuch unread count in modeline. -*- coding: utf-8 -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
(require 'subr-x)
(require 'ezimage)

(defgroup notmuch-modeline nil
  "Display `notmuch' unread count in modeline."
  :prefix "notmuch-modeline-"
  :group 'hardware)

(defcustom notmuch-modeline--update-interval 60
  "Update interval for when to check for unread count."
  :type 'integer
  :group 'notmuch-modeline)

(defvar notmuch-modeline--update-timer nil
  "Interval timer object.")

(defcustom notmuch-modeline--count-command
  (concat "notmuch" " search --output=messages tag:unread|wc -l")
  "Command to retrieve count of emails in `notmuch'."
  :type 'string
  :group 'notmuch-modeline)

(defvar notmuch-modeline--mode-line-string nil
  "String to display in the mode line.")
(put 'notmuch-modeline--mode-line-string 'risky-local-variable t)

(define-minor-mode notmuch-modeline-mode
  "Toggle display of `notmuch' unread count in modeline.
With a prefix argument ARG, enable display notmuch-modeline-mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t :group 'notmuch-modeline
  (setq notmuch-modeline--mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and notmuch-modeline--update-timer (cancel-timer notmuch-modeline--update-timer))

  (if notmuch-modeline-mode
      (progn
        (add-to-list 'global-mode-string 'notmuch-modeline--mode-line-string t)
        (setq notmuch-modeline--update-timer
              (run-at-time nil notmuch-modeline--update-interval
                           #'notmuch-modeline--update-with-delay))
        (advice-add
         'notmuch-bury-or-kill-this-buffer
         :after #'notmuch-modeline--update-with-delay))
    (advice-remove 'notmuch-bury-or-kill-this-buffer
                   #'notmuch-modeline--update-with-delay)
    (setq global-mode-string
          (delq 'notmuch-modeline--mode-line-string global-mode-string))
    (notmuch-modeline--update)))

(defun notmuch-modeline--update ()
  (interactive)
  (notmuch-modeline--async-shell-command-to-string
   notmuch-modeline--count-command
   (lambda (result)
     (let ((old-string notmuch-modeline--mode-line-string)
           (unread (string-to-number (string-trim result))))
       (setq notmuch-modeline--mode-line-string
             (notmuch-modeline--mode-line-formatter unread))
       (unless (string= notmuch-modeline--mode-line-string old-string)
         (force-mode-line-update))))))

(defun notmuch-modeline--update-with-delay ()
  "Update notmuch-modeline after idle delay."
  (interactive)
  (run-with-idle-timer 1 nil #'notmuch-modeline--update))

(defun notmuch-modeline--mode-line-formatter (mail-count)
  "Default formatter used to get the string to be displayed in the mode-line.
MAIL-COUNT is the count of mails for which the string is to displayed"
  (if (zerop mail-count)
      ""
    (concat
     " "
     (propertize
      "Mail"
      'display (when (display-graphic-p) ezimage-mail)
      'help-echo (concat
                  (if (= mail-count 1)
                      "You have an unread email"
                    (format "You have %s unread emails." mail-count))
                  "\nClick here to view "
                  (if (= mail-count 1) "it." "them."))
      'keymap '(mode-line keymap
                          (mouse-1 . (lambda ()
                                       (interactive)
                                       (notmuch-search "tag:unread"))))))))

(defun notmuch-modeline--async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background.
Return the temporary output buffer which command is writing to
during execution.
When the command is finished, call CALLBACK with the resulting
output as a string.
Synopsis:
  (notmuch-modeline--async-shell-command-to-string \"echo hello\" (lambda (s) (message \"RETURNED (%s)\" s)))
"
  (let ((output-buffer (generate-new-buffer " *temp*")))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(provide 'notmuch-modeline)

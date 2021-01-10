;;;; -*- lexical-binding: t; -*-

(require 'jn-functions)

(use-package company
  :ensure t
  ;; :load-path "~/.emacs.d/fork/company-mode"
  :init
  ;; https://github.com/company-mode/company-mode/issues/205
  (defun company--unique-match-p/not-yasnippet (candidates _prefix _ignore-case)
    (not (get-text-property 0 'yas-template (car candidates))))

  (advice-add 'company--unique-match-p :after-while
              'company--unique-match-p/not-yasnippet)

  (defun j-company-backend-in-backends (b)
    "Check if backend B is already in `company-backends'."
    (if (member b company-backends)
        t
      (let ((in-backend nil))
        (dolist (backend company-backends)
          (when (and
                 (consp backend)
                 (member b backend))
            (setq in-backend t)))
        in-backend)))

  (defun j-company-push-backend (backend &optional local append)
    "Adds backend BACKEND to company mode if it's not already in the list of backends.
IF APPEND, add to end of list."
    (when local
      (make-local-variable 'company-backends))
    (unless (j-company-backend-in-backends backend)
      (if append
          (setcdr (last company-backends) (list backend))
        (push backend company-backends))
      (j-company-merge-backends)))

  (defun company-merge-with-backends (backend-to-merge)
    "Merges a backend with every backend in company-backends.
BACKEND-TO_MERGE will only be merged if it's not already in the current backend.

Merging `company-yasnippet' to `company-capf' will yield
'\(company-capf :with company-yasnippet\)."
    (setq company-backends
          (mapcar (lambda (backend)
                    (cond
                     ((and (listp backend)
                           (member backend-to-merge backend))
                      backend)
                     (:else
                      (append (if (consp backend)
                                  backend
                                (list backend))
                              (if (and (listp backend)
                                       (member :with backend))
                                  `(,backend-to-merge)
                                `(:with ,backend-to-merge))))))
                  company-backends)))

  (defun j-company-merge-backends ()
    "Merge common backends."
    (interactive)
    (company-merge-with-backends 'company-yasnippet)
    (company-merge-with-backends 'company-dabbrev-code))
  :config
  (when YT-P
    (setq company-backends (delete 'company-files company-backends)))
  (setq company-backends (delete 'company-clang company-backends))

  ;; With this in the backend list, it will automatically save the buffer
  ;; when typing.
  (setq company-backends (delete 'company-eclim company-backends))

  (setq company-dabbrev-downcase nil
        company-dabbrev-code-ignore-case t
        company-dabbrev-ignore-case 'keep-prefix
        ;; Dabbrev all code buffers together.
        company-dabbrev-code-other-buffers 'code
        ;; Only search same major mode buffers.
        company-dabbrev-other-buffers t
        ;; Don't take too much time searching for matches.
        company-dabbrev-code-time-limit .0005
        company-dabbrev-time-limit .0005)
  (setq company-lighter-base "/"
        company-tooltip-align-annotations t
        company-echo-delay 1
        company-selection-wrap-around t
        company-idle-delay .05
        company-minimum-prefix-length 1)

  (defun j-company-remove-dabbrev-dups-keep-order (candidates)
    "Loop over CANDIDATES and remove duplicate candidates if they belong to
  `company-dabbrev' or `company-dabbrev-code'."
    (let ((hash (make-hash-table :test 'equal :size (length candidates)))
          (new-list nil))
      (dolist (candidate candidates)
        (let ((stripped-candidate (substring-no-properties candidate))
              (candidate-backend (get-text-property 0 'company-backend candidate)))
          (cond
           ;; Candidate is `company-yasnippet', always push this.
           ((eq (get-text-property 0 'company-backend candidate)
                'company-yasnippet)
            (push candidate new-list))
           ;; Candidate has not been seen.
           ((not (gethash stripped-candidate hash))
            (puthash stripped-candidate candidate hash)
            (push candidate new-list))
           ;; Candidate has been seen.
           ;; `company-dabbrev' or `company-dabbrev-code' is the candidate.
           ((or candidate-backend
                (eq candidate-backend 'company-dabbrev-code)
                (eq candidate-backend 'company-dabbrev))
            t)
           ;; Candidate has been seen but is not `company-dabbrev'
           ;; or `company-dabbrev-code'.
           (:seen-but-candidate-not-dabbrev
            ;; If the candidate in the hash table is dabbrev, replace it.
            ;; Otherwise, we are fine with duplicates as long as the backends
            ;; are meaningful.
            (let* ((hash-candidate (gethash stripped-candidate hash))
                   (hash-backend (get-text-property
                                  0 'company-backend hash-candidate)))
              (if (or hash-backend
                      (eq hash-backend 'company-dabbrev)
                      (eq hash-backend 'company-dabbrev-code))
                  (progn
                    (setcar
                     (nthcdr
                      (cl-position hash-candidate new-list :test 'eq)
                      new-list)
                     candidate)
                    (puthash stripped-candidate candidate hash)
                    t)
                ;; Only need one candidate in the hash table.
                (push candidate new-list)))))))
      (reverse new-list)))

  (push #'company-sort-by-backend-importance company-transformers)
  (push #'company-sort-prefer-same-case-prefix company-transformers)
  (push #'j-company-remove-dabbrev-dups-keep-order company-transformers)

  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Don't enable `company-mode' for remote files.
              (when (file-remote-p default-directory)
                (company-mode -1))))

  (defun j-company-tng-configure-default ()
    "Applies the default configuration to enable `company-tng'."
    ;; Copied from `company-tng-mode'.
    (advice-add #'eglot--snippet-expansion-fn :override #'ignore)
    (setq company-selection-default nil)
    (setq company-require-match nil)

    (defun j-company-expand-yasnippet-or-return ()
      "Expand yas template or call RET normally.

If `yasnippet' expansion in progress, always complete selection."
      (interactive)
      (cond
       ;; `yasnippet' expansion in progress.
       ((yas-current-field)
        (call-interactively #'company-complete-selection))
       ;; Check the type of the candidate.
       ((and company-active-map
             company-selection-changed
             (car company-candidates) ;; Making sure there are candidates.
             (let ((candidate (nth company-selection
                                   company-candidates)))
               (or
                ;; This might work for `eglot'.
                ;; (get-text-property 0 :label candidate)
                ;; `company-lsp'
                (get-text-property 0 'lsp-completion-item candidate)
                ;; `company-sourcekit'
                (get-text-property 0 'sourcetext candidate)
                ;; `yasnippet'
                (get-text-property 0 'yas-template candidate))))
        (call-interactively #'company-complete-selection))
       (:default
        (when company-selection-changed
          (company-complete-selection))
        (let ((company-active-map))
          (call-interactively (key-binding (kbd "RET")))))))

    (setq company-frontends
          '(company-tng-frontend
            company-pseudo-tooltip-unless-just-one-frontend
            company-echo-metadata-frontend
            company-preview-unless-selection
            company-quickhelp-frontend))

    ;; https://github.com/company-mode/company-mode/pull/706
    (defun company-preview-unless-selection (command)
      "`company-preview-frontend', but only if there's no selection"
      (unless (and (eq command 'post-command)
                   company-selection)
        (pcase command
          (`pre-command (company-preview-hide))
          (`post-command
           (company-preview-show-at-point (point)
                                          (nth 0 company-candidates)))
          (`hide (company-preview-hide)))))

    (defvar company-cancel-or-backspace
      `(menu-item "" nil :filter ,(lambda (&optional _)
                                    (when company-selection
                                      #'company-abort)))
      "If we made a selection during company completion, cancel it.")

    (let ((keymap company-active-map))
      (define-key keymap [return] 'j-company-expand-yasnippet-or-return)
      (define-key keymap (kbd "RET") 'j-company-expand-yasnippet-or-return)
      (define-key keymap "\C-n" 'company-select-next-if-tooltip-visible-or-complete-selection)
      (define-key keymap [tab] 'company-select-next-if-tooltip-visible-or-complete-selection)
      (define-key keymap (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection)
      (define-key keymap [S-tab] 'company-select-previous)
      (define-key keymap [S-iso-lefttab] 'company-select-previous)
      (define-key keymap [(shift tab)] 'company-select-previous)
      (define-key keymap [backtab] 'company-select-previous)
      (define-key keymap "\C-p" 'company-select-previous)
      (define-key keymap [backtab] 'company-select-previous)
      (define-key keymap (kbd "S-TAB") 'company-select-previous)
      (define-key keymap (kbd "DEL") company-cancel-or-backspace)
      (define-key keymap [backspace] company-cancel-or-backspace)
      (define-key keymap (kbd "<backspace>") company-cancel-or-backspace))

    (defun j-company-complete-selection ()
      "Insert the selected candidate even if the user hasn't selected anything.
The first candidate will be DWIM."
      (interactive)
      (when (company-manual-begin)
        (let ((result (nth (if company-selection
                               company-selection
                             0)
                           company-candidates)))
          (company-finish result))))
    (advice-add 'company-complete-selection :override 'j-company-complete-selection))

  (j-company-merge-backends)
  (j-company-tng-configure-default)
  (global-company-mode))

(use-package company-quickhelp
  ;; Documentation popup for company.
  :ensure t
  :commands (company-quickhelp-mode)
  :init
  (add-hook 'company-mode-hook #'company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 2.0))

(use-package company-shell
  ;; Completion for use with terminal modes.
  :ensure t
  :commands company-shell)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (defun j-yasnippet-expansion-buffer-name ()
    "Expands buffer's filename into a name without its extension."
    (file-name-sans-extension (buffer-name)))

  ;; https://github.com/joaotavora/yasnippet/issues/289
  (add-hook 'term-mode-hook (lambda ()
                              (yas-minor-mode -1)))
  :config
  ;; When yasnippet gets an empty prefix, (can happen in a grouped backend)
  ;; don't trigger it if the prefix comes back with an empty string.
  (advice-add 'company-yasnippet :around #'j-company-yasnippet)

  (defun j-company-yasnippet (orig-fun &rest args)
    "`company-mode' backend for `yasnippet'."
    (interactive (list 'interactive))
    (cl-case (nth 0 args)
      (prefix
       (and (bound-and-true-p yas-minor-mode)
            (let ((company-symbol (company-grab-symbol)))
              (if (string-equal company-symbol "")
                  nil
                company-symbol))))
      (t
       (apply orig-fun args))))

  ;; Add to end so local snippets take priority.
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/fork/yasnippet-snippets/snippets" t)

  ;; Yas messages stretches the status buffer when it starts up.
  (setq yas-verbosity 0)

  ;; Disable in normal mode in `evil-mode'.
  ;; Expansion generally happens in insert mode and this TAB key conflicts with
  ;; `hs-minor-mode'.
  (with-eval-after-load 'evil
    (define-key yas-minor-mode-map [(tab)] nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (evil-define-key 'insert yas-minor-mode-map [(tab)] yas-maybe-expand)
    (evil-define-key 'insert yas-minor-mode-map (kbd "TAB") yas-maybe-expand))

  (yas-global-mode 1))

(provide 'jn-autocomplete)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-autocomplete.el ends here

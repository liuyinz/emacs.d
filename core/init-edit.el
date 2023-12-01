;;; init-edit.el --- edit setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; --------------------------- Edit -------------------------------

(leaf atomic-chrome
  :init
  (setq atomic-chrome-buffer-open-style 'split)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("reddit\\.com" . gfm-mode)
          ("emacs-china\\.org" . gfm-mode)
          ("stackexchange\\.com" . gfm-mode)
          ("stackoverflow\\.com" . gfm-mode))))

(leaf hungry-delete
  :hook (after-init-hook . global-hungry-delete-mode)
  :defer-config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

(leaf pangu-spacing
  :hook (after-init-hook . global-pangu-spacing-mode)
  :init
  (setq pangu-spacing-real-insert-separtor t)
  :defer-config
  (push 'dired-mode pangu-spacing-inhibit-mode-alist))

(leaf coercion :require t)

(leaf powerthesaurus)

;; SEE https://www.python.org/dev/peps/pep-0350/#mnemonics
(leaf hl-todo
  :hook (after-init-hook . global-hl-todo-mode)
  :defer-config (prependq! hl-todo-include-modes '(conf-mode))
  :init
  (setq hl-todo-wrap-movement t)
  (setq hl-todo-keyword-faces
        '(;; Informal tasks/features that are pending completion
          ("TODO"       . "#afd8af")
          ;; Problematic or ugly code needing refactoring or cleanup
          ("FIXME"      . "#cc9393")
          ;;Temporary code to force inflexible functionality
          ("HACK"       . "#d0bf8f")
          ;; Portability for specific OS, Python version,etc
          ("WORKAROUND" . "#7cb8bb")
          ;; Satisfactions of specific, formal requirements.
          ("REQUIRE"    . "#7cb8bb")
          ;; Pointers to other code, web link, etc
          ("SEE"        . "#99d0f6")
          ;; Code shared on Github
          ("PR"         . "#dc8cc3")
          ("ISSUE"      . "#dc8cc3")
          ("DISCUSSION" . "#dc8cc3")
          ;; Reviewer thinks needs more discussion
          ("NOTE"       . "#dc8cc3")
          ;; Problems
          ("XXX"        . "#cc9393")
          ("BUG"        . "#ff665c")
          ("WONTFIX"    . "#8c5353")
          ;; Remove since version X.x.x
          ("DEPRECATED" . "#8c5353")
          )))

(leaf consult-todo
  :init
  (setq consult-todo-narrow
        '((?t . "TODO")
          (?f . "FIXME")
          (?h . "HACK")
          (?k . "WORKAROUND")
          (?r . "REQUIRE")
          (?s . "SEE")
          (?p . "PR")
          (?i . "ISSUE")
          (?c . "DISCUSSION")
          (?n . "NOTE")
          (?x . "XXX")
          (?b . "BUG")
          (?w . "WONTFIX")
          (?d . "DEPRECATED"))))

(leaf edit-indirect
  :commands edit-indirect-buffer-indirect-p
  :init
  (setq edit-indirect-mode-map nil)

  (defun my/edit-indirect ()
    "Edit indirectly according to `major-mode'"
    (interactive)
    (pcase major-mode
      ('snippet-mode (yas-edit-elisp-indirect))
      ((or 'markdown-mode 'gfm-mode) (markdown-edit-code-block))
      ('jsonian-mode (jsonian-edit-string))
      (_ nil)))
  )

(leaf wgrep
  :init
  (setq wgrep-change-readonly-file t)
  (setq wgrep-auto-save-buffer t))

(leaf rg
  :hook (rg-mode-hook . (lambda ()
                          (setq-local compilation-scroll-output 'first-error
                                      compilation-always-kill t)))
  :bind
  ("C-c s" . rg-menu)
  (:rg-mode-map
   ("<"    . compilation-first-error)
   (">"    . compilation-last-error)
   ("n"    . compilation-next-error)
   ("p"    . compilation-previous-error)
   ("\C-o" . compilation-display-error)
   ("\C-n" . next-error-no-select)
   ("\C-p" . previous-error-no-select)
   ("N"    . rg-next-file)
   ("P"    . rg-prev-file)
   ("w"    . rg-forward-history)
   ("b"    . rg-back-history)
   ("R"    . rg-replace)
   ("?"    . rg-menu)
   )

  :init
  (setq rg-ignore-case 'smart
        rg-command-line-flags '("-z" "--pcre2"))

  :defer-config

  ;; FIXME emacs regexp not support, use rg-match-face to replace in future
  ;; BUG undo when wgrep finished ?
  (defun rg-replace ()
    "Replace current search in Rg-mode."
    (interactive)
    (save-excursion
      (wgrep-change-to-wgrep-mode)
      (unwind-protect
          (let* ((literal (rg-search-literal rg-cur-search))
                 (pattern (rg-search-pattern rg-cur-search))
                 (cmd (if literal 'query-replace 'query-replace-regexp))
                 (prompt (concat "Query replace" (unless literal " regexp")))
                 (from (if literal pattern (query-replace-read-from prompt t)))
                 (to (query-replace-read-to from prompt nil)))
            (funcall cmd from to))
        (wgrep-finish-edit))))
  (rg-menu-transient-insert "Rerun" "R" "Replace" #'rg-replace))

;; NOTE command-key [super] couldn't identifiled in emacs -nw
(leaf simpleclip
  :init
  (setq simpleclip-less-feedback t
        simpleclip-unmark-on-copy t)
  :hook (after-init-hook . simpleclip-mode))

(leaf copy-as-format
  :require t
  :bind
  ("s-C" . simpleclip-copy-as-format)
  :init
  (defun simpleclip-copy-as-format ()
    "Copy the current line or active region and add it to the system clipboard as
GitHub/Slack/JIRA/HipChat/... formatted code.  Format defaults to
`copy-as-format-default'.  The buffer will not be modified.
With a prefix argument prompt for the format."
    (interactive)
    (let* ((text (copy-as-format--extract-text))
           (format (if current-prefix-arg
                       (completing-read "Format: "
                                        (mapcar 'car copy-as-format-format-alist)
                                        nil t "" nil copy-as-format-default)
                     copy-as-format-default))
           (func (cadr (assoc format copy-as-format-format-alist))))
      (when (eq (length text) 0)
        (error "No text selected"))
      (when (not (fboundp func))
        (error "Missing or invalid format function for `%s'" format))
      (simpleclip-set-contents (funcall func text (use-region-p)))
      (setq deactivate-mark t)))
  )

(provide 'init-edit)
;;; init-edit.el ends here

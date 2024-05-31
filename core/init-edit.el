;;; init-edit.el --- edit setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; --------------------------- Edit -------------------------------

(leaf atomic-chrome
  :hook (after-init-hook . atomic-chrome-start-server)
  :init
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("reddit\\.com" . gfm-mode)
          ("emacs-china\\.org" . gfm-mode)
          ("stackexchange\\.com" . gfm-mode)
          ("stackoverflow\\.com" . gfm-mode)
          ("react.docschina.org" . jtsx-jsx-mode)
          ("react.dev" . jtsx-jsx-mode)
          )))

(leaf hungry-delete
  :hook (after-init-hook . global-hungry-delete-mode)
  :defer-config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

(leaf pangu-spacing
  :hook (after-init-hook . global-pangu-spacing-mode)
  :init
  (setq pangu-spacing-real-insert-separtor t)
  :defer-config
  (push 'dired-mode pangu-spacing-inhibit-mode-alist))

;; (leaf super-save
;;   :hook (after-init-hook . super-save-mode)
;;   :init
;;   (setq super-save-auto-save-when-idle t
;;         super-save-all-buffers t
;;         super-save-idle-duration 10
;;         super-save-silent t))

(leaf coercion
  :bind-keymap
  ("C-c c" . coercion-command-map))

;; SEE https://www.python.org/dev/peps/pep-0350/#mnemonics
(leaf hl-todo
  :hook (after-init-hook . global-hl-todo-mode)
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
  (defun my/edit-indirect ()
    "Edit indirectly according to `major-mode'"
    (interactive)
    (pcase major-mode
      ('snippet-mode (yas-edit-elisp-indirect))
      ((or 'markdown-mode 'gfm-mode) (markdown-edit-code-block)))))

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
   ("?"    . rg-menu))

  :init
  (setq rg-ignore-case 'smart
        rg-command-line-flags '("-z" "--pcre2"))

  :defer-config

  ;; FIXME replace failed when rg search with --multiline
  ;; now use `query-replace-regexp' to replace \n (`C-q C-j') first
  (defun rg-replace (to-string)
    "Replace matched result in rg-mode buffer."
    (interactive (list (read-string "Rg replace reulst with: ")))
    (let ((final-pos (point)))
      (unwind-protect
          (let* ((keep-asking t)
                 (stop-replace nil)
                 (prompt (format "Replace match string with %s: (y,n,q,!,.) ?" to-string)))
            (cl-flet ((replace-func (prop)
                        (delete-region (prop-match-beginning prop) (prop-match-end prop))
                        (insert to-string)))
              (wgrep-change-to-wgrep-mode)
              (while (and (not stop-replace)
                          (setq cur-match (text-property-search-forward
                                           'face 'rg-match-face t)))
                (if keep-asking
                    (let ((illegal-key t))
                      (while illegal-key
                        (let ((key (single-key-description (read-key prompt) t)))
                          (pcase key
                            ("!" (setq keep-asking nil))
                            ((or "q" "RET" "C-g" "." "ESC") (setq stop-replace t)))
                          (when (member key '("!" "y" "Y" "." "SPC"))
                            (replace-func cur-match))
                          (when (member key '("!" "Y" "y" "N" "n"
                                              "DEL" "." "q" "RET" "C-g"))
                            (setq illegal-key nil))))
                      (setq final-pos (point)))
                  (replace-func cur-match)))))
        (wgrep-finish-edit)
        (goto-char final-pos))))

  ;; light theme background: "#F0F3F4i"
  ;; parse nippon-color
  ;;   (let ((json-object-type 'plist)
  ;;     (json-array-type 'list)
  ;;     )
  ;; (with-current-buffer (get-buffer-create "temp.el")
  ;;   (erase-buffer)
  ;;   (let ((print-level nil)
  ;;         (print-length nil)
  ;;         (fill-column 130))
  ;;     (pp (json-read-file (buffer-file-name (get-buffer "nippon-color.json")))
  ;;         (current-buffer)))
  ;;   )
  ;; )


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

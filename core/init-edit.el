;;; init-edit.el --- edit setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; --------------------------- Edit -------------------------------

(leaf hungry-delete
  :hook (after-init-hook . global-hungry-delete-mode)
  :defer-config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

(leaf isolate
  :require t
  :defer-config
  (add-hook 'isolate-add-mode-hook #'my/meow-motion-temporary))

(leaf writeroom-mode
  :hook (writeroom-mode-hook . toggle-cjk-writeroom)
  :init
  (setq writeroom-width 120)

  ;; toggle modes according to writeroom-mode
  (defvar writeroom-toggle-modes '((diff-hl-mode . nil)))
  (add-hook 'writeroom-mode-hook (lambda () (mode-hook-toggle
                                             writeroom-mode
                                             writeroom-toggle-modes)))

  (defun toggle-cjk-writeroom ()
    (interactive)
    (if (bound-and-true-p writeroom-mode)
        (cjk-font-setting "Source Han Serif" 1.4)
      (cjk-font-setting "Sarasa Mono SC" 1))))

(leaf topsy
  :hook (emacs-lisp-mode-hook . topsy-mode))

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
          ("XXX+"       . "#cc9393")
          ("BUG"        . "#ff665c")
          ("WONTFIX"    . "#8c5353")
          ;; Remove since version X.x.x
          ("DEPRECATED" . "#8c5353")
          )))

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
  ((kbd "C-c s") . rg-menu)
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

;; --------------------------- Undo -------------------------------

(leaf undohist
  :hook (after-init-hook . undohist-initialize)
  :init (setq undohist-ignored-files '("\\.git/COMMIT_EDITMSG$")))

(leaf undo-hl
  :hook ((text-mode-hook prog-mode-hook) . undo-hl-mode)
  :defer-config
  (appendq! undo-hl-undo-commands '(meow-undo))
  (setq undo-hl-flash-duration 0.2))

(leaf vundo
  :init
  (setq vundo-window-max-height 5)
  :bind
  ("C-c u" . vundo)
  (:vundo-mode-map
   :package vundo
   ("l" . vundo-forward)
   ("h" . vundo-backward)
   ("j" . vundo-next)
   ("k" . vundo-previous)))

;; --------------------------- Jump -------------------------------

(leaf avy
  :hook (after-init-hook . avy-setup-default)
  :defer-config
  (setq avy-all-windows t
        avy-all-windows-alt t
        avy-background t
        avy-style 'at-full
        avy-keys '(?a ?s ?d ?f ?h ?j ?k ?l ?q ?u ?w ?i ?e ?o))
  (setq avy-orders-alist
        '((avy-goto-char . avy-order-closest)
          (avy-goto-word-0 . avy-order-closest)
          (avy-goto-paren . avy-order-closest)))

  ;; SEE https://github.com/abo-abo/avy/wiki/custom-commands#jumping-to-an-open-paren
  ;; https://stackoverflow.com/a/50063226/13194984
  (defun avy-goto-paren ()
    "Avy jump to paren."
    (interactive)
    (let ((avy-command this-command))
      (avy-jump "[][(){}]")))

  ;; Pinyin support
  (leaf ace-pinyin
    :init (setq ace-pinyin-simplified-chinese-only-p nil)
    :defer-config (ace-pinyin-global-mode +1))
  )

(provide 'init-edit)
;;; init-edit.el ends here

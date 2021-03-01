;;; init-edit.el --- edit setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

;; Hungry deletion
(leaf hungry-delete
  :hook (after-init-hook . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Jump to things in Emacs tree-style
(leaf avy
  :hook (after-init-hook . avy-setup-default)
  :config
  (setq avy-all-windows t
        avy-all-windows-alt t
        avy-background nil
        avy-style 'at-full
        avy-keys '(?a ?s ?d ?f ?h ?j ?k ?l ?q ?u ?w ?i ?e ?o))
  (setq avy-orders-alist
        '((avy-goto-char . avy-order-closest)
          (avy-goto-word-0 . avy-order-closest)
          (avy-goto-parens . avy-order-closest)))

  ;; HACK go-to paren
  ;; @https://github.com/abo-abo/avy/wiki/custom-commands#jumping-to-an-open-paren
  ;; @https://stackoverflow.com/a/50063226/13194984
  (defun avy-goto-paren ()
    (interactive)
    (let ((avy-command this-command))   ; for look up in avy-orders-alist
      (avy-jump "[]\[(){}]")))
  )

;; undo-redo
(leaf undo-fu :require t)
(leaf undo-fu-session
  :after undo-fu
  :hook (after-init-hook . global-undo-fu-session-mode))

;; Flexible text folding
(leaf origami
  :hook (prog-mode-hook . origami-mode)
  :init (setq origami-show-fold-header t)
  :config (face-spec-reset-face 'origami-fold-header-face))

(leaf command-log-mode
  :commands (global-command-log-mode clm/toggle-command-log-buffer)
  :init
  (setq command-log-mode-open-log-turns-on-mode t
        command-log-mode-is-global t
        command-log-mode-window-size 40))

(leaf wgrep
  :hook (grep-mode-hook . wgrep-change-to-wgrep-mode)
  :init
  (setq wgrep-change-readonly-file t)
  (setq wgrep-auto-save-buffer t))

(leaf color-rg
  :commands (color-rg-search-project-with-type
             color-rg-search-input
             color-rg-search-input-in-project
             color-rg-search-input-in-current-file)
  :init
  (setq color-rg-mac-load-path-from-shell nil
        color-rg-max-column 6000))

(provide 'init-edit)
;;; init-edit.el ends here

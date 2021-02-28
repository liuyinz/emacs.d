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
          (avy-goto-word-0 . avy-order-closest))))

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

(leaf awesome-pair
  :hook ((c-mode-common-hook
          c-mode-hook
          c++-mode-hook
          java-mode-hook
          haskell-mode-hook
          emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          lisp-mode-hook
          maxima-mode-hook
          ielm-mode-hook
          sh-mode-hook
          makefile-gmake-mode-hook
          php-mode-hook
          python-mode-hook
          js-mode-hook
          go-mode-hook
          qml-mode-hook
          jade-mode-hook
          css-mode-hook
          ruby-mode-hook
          coffee-mode-hook
          rust-mode-hook
          qmake-mode-hook
          lua-mode-hook
          swift-mode-hook
          minibuffer-inactive-mode-hook) . awesome-pair-mode))

(provide 'init-edit)
;;; init-edit.el ends here

;;; init-edit.el --- edit setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

;; Writable `grep' buffer
(use-package wgrep
  :straight t
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Hungry deletion
(use-package hungry-delete
  :straight t
  :delight
  :hook (after-init-hook . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Jump to things in Emacs tree-style
(use-package avy
  :straight t
  :hook (after-init-hook . avy-setup-default)
  :config
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        avy-style 'at-full
        avy-keys (number-sequence ?a ?z))
  (setq avy-orders-alist
        '((avy-goto-char . avy-order-closest)
          (avy-goto-word-0 . avy-order-closest))))

;; Treat undo history as a tree
(use-package undo-tree
  :straight t
  ;; :functions undo-tree-visualizer-selection-mode
  :hook (after-init-hook . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history t
        undo-tree-incompatible-major-modes '(term-mode
                                             vterm-mode))
  :config

  ;; fix diff window position
  (defun my-undo-tree-visualizer-show-diff (&optional node)
    ;; show visualizer diff display
    (setq undo-tree-visualizer-diff t)
    (let ((buff (with-current-buffer undo-tree-visualizer-parent-buffer
                  (undo-tree-diff node)))
          (display-buffer-mark-dedicated 'soft)
          win)
      (setq win (split-window nil nil 'right nil))
      (set-window-buffer win buff)
      (shrink-window-if-larger-than-buffer win)))
  (advice-add #'undo-tree-visualizer-show-diff :override #'my-undo-tree-visualizer-show-diff)

  (add-hook 'undo-tree-visualizer-mode-hook (lambda ()
                                              (undo-tree-visualizer-selection-mode)
                                              (display-line-numbers-mode 0)))
  ;; HACK: keep the diff window
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t)))

(use-package comment-dwim-2
  :straight t
  :bind ([remap comment-dwim] . comment-dwim-2))


(setq isearch-lazy-count t)

;; Flexible text folding
(use-package origami
  :straight t
  :hook (after-init-hook . global-origami-mode)
  :init (setq origami-show-fold-header t)
  :config (face-spec-reset-face 'origami-fold-header-face))

(use-package command-log-mode
  :straight t
  :hook (after-init-hook . global-command-log-mode)
  :init
  (setq command-log-mode-open-log-turns-on-mode t
        command-log-mode-is-global t
        command-log-mode-window-size 40))

(use-package awesome-pair
  :straight (:type git :host github :repo "manateelazycat/awesome-pair")
  :demand
  :config
  (dolist (hook (list
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'java-mode-hook
                 'haskell-mode-hook
                 'emacs-lisp-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'maxima-mode-hook
                 'ielm-mode-hook
                 'sh-mode-hook
                 'makefile-gmake-mode-hook
                 'php-mode-hook
                 'python-mode-hook
                 'js-mode-hook
                 'go-mode-hook
                 'qml-mode-hook
                 'jade-mode-hook
                 'css-mode-hook
                 'ruby-mode-hook
                 'coffee-mode-hook
                 'rust-mode-hook
                 'qmake-mode-hook
                 'lua-mode-hook
                 'swift-mode-hook
                 'minibuffer-inactive-mode-hook
                 ))
    (add-hook hook '(lambda () (awesome-pair-mode 1))))

  (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
  (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
  (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
  (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
  (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
  (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
  (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

  (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
  (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

  (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)

  (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
  (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
  (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

  (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
  (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
  (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
  (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
  (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

  (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
  (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
  (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)
  )

(provide 'init-edit)
;;; init-edit.el ends here

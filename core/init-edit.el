;;; init-edit.el --- edit setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

;; Hungry deletion
(use-package hungry-delete
  :straight t
  :delight t
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
        avy-keys '(?a ?s ?d ?f ?h ?j ?k ?l ?q ?u ?w ?i ?e ?o))
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

(use-package color-rg
  :straight (:type git :host github :repo "manateelazycat/color-rg")
  :commands (color-rg-search-project-with-type
             color-rg-search-input
             color-rg-search-input-in-project
             color-rg-search-input-in-current-file)
  :bind (:map color-rg-mode-map
              ("h" . color-rg-jump-prev-file)
              ("l" . color-rg-jump-next-file)
              )
  :init
  (setq color-rg-mac-load-path-from-shell nil))

(use-package awesome-pair
  :straight (:type git :host github :repo "manateelazycat/awesome-pair")
  :demand
  :bind (:map awesome-pair-mode-map
              ("(" . awesome-pair-open-round)
              ("[" . awesome-pair-open-bracket)
              ("{" . awesome-pair-open-curly)
              (")" . awesome-pair-close-round)
              ("]" . awesome-pair-close-bracket)
              ("}" . awesome-pair-close-curly)
              ("=" . awesome-pair-equal)
              ("%" . awesome-pair-match-paren)
              ("\"" . awesome-pair-double-quote)
              ("SPC" . awesome-pair-space)
              ("M-o" . awesome-pair-backward-delete)
              ("C-d" . awesome-pair-forward-delete)
              ("C-k" . awesome-pair-kill)
              ("M-\"" . awesome-pair-wrap-double-quote)
              ("M-[" . awesome-pair-wrap-bracket)
              ("M-{" . awesome-pair-wrap-curly)
              ("M-(" . awesome-pair-wrap-round)
              ("M-)" . awesome-pair-unwrap)
              ("M-p" . awesome-pair-jump-right)
              ("M-n" . awesome-pair-jump-left)
              ("M-:" . awesome-pair-jump-out-pair-and-newline))

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
    (add-hook hook '(lambda () (awesome-pair-mode 1)))))

(provide 'init-edit)
;;; init-edit.el ends here

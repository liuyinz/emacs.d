(require 'init-const)

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Jump to things in Emacs tree-style
(use-package avy
  :hook (after-init . avy-setup-default)
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
  ;; :functions undo-tree-visualizer-selection-mode
  :hook (after-init . global-undo-tree-mode)
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
  :bind ([remap comment-dwim] . comment-dwim-2))


(setq isearch-lazy-count t)

;; Flexible text folding
(use-package origami
  :hook (after-init . global-origami-mode)
  :init (setq origami-show-fold-header t)
  :config (face-spec-reset-face 'origami-fold-header-face))


(provide 'init-edit)

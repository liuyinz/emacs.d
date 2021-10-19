;;; init-highlight.el --- highlight setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Colorize color names in buffers
(use-package rainbow-mode
  :hook ((html-mode-hook
          php-mode-hook
          css-mode-hook
          emacs-lisp-mode-hook
          lisp-interaction-mode) . rainbow-mode)
  :config
  (with-no-warnings
    ;; HACK Use overlay instead of text properties to override `hl-line' faces.
    ;; SEE https://emacs.stackexchange.com/questions/36420
    (defun ad/rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5
                                                     (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'ad/rainbow-colorize-match)

    (defun ad/rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'ad/rainbow-clear-overlays)))

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :init
  (setq rainbow-delimiters-max-face-count 4))

(use-package highlight-parentheses
  :hook (prog-mode-hook . highlight-parentheses-mode)
  :init
  (setq highlight-parentheses-colors nil
        highlight-parentheses-delay 0.05
        highlight-parentheses-highlight-adjacent t
        ;; BUG highlight overlay cover margin face such as diff-hl
        highlight-parentheses-attributes '((:inverse-video t :weight bold))))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :hook (after-init-hook . global-diff-hl-mode)
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-ask-before-revert-hunk nil
        diff-hl-side 'left)
  :config

  (use-package diff-hl-margin
    :hook (diff-hl-mode-hook . diff-hl-margin-mode)
    :init
    ;; (char-to-string ?\x258d) => "▍" , SEE https://www.htmlsymbols.xyz/box-drawing
    (setq diff-hl-margin-symbols-alist
          '((insert  . "\x258d")
            (delete  . "\x258d")
            (change  . "\x258d")
            (unknown . "\x258d")
            (ignored . "\x258d"))))

  (use-package diff-hl-dired
    :hook (dired-mode-hook . diff-hl-dired-mode))

  ;; ;; BUG conflict with company-mode,evil-terminal-crusor-change
  ;; (use-package diff-hl-flydiff
  ;;   :hook (diff-hl-mode-hook . diff-hl-flydiff-mode))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(provide 'init-highlight)
;;; init-highlight.el ends here

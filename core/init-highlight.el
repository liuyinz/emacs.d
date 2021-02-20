;;; init-highlight.el --- highlight setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Highlight brackets according to their depth
(leaf rainbow-delimiters
  :blackout t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf highlight-parentheses
  :blackout t
  :hook (prog-mode-hook . highlight-parentheses-mode)
  :init
  (setq highlight-parentheses-colors nil
        highlight-parentheses-background-colors '("#5d656b")
        highlight-parentheses-highlight-adjacent t))

;;Highlight uncommitted changes using VC
(leaf diff-hl
  :blackout t
  :hook
  (after-init-hook . global-diff-hl-mode)
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-ask-before-revert-hunk nil
        diff-hl-side 'left)
  ;; Set fringe style
  (setq-default fringes-outside-margins t)
  :config

  (leaf diff-hl-dired
    :hook (dired-mode-hook . diff-hl-dired-mode))

  (leaf diff-hl-flydiff
    :hook (diff-hl-mode-hook . diff-hl-flydiff-mode))

  ;; UI
  (set-face-attribute 'diff-hl-change nil
                      :foreground (face-background 'highlight)
                      :background nil)
  (set-face-attribute 'diff-hl-insert nil :background nil)
  (set-face-attribute 'diff-hl-delete nil :background nil)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector #b11100000) 1 8 '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;; Colorize color names in buffers
(leaf rainbow-mode
  :blackout t
  :hook ((html-mode-hook php-mode-hook css-mode-hook) . rainbow-mode)
  :config
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays)))

;;indent-guide
(leaf indent-guide
  :blackout t
  :hook (prog-mode-hook . indent-guide-mode)
  :config
  (setq indent-guide-char "¦")
  (set-face-background 'indent-guide-face (face-background 'default))
  (set-face-foreground 'indent-guide-face (face-foreground 'font-lock-comment-face)))

(provide 'init-highlight)
;;; init-highlight.el ends here

;;; init-highlight.el --- highlight setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Colorize color names in buffers
(leaf rainbow-mode
  :hook ((html-mode-hook css-mode-hook) . rainbow-mode)
  :defer-config
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

(leaf rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :init
  (setq rainbow-delimiters-max-face-count 4))

(leaf highlight-parentheses
  :hook
  ((prog-mode-hook helpful-mode-hook) . highlight-parentheses-mode)
  (minibuffer-setup-hook . highlight-parentheses-minibuffer-setup)
  :init
  (setq highlight-parentheses-delay 0.05
        highlight-parentheses-highlight-adjacent t
        highlight-parentheses-colors nil
        highlight-parentheses-attributes '((:inverse-video t :weight bold))))

;; Syntax highlighting of known Elisp symbols
(leaf highlight-defined
  :init (setq highlight-defined-face-use-itself t))

;; BUG conflict with meow in cursor shape
(leaf diff-hl
  :hook (after-init-hook . global-diff-hl-mode)
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-ask-before-revert-hunk nil
        diff-hl-show-staged-changes nil
        diff-hl-side 'left)

  :defer-config

  (leaf diff-hl-margin
    :hook (global-diff-hl-mode-hook . diff-hl-margin-mode)
    :init
    ;; SEE https://symbl.cc/en/unicode/blocks/block-elements/
    ;; (char-to-string ?\x258d) => "▍" , ?\x2588 - 258f
    ;; if font is designed for chinese,then use ?\x258f, or use ?\x258d instead.
    (setq diff-hl-margin-symbols-alist
          '((insert  . "\x258f")
            (delete  . "\x258f")
            (change  . "\x258f")
            (unknown . "\x258f")
            (ignored . "\x258f"))))

  (leaf diff-hl-dired
    :hook (dired-mode-hook . diff-hl-dired-mode-unless-remote))

  (leaf diff-hl-flydiff
    :hook (diff-hl-mode-hook . diff-hl-flydiff-mode))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;; SEE https://github.com/jdtsmith/indent-bars.git
;; (leaf indent-bars
;;   :hook (prog-mode-hook . indent-bars-mode)
;;   :init
;;   (setq
;;    indent-bars-pattern "."
;;    indent-bars-width-frac 0.5
;;    indent-bars-pad-frac 0.25
;;    indent-bars-color-by-depth nil
;;    indent-bars-highlight-current-depth '(:face default :blend 0.4)))

(provide 'init-highlight)
;;; init-highlight.el ends here

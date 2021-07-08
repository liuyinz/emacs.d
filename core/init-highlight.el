;;; init-highlight.el --- highlight setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Highlight brackets according to their depth
(leaf rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :init
  (setq rainbow-delimiters-max-face-count 5))

(leaf highlight-parentheses
  :hook (prog-mode-hook . highlight-parentheses-mode)
  :init
  (setq highlight-parentheses-colors nil
        highlight-parentheses-delay 0.05
        highlight-parentheses-highlight-adjacent t
        highlight-parentheses-attributes '((:inverse-video t :weight bold))))

;; Highlight TODO and similar keywords in comments and strings
;; TODO consutl-todo implements
(leaf hl-todo
  :hook (after-init-hook . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

;;Highlight uncommitted changes using VC
(leaf diff-hl
  :hook
  (after-init-hook . global-diff-hl-mode)
  (writeroom-mode-hook . (lambda () (diff-hl-mode -1)))
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-ask-before-revert-hunk nil
        diff-hl-side 'left)
  :config
  ;; (with-no-warnings
  ;;   (defun my-diff-hl-fringe-bmp-function (_type _pos)
  ;;     "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
  ;;     (define-fringe-bitmap 'my-diff-hl-bmp
  ;;       (vector #b11100000) 1 8 '(center t)))
  ;;   (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

  (leaf diff-hl-margin
    :hook (diff-hl-mode-hook . diff-hl-margin-mode)
    :init
    ;; NOTE see@https://www.htmlsymbols.xyz/box-drawing
    ;; HACK (char-to-string ?\x258d) => "▍"
    (setq diff-hl-margin-symbols-alist
          '((insert  . "\x258d")
            (delete  . "\x258d")
            (change  . "\x258d")
            (unknown . "\x258d")
            (ignored . "\x258d"))))

  (leaf diff-hl-dired
    :hook (dired-mode-hook . diff-hl-dired-mode))

  ;; ;; BUG  too slow for large files
  ;; (leaf diff-hl-flydiff
  ;;   :hook (diff-hl-mode-hook . diff-hl-flydiff-mode))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;; Colorize color names in buffers
(leaf rainbow-mode
  :hook ((html-mode-hook php-mode-hook css-mode-hook) . rainbow-mode)
  :config
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5
                                                     (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays)))

(provide 'init-highlight)
;;; init-highlight.el ends here

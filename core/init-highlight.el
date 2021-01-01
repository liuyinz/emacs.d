(use-package hl-line
  :hook (((evil-normal-state-entry
           evil-emacs-state-exit) . hl-line-mode)
         ((evil-insert-state-entry
           evil-emacs-state-entry) . (lambda () (hl-line-mode -1)))))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :blackout
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :hook (after-init . global-highlight-parentheses-mode)
  :init
  (setq highlight-parentheses-colors nil
        highlight-parentheses-background-colors '("#5d7281")
        highlight-parentheses-highlight-adjacent t))

;;Highlight uncommitted changes using VC
(use-package diff-hl
  :blackout
  ;; :if (memq window-system '(mac ns))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-ask-before-revert-hunk nil
        diff-hl-side 'left)
  ;; Set fringe style
  (setq-default fringes-outside-margins t)
  :config
  ;; conflicts with company-active-map keybindings
  ;; (diff-hl-flydiff-mode 1)

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

  ;; (unless (display-graphic-p)
  ;;   (diff-hl-margin-mode))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;; Colorize color names in buffers
(use-package rainbow-mode
  :blackout
  :hook ((html-mode php-mode css-mode) . rainbow-mode)
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
(use-package indent-guide
  :blackout
  :hook (prog-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "¦")
  (set-face-background 'indent-guide-face (face-background 'default))
  (set-face-foreground 'indent-guide-face (face-foreground 'font-lock-comment-face)))

(provide 'init-highlight)

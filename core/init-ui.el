;;; init-ui.el --- ui settings -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(leaf which-key
  :hook (after-init-hook . which-key-mode)
  :init
  (setq which-key-show-prefix 'top
        which-key-popup-type 'minibuffer
        which-key-preserve-window-configuration t
        which-key-max-description-length 45
        which-key-dont-use-unicode t
        which-key-idle-delay 0.6
        which-key-idle-secondary-delay 0.2))

(leaf repeat
  :hook (after-init-hook . repeat-mode)
  :init
  (setq repeat-exit-key (kbd "C-g")))

(leaf repeat-help
  :hook (repeat-mode-hook . repeat-help-mode)
  :init
  (setq repeat-help-popup-type 'which-key))

;; FIXME shining when move up/down in rg-mode result when display is enabled
(leaf mini-echo
  :hook (after-init-hook . mini-echo-mode)
  :init
  (setq mode-line-position-column-line-format '("%l:%c,%p"))
  (setq mini-echo-right-padding 2)
  (setq mini-echo-mise-show-always nil)
  (setq mini-echo-persistent-rule
        '(:long ("meow" "shrink-path" "vcs" "buffer-position"
                 "buffer-size" "flymake" "mise" "envrc")
          :short ("meow" "buffer-name" "buffer-position" "flymake")))

  (setq mini-echo-persistent-function #'my/mini-echo-persistent-detect)
  (defun my/mini-echo-persistent-detect ()
    (with-current-buffer (current-buffer)
      (pcase major-mode
        ((guard (bound-and-true-p atomic-chrome-edit-mode))
         '(:both ("meow" "atomic-chrome" "buffer-name" "buffer-position" "flymake")))
        ((guard (or (memq major-mode '(git-commit-elisp-text-mode git-rebase-mode))
                    (string-match-p "\\`magit-.*-mode\\'" (symbol-name major-mode))))
         '(:both ("meow" "major-mode" "project")))
        ((guard (and (fboundp 'popper-display-control-p)
                     (popper-display-control-p (current-buffer))))
         '(:both ("meow" "popper")))
        ('rg-mode '(:both ("meow" "major-mode")))
        ('diff-mode '(:both ("meow" "major-mode")))
        ('ibuffer-mode '(:both ("meow" "major-mode")))
        ('dired-mode '(:both ("meow" "major-mode" "dired")))
        ('helpful-mode '(:both ("meow" "major-mode" "helpful")))
        ('xwidget-webkit-mode '(:long ("meow" "shrink-path")
                                :short ("meow" "buffer-name")))
        (_ nil))))
  )

;; (leaf breadcrumb
;;   :hook (after-init-hook . breadcrumb-mode))

;; TODO matrix screensaver
(leaf insecure-lock
  :bind
  ("s-q" . insecure-lock-enter)
  :init
  (setq insecure-lock-require-password nil)
  (setq insecure-lock-mode-hook '(insecure-lock-redact-with-minibuf insecure-lock-posframe))
  (defun insecure-lock-redact-with-minibuf ()
    "`insecure-lock' module that redacts buffers.
No changes in mode-line."
    (unless (require 'redacted nil t) (user-error "Package `redacted' not available"))
    (let ((arg (if insecure-lock-mode 1 -1)))
      (dolist (frame (frame-list))
        ;; NOTE call redacted-mode also in minibuf
        (dolist (window (window-list frame t))
          (with-current-buffer (window-buffer window)
            (redacted-mode arg)))))))

(leaf page-break-lines
  :hook (after-init-hook . global-page-break-lines-mode)
  :init
  (setq page-break-lines-max-width fill-column))

(leaf doom-themes
  :require t
  :config
  (load-theme 'doom-tokyo-night t)
  :init
  (advice-add 'load-theme :after #'av/doom-theme-customize)
  (defun av/doom-theme-customize (&rest _)
    (pcase (car custom-enabled-themes)
      ('doom-tokyo-night
       (custom-set-faces
        `(secondary-selection ((t (:background ,(doom-darken (doom-color 'violet) 0.7)))))
        `(hl-line ((t (:background ,(doom-darken (doom-color 'blue) 0.7)))))
        ;;consult
        '(consult-file         ((t (:inherit font-lock-doc-face))))
        '(consult-imenu-prefix ((t (:inherit font-lock-doc-face :slant italic))))
        ;; vertico
        `(vertico-current ((t (:background ,(doom-darken (doom-color 'blue) 0.7) :extend t))))
        ;;diff-hl
        `(diff-hl-change ((t (:background unspecified :inverse-video nil))))
        `(diff-hl-insert ((t (:background unspecified :inverse-video nil))))
        `(diff-hl-delete ((t (:background unspecified :inverse-video nil))))
        ;; transient
        `(transient-key-exit ((t (:foreground ,(doom-color 'yellow)))))
        `(transient-key-stay ((t (:foreground ,(doom-color 'green)))))
        `(transient-key-return ((t (:foreground ,(doom-color 'violet)))))
        ;; meow
        `(meow-normal-indicator   ((t (:foreground ,(doom-color 'blue) :bold t))))
        `(meow-insert-indicator   ((t (:foreground ,(doom-color 'green) :bold t))))
        `(meow-motion-indicator   ((t (:foreground ,(doom-color 'red) :bold t))))
        `(meow-keypad-indicator   ((t (:foreground ,(doom-color 'magenta) :bold t))))
        `(meow-beacon-indicator   ((t (:foreground ,(doom-color 'yellow) :bold t))))
        `(meow-search-indicator   ((t (:foreground ,(doom-color 'cyan) :bold t))))
        `(meow-beacon-fake-cursor ((t (:foreground ,(doom-color 'bg)
                                       :background ,(doom-blend 'yellow 'bg 0.7)
                                       :bold t))))
        `(meow-position-highlight-number ((t (:foreground ,(doom-color 'bg)
                                              :background ,(doom-blend 'cyan 'bg 0.7)
                                              :bold t))))
        `(meow-position-highlight-number-2 ((t (:foreground ,(doom-color 'bg)
                                                :background ,(doom-blend 'magenta 'bg 0.7)
                                                :bold t))))
        `(meow-position-highlight-number-3 ((t (:foreground ,(doom-color 'bg)
                                                :background ,(doom-blend 'yellow 'bg 0.7)
                                                :bold t))))
        ;; dirvish
        '(dirvish-hl-line ((t (:inherit hl-line))))
        ;; mini-echo
        `(window-divider ((t (:foreground ,(doom-color 'base0)))))

        ;; ;;org
        ;; `(org-block            ((t (:background ,(doom-darken (doom-color 'base3) 0.15)))))
        ;; `(org-block-begin-line ((t (:background ,(doom-darken (doom-color 'base3) 0.15)
        ;;                             :foreground ,(doom-color 'fg-alt)))))
        ;; `(org-block-end-line   ((t (:background ,(doom-darken 'base3 0.15)
        ;;                             :foreground ,(doom-color 'fg-alt)))))
        ;; ;; markdown-mode
        ;; `(markdown-code-face   ((t (:background ,(doom-darken (doom-color 'base3) 0.15)))))
        ;; `(markdown-inline-code-face ((t (:background ,(doom-color 'base3)))))

        ;; combobulate
        '(combobulate-refactor-highlight-face ((t (:background unspecified))))
        `(combobulate-refactor-label-face ((t (:foreground ,(doom-color 'bg)
                                               :background ,(doom-blend 'cyan 'bg 0.7)
                                               :bold t))))
        `(combobulate-active-indicator-face ((t (:foreground ,(doom-blend 'cyan 'bg 0.7)
                                                 :bold t))))
        ;; web-mode
        `(web-mode-current-element-highlight-face
          ((t (:weight bold :background ,(doom-blend 'cyan 'bg 0.3)))))
        `(web-mode-current-column-highlight-face  ((t (:background ,(doom-color 'base4)
                                                       :foreground unspecified))))

        ;; orderless
        `(orderless-match-face-0 ((t (:underline t))))
        `(orderless-match-face-1 ((t (:underline t))))
        `(orderless-match-face-2 ((t (:underline t))))
        `(orderless-match-face-3 ((t (:underline t))))
        ;;rg.elv
        `(rg-match-face ((t (:inherit orderless-match-face-2 :bold t :underline t))))
        `(rg-info-face        ((t (:foreground ,(doom-color 'red)))))
        `(rg-filename-face    ((t (:foreground ,(doom-color 'blue) :underline t))))
        `(rg-line-number-face ((t (:foreground ,(doom-color 'yellow)))))
        `(rg-file-tag-face ((t (:foreground ,(doom-color 'magenta)))))
        ;; wgrep
        ;; `(wgrep-face)
        ))))
  )

(provide 'init-ui)
;;; init-ui.el ends here

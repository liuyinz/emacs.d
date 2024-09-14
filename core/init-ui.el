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
  ;; (load-theme 'doom-tomorrow-night t)
  (load-theme 'doom-city-lights t)
  (my/doom-theme-city-lights-customize)
  :init
  (defun my/doom-theme-city-lights-customize ()
    (custom-set-faces
     `(font-lock-comment-face ((t (:foreground
                                   ,(doom-lighten (doom-color 'comments) 0.15)))))
     `(font-lock-function-name-face ((t (:foreground ,(doom-color 'green)))))
     `(secondary-selection ((t (:background ,(doom-darken (doom-color 'blue) 0.7)))))
     ;; `(region ((t (:background ,(doom-darken (doom-color 'dark-blue) 0.6)))))
     ;; `(hl-line ((t (:background ,(doom-darken (doom-color 'dark-blue) 0.6)))))
     ;; '(match          ((t (:inherit font-lock-variable-name-face
     ;;                       :weight bold
     ;;                       :inverse-video t))))
     ;; '(lazy-highlight ((t (:inherit match
     ;;                       :foreground unspecified
     ;;                       :background unspecified))))
     ;;
     ;; '(cursor  ((t (:background "#a0b3c5" :foreground "#1d252c"))))
     ;; '(isearch ((t (:inherit cursor))))
     ;; '(next-error              ((t (:inherit match))))
     ;; '(next-error-message      ((t (:inherit unspecified :background "#384551"))))
     ;; '(completions-common-part ((t (:inherit font-lock-keyword-face))))

     ;; '(fill-column-indicator   ((t (:inherit unspecified
     ;;                                :foreground unspecified
     ;;                                :background "#181f25"))))

     `(vc-state-base ((t (:bold t :foreground ,(doom-color 'green)))))
     ;; transient
     `(transient-key-exit ((t (:foreground ,(doom-color 'yellow)))))
     `(transient-key-stay ((t (:foreground ,(doom-color 'green)))))
     `(transient-key-return ((t (:foreground ,(doom-color 'violet)))))
     ;; meow
     `(meow-normal-indicator   ((t (:foreground ,(doom-color 'blue) :bold t))))
     `(meow-insert-indicator   ((t (:foreground ,(doom-color 'green) :bold t))))
     `(meow-beacon-indicator   ((t (:foreground ,(doom-color 'yellow) :bold t))))
     `(meow-keypad-indicator   ((t (:foreground ,(doom-color 'violet) :bold t))))
     `(meow-motion-indicator   ((t (:foreground ,(doom-color 'magenta) :bold t))))
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
     ;; rg.el
     `(rg-info-face        ((t (:foreground ,(doom-color 'blue)))))
     `(rg-filename-face    ((t (:foreground ,(doom-color 'blue) :underline t))))
     `(rg-line-number-face ((t (:foreground ,(doom-color 'yellow)))))
     ;; vertico
     `(vertico-current ((t (:foreground "white"
                            :background ,(doom-color 'base4)
                            :extend t))))
     ;; corfu
     `(corfu-default ((t (:background ,(doom-color 'bg-alt)))))
     '(corfu-current ((t (:inherit vertico-current
                          :background unspecified
                          :foreground unspecified))))
     ;;consult
     '(consult-file         ((t (:inherit font-lock-doc-face))))
     '(consult-imenu-prefix ((t (:inherit font-lock-doc-face :slant italic))))
     ;;marginalia
     '(marginalia-type     ((t (:inherit font-lock-constant-face))))
     '(marginalia-key      ((t (:inherit font-lock-keyword-face :weight bold))))
     '(marginalia-date     ((t (:inherit font-lock-keyword-face))))
     '(marginalia-modified ((t (:inherit font-lock-variable-name-face))))
     ;;diff-hl
     `(diff-hl-change ((t (:foreground ,(doom-color 'yellow)
                           :background unspecified
                           :inverse-video nil))))
     '(diff-hl-insert ((t (:background unspecified :inverse-video nil))))
     '(diff-hl-delete ((t (:background unspecified :inverse-video nil))))
     ;; yasnippet
     `(yas-field-highlight-face ((t (:inherit unspecified
                                     :weight bold
                                     :foreground unspecified
                                     :background ,(doom-color 'base4)))))
     ;;org
     `(org-block            ((t (:background ,(doom-darken (doom-color 'base3) 0.15)))))
     `(org-block-begin-line ((t (:background ,(doom-darken (doom-color 'base3) 0.15)
                                 :foreground ,(doom-color 'fg-alt)))))
     `(org-block-end-line   ((t (:background ,(doom-darken 'base3 0.15)
                                 :foreground ,(doom-color 'fg-alt)))))
     ;; markdown-mode
     `(markdown-code-face   ((t (:background ,(doom-darken (doom-color 'base3) 0.15)))))
     `(markdown-inline-code-face ((t (:background ,(doom-color 'base3)))))
     ;; highlight-defined
     '(highlight-defined-function-name-face ((t (:inherit unspecified))))
     '(highlight-defined-variable-name-face ((t (:inherit unspecified))))

     ;; magit
     '(magit-hash        ((t (:inherit font-lock-constant-face
                              :foreground unspecified))))
     '(magit-log-date    ((t (:inherit font-lock-function-name-face
                              :foreground unspecified))))
     '(magit-log-author  ((t (:inherit font-lock-keyword-face
                              :foreground unspecified))))
     '(magit-header-line ((t (:inherit vertico-current
                              :foreground unspecified
                              :background unspecified
                              :box nil))))
     `(dashboard-text-banner-face ((t (:inherit font-lock-constant-face))))
     `(dashboard-heading-face ((t (:inherit font-lock-function-name-face))))
     `(dashboard-items-face ((t (:inherit unspecified))))
     `(dashboard-footer-face ((t (:inherit font-lock-type-face))))
     `(elfeed-search-tag-face ((t (:foreground ,(doom-color 'green)))))
     `(elfeed-search-title-face ((t (:foreground ,(doom-color 'fg-alt)))))
     ;; jupyter
     '(jupyter-repl-input-prompt  ((t (:inherit font-lock-property-face))))
     '(jupyter-repl-output-prompt ((t (:inherit font-lock-function-name-face))))
     '(jupyter-repl-traceback     ((t (:inherit font-lock-warning-face
                                       :bold t))))
     '(binky-preview-column-mark-auto ((t (:inherit font-lock-string-face))))
     '(binky-preview-column-mark-manual ((t (:inherit font-lock-constant-face))))
     ;; ;; mini-echo
     '(window-divider ((t (:foreground "#5d6a76"))))
     ;; dirvish-subtree
     '(dirvish-subtree-state ((t (:inherit dired-special))))
     '(dirvish-hl-line ((t (:inherit hl-line))))
     ;; combobulate
     '(combobulate-refactor-highlight-face ((t (:background unspecified))))
     `(combobulate-refactor-label-face ((t (:foreground ,(doom-color 'bg)
                                            :background ,(doom-blend 'cyan 'bg 0.7)
                                            :bold t))))
     `(combobulate-active-indicator-face ((t (:foreground ,(doom-blend 'cyan 'bg 0.7)
                                              :bold t))))
     ;; web-mode
     `(web-mode-current-element-highlight-face ((t (:weight bold
                                                    :background ,(doom-blend 'methods 'bg 0.3)))))
     `(web-mode-current-column-highlight-face  ((t (:background ,(doom-color 'base4)
                                                    :foreground unspecified))))
     ;; cperl-mode
     '(cperl-array-face ((t (:inherit font-lock-variable-name-face))))
     '(cperl-hash-face  ((t (:inherit font-lock-type-face))))
     ;; perl-mode
     '(perl-non-scalar-variable ((t (:inherit font-lock-type-face))))
     )))

(provide 'init-ui)
;;; init-ui.el ends here

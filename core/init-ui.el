;;; init-ui.el --- ui settings -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(leaf redacted)

(leaf topsy
  :hook (emacs-lisp-mode-hook . topsy-mode))

(leaf insecure-lock
  :commands insecure-lock-enter
  :init
  (setq insecure-lock-require-password t)
  (setq insecure-lock-mode-hook '(insecure-lock-redact)))

(leaf hide-mode-line)

(leaf page-break-lines
  :hook (after-init-hook . global-page-break-lines-mode)
  :init
  (setq page-break-lines-max-width fill-column))

(leaf doom-modeline
  :hook (after-init-hook . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil
        doom-modeline-bar-width 0
        doom-modeline-height 15
        doom-modeline-persp-name nil
        doom-modeline-irc nil
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-checker-simple-format nil
        doom-modeline-indent-info nil
        doom-modeline-env-load-string "..."
        doom-modeline-vcs-max-length 20
        doom-modeline-window-width-limit (+ fill-column 20)
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-project-detection 'project))

(leaf doom-themes
  :require t
  :config
  (load-theme 'doom-city-lights t)
  (my/doom-theme-city-lights-customize)
  :init
  (defun my/doom-theme-city-lights-customize ()
    (custom-set-faces
     '(default ((t (:foreground "#a0b3c5" :background "#1d252c"))))
     '(link    ((t (:weight normal))))
     '(vertical-border ((t (:background unspecified))))

     `(font-lock-comment-face ((t (:foreground ,(doom-darken "#718ca1" 0.1)
                                   :italic t))))
     '(font-lock-string-face               ((t (:foreground "#008b94"))))
     '(font-lock-builtin-face              ((t (:foreground "#718ca1"))))
     '(font-lock-warning-face              ((t (:foreground "#d95468"))))
     '(font-lock-variable-name-face        ((t (:foreground "#8bd49c"))))
     '(font-lock-function-name-face        ((t (:foreground "#c06ece"))))
     '(font-lock-regexp-grouping-backslash ((t (:foreground "#c06ece"))))
     '(font-lock-regexp-grouping-construct ((t (:foreground "#c06ece"))))

     '(match          ((t (:inherit font-lock-variable-name-face
                           :weight bold
                           :inverse-video t))))
     '(lazy-highlight ((t (:inherit match
                           :foreground unspecified
                           :background unspecified))))

     '(cursor  ((t (:background "#a0b3c5" :foreground "#1d252c"))))
     '(isearch ((t (:inherit cursor))))
     '(next-error              ((t (:inherit match))))
     '(next-error-message      ((t (:inherit unspecified :background "#384551"))))
     '(secondary-selection     ((t (:background "#384551"))))
     '(completions-common-part ((t (:inherit font-lock-keyword-face))))

     '(fill-column-indicator   ((t (:inherit unspecified
                                    :foreground unspecified
                                    :background "#181f25"))))

     ;; meow
     '(meow-normal-indicator   ((t (:inherit font-lock-keyword-face :bold t))))
     '(meow-insert-indicator   ((t (:inherit font-lock-warning-face :bold t))))
     '(meow-beacon-indicator   ((t (:inherit font-lock-type-face :bold t))))
     '(meow-keypad-indicator   ((t (:inherit font-lock-function-name-face :bold t))))
     '(meow-motion-indicator   ((t (:inherit font-lock-string-face :bold t))))
     '(meow-search-indicator   ((t (:inherit font-lock-variable-name-face :bold t))))
     '(meow-beacon-fake-cursor ((t :inherit meow-beacon-indicator :inverse-video t)))

     ;; orderless
     '(orderless-match-face-0 ((t (:inherit font-lock-keyword-face
                                   :background unspecified
                                   :foreground unspecified
                                   :inverse-video t))))
     '(orderless-match-face-1 ((t (:inherit font-lock-warning-face
                                   :background unspecified
                                   :foreground unspecified
                                   :inverse-video t))))
     '(orderless-match-face-2 ((t (:inherit font-lock-type-face
                                   :background unspecified
                                   :foreground unspecified
                                   :inverse-video t))))
     '(orderless-match-face-3 ((t (:inherit font-lock-string-face
                                   :background unspecified
                                   :foreground unspecified
                                   :inverse-video t))))
     ;; rainbow-delimiters
     '(rainbow-delimiters-depth-1-face   ((t (:foreground "#b62d65"))))
     '(rainbow-delimiters-depth-2-face   ((t (:foreground "#8bd49c"))))
     '(rainbow-delimiters-depth-3-face   ((t (:foreground "#539afc"))))
     '(rainbow-delimiters-depth-4-face   ((t (:foreground "#ebbf83"))))
     '(rainbow-delimiters-unmatched-face ((t (:background "white"))))

     ;; vertico
     '(vertico-current ((t (:foreground "white" :background "#384551" :extend t))))

     ;; corfu
     '(corfu-default ((t (:background "#181f25"))))
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
     '(diff-hl-change ((t (:foreground "#ebbf83"
                           :background unspecified
                           :inverse-video nil))))
     '(diff-hl-insert ((t (:background unspecified :inverse-video nil))))
     '(diff-hl-delete ((t (:background unspecified :inverse-video nil))))

     ;; yasnippet
     '(yas-field-highlight-face ((t (:inherit font-lock-variable-name-face
                                     :weight bold
                                     :background "#384551"))))
     ;;org
     `(org-block            ((t (:background ,(doom-darken "#28323B" 0.15)))))
     `(org-block-begin-line ((t (:background ,(doom-darken "#28323B" 0.15)
                                 :foreground "#707b86"))))
     `(org-block-end-line   ((t (:background ,(doom-darken "#28323B" 0.15)
                                 :foreground "#707b86"))))
     ;; markdown-mode
     `(markdown-code-face        ((t (:background ,(doom-darken "#28323B" 0.15)))))
     '(markdown-inline-code-face ((t (:background "#28323B"))))

     ;; avy
     '(avy-lead-face   ((t (:foreground "#539afc" :background unspecified))))
     '(avy-lead-face-0 ((t (:foreground "#99d0f6" :background unspecified))))
     '(avy-lead-face-1 ((t (:foreground "#cfe9fb" :background unspecified))))
     '(avy-lead-face-2 ((t (:foreground "#f3f9fe" :background unspecified))))
     ;; highlight-defined
     '(highlight-defined-function-name-face ((t (:inherit unspecified))))
     '(highlight-defined-variable-name-face ((t (:inherit unspecified))))

     ;; rg.el
     '(rg-info-face        ((t (:foreground "#5ec4ff"))))
     '(rg-filename-face    ((t (:foreground "#5ec4ff" :underline t))))
     '(rg-line-number-face ((t (:foreground "#ebbf83"))))

     ;; magit
     '(magit-hash        ((t (:inherit font-lock-function-name-face
                              :foreground unspecified))))
     '(magit-log-date    ((t (:inherit font-lock-variable-name-face
                              :foreground unspecified))))
     '(magit-log-author  ((t (:inherit font-lock-keyword-face
                              :foreground unspecified))))
     '(magit-header-line ((t (:inherit vertico-current
                              :foreground unspecified
                              :background unspecified
                              :box nil))))

     ;; web-mode
     '(web-mode-current-element-highlight-face ((t (:weight bold :inverse-video t))))
     '(web-mode-current-column-highlight-face  ((t (:background "#384551"
                                                    :foreground unspecified))))

     ;; cperl-mode
     '(cperl-array-face ((t (:inherit font-lock-variable-name-face))))
     '(cperl-hash-face  ((t (:inherit font-lock-type-face))))

     ;; perl-mode
     '(perl-non-scalar-variable ((t (:inherit font-lock-type-face))))

     )))

(provide 'init-ui)
;;; init-ui.el ends here

;;; init-ui.el --- ui settings -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(leaf redacted)

(leaf topsy
  :hook (emacs-lisp-mode-hook . topsy-mode))

(leaf insecure-lock
  :commands insecure-lock-enter
  :init
  (defun insecure-lock-redact-pure ()
    "`insecure-lock' module that redacts buffers.
No changes in mode--line."
    (unless (require 'redacted nil t) (user-error "Package `redacted' not available"))
    (let ((arg (if insecure-lock-mode 1 -1)))
      (dolist (frame (frame-list))
        (dolist (window (window-list frame))
          (with-current-buffer (window-buffer window)
            (redacted-mode arg))))))

  (setq insecure-lock-require-password t)
  (setq insecure-lock-mode-hook '(insecure-lock-redact-pure insecure-lock-posframe)))

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
     `(font-lock-comment-face ((t (:foreground
                                   ,(doom-lighten (doom-color 'comments) 0.15)))))
     `(font-lock-function-name-face ((t (:foreground ,(doom-color 'green)))))
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
     ;; '(secondary-selection     ((t (:background "#384551"))))
     ;; '(completions-common-part ((t (:inherit font-lock-keyword-face))))

     ;; '(fill-column-indicator   ((t (:inherit unspecified
     ;;                                :foreground unspecified
     ;;                                :background "#181f25"))))

     ;; meow
     `(meow-normal-indicator   ((t (:foreground ,(doom-color 'blue) :bold t))))
     `(meow-insert-indicator   ((t (:foreground ,(doom-color 'green) :bold t))))
     `(meow-beacon-indicator   ((t (:foreground ,(doom-color 'yellow) :bold t))))
     `(meow-keypad-indicator   ((t (:foreground ,(doom-color 'violet) :bold t))))
     `(meow-motion-indicator   ((t (:foreground ,(doom-color 'magenta) :bold t))))
     `(meow-search-indicator   ((t (:foreground ,(doom-color 'cyan) :bold t))))
     `(meow-beacon-fake-cursor ((t (:inherit meow-beacon-indicator :inverse-video t))))
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
     `(markdown-code-face        ((t (:background ,(doom-darken (doom-color 'base3) 0.15)))))
     `(markdown-inline-code-face ((t (:background ,(doom-color 'base3)))))
     ;; ;; avy
     ;; '(avy-lead-face   ((t (:foreground "#539afc" :background unspecified))))
     ;; '(avy-lead-face-0 ((t (:foreground "#99d0f6" :background unspecified))))
     ;; '(avy-lead-face-1 ((t (:foreground "#cfe9fb" :background unspecified))))
     ;; '(avy-lead-face-2 ((t (:foreground "#f3f9fe" :background unspecified))))
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
     ;; jupyter
     '(jupyter-repl-input-prompt  ((t :inherit font-lock-property-face)))
     '(jupyter-repl-output-prompt ((t :inherit font-lock-function-name-face)))
     '(jupyter-repl-traceback     ((t :inherit font-lock-warning-face
                                      :bold t)))

     ;; web-mode
     '(web-mode-current-element-highlight-face ((t (:weight bold :inverse-video t))))
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

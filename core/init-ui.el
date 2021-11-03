;;; init-ui.el --- ui settings -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; (require 'subr-x)

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(leaf doom-modeline
  :hook (after-init-hook . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil
        doom-modeline-bar-width 0
        doom-modeline-height 15
        doom-modeline-persp-name nil
        doom-modeline-irc nil
        doom-modeline-project-detection 'projectile
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-checker-simple-format nil
        doom-modeline-indent-info nil
        doom-modeline-env-load-string "..."
        doom-modeline-vcs-max-length 20
        doom-modeline-window-width-limit (+ fill-column 20)
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-env-python-executable "/usr/local/bin/python3"))

(leaf doom-themes
  :require t
  :config
  (load-theme 'doom-city-lights t)
  :init
  (add-hook 'after-load-theme-hook #'my/doom-theme-vibrant-customize)
  (defun my/doom-theme-vibrant-customize ()
    (custom-set-faces
     '(default ((t (:foreground "#a0b3c5"
                    :background "#1d252c"))))
     '(font-lock-string-face ((t (:foreground "#008b94"))))
     '(font-lock-builtin-face ((t (:foreground "#718ca1"))))
     '(font-lock-warning-face ((t (:foreground "#d95468"))))
     '(font-lock-variable-name-face ((t (:foreground "#8bd49c"))))
     '(font-lock-function-name-face  ((t (:foreground "#c06ece"))))
     '(font-lock-regexp-grouping-backslash  ((t (:foreground "#c06ece"))))
     '(font-lock-regexp-grouping-construct  ((t (:foreground "#c06ece"))))

     '(match ((t (:inherit font-lock-variable-name-face
                  :weight bold
                  :inverse-video t))))
     '(lazy-highlight ((t (:inherit match
                           :foreground nil
                           :background nil))))
     '(completions-common-part ((t (:inherit match))))
     '(cursor ((t (:background "#a0b3c5"
                   :foreground "#1d252c"
                   ))))
     '(isearch ((t (:inherit cursor))))
     '(link ((t (:weight normal))))
     '(next-error ((t (:inherit match))))
     '(next-error-message ((t (:inherit unspecified
                               :background "#3d4451"
                               ))))

     ;; evil
     '(evil-ex-substitute-matches ((t (:inherit font-lock-warning-face
                                       :weight bold
                                       :inverse-video t))))
     '(evil-ex-substitute-replacement ((t (:inherit match))))

     ;; orderless
     '(orderless-match-face-0 ((t (:inherit font-lock-keyword-face
                                   :background nil
                                   :foreground nil
                                   :inverse-video t))))
     '(orderless-match-face-1 ((t (:inherit font-lock-warning-face
                                   :background nil
                                   :foreground nil
                                   :inverse-video t))))
     '(orderless-match-face-2 ((t (:inherit font-lock-type-face
                                   :background nil
                                   :foreground nil
                                   :inverse-video t))))
     '(orderless-match-face-3 ((t (:inherit font-lock-string-face
                                   :background nil
                                   :foreground nil
                                   :inverse-video t))))
     ;; rainbow-delimiters
     '(rainbow-delimiters-depth-1-face ((t (:foreground "#b62d65"))))
     '(rainbow-delimiters-depth-2-face ((t (:foreground "#8bd49c"))))
     '(rainbow-delimiters-depth-3-face ((t (:foreground "#539afc"))))
     '(rainbow-delimiters-depth-4-face ((t (:foreground "#ebbf83"))))
     '(rainbow-delimiters-unmatched-face ((t (:background "white"))))

     ;; vertico
     '(vertico-current ((t (:foreground "white"
                            :background "#384551"
                            :extend t))))

     ;; company
     '(company-tooltip-mouse ((t (:inherit company-tooltip :background nil))))
     '(company-tooltip-selection ((t (:inherit vertico-current :background nil))))
     '(company-tooltip-search ((t (:inherit font-lock-variable-name-face
                                   :background nil
                                   :foreground nil))))

     ;;consult
     '(consult-file ((t (:inherit font-lock-doc-face))))
     '(consult-imenu-prefix ((t (:inherit font-lock-doc-face
                                 :slant italic))))
     ;;marginalia
     '(marginalia-type ((t (:inherit font-lock-constant-face))))
     '(marginalia-key ((t (:inherit font-lock-keyword-face :weight bold))))
     '(marginalia-modified ((t (:inherit font-lock-variable-name-face))))
     '(marginalia-date ((t (:inherit font-lock-keyword-face))))

     ;;diff-hl
     '(diff-hl-change ((t (:foreground "#ebbf83" :background nil))))
     '(diff-hl-insert ((t (:background nil))))
     '(diff-hl-delete ((t (:background nil))))
     ;; yasnippet
     '(yas-field-highlight-face ((t (:inherit font-lock-variable-name-face
                                     :weight bold
                                     :background "#384551"))))
     ;;org
     `(org-block ((t (:background ,(doom-darken "#28323B" 0.15)))))
     `(org-block-begin-line ((t (:background ,(doom-darken "#28323B" 0.15)
                                 :foreground "#707b86"))))
     `(org-block-end-line ((t (:background ,(doom-darken "#28323B" 0.15)
                               :foreground "#707b86"))))
     ;; markdown-mode
     `(markdown-code-face ((t (:background ,(doom-darken "#28323B" 0.15)))))
     '(markdown-inline-code-face ((t (:background "#28323B"))))

     ;; avy
     '(avy-lead-face ((t (:foreground "#539afc" :background nil))))
     '(avy-lead-face-0 ((t (:foreground "#99d0f6" :background nil))))
     '(avy-lead-face-1 ((t (:foreground "#cfe9fb" :background nil))))
     '(avy-lead-face-2 ((t (:foreground "#f3f9fe" :background nil))))
     ;; highlight-defined
     '(highlight-defined-function-name-face
       ((t (:inherit unspecified))))
     '(highlight-defined-variable-name-face
       ((t (:inherit unspecified))))
     ;; window
     '(vertical-border ((t (:background nil))))
     ;; rg.el
     '(rg-info-face ((t (:foreground "#5ec4ff"))))
     '(rg-filename-face ((t (:foreground "#5ec4ff" :underline t))))
     '(rg-line-number-face ((t (:foreground "#ebbf83"))))
     ;; web-mode
     '(web-mode-current-element-highlight-face ((t (:weight bold :inverse-video t))))
     '(web-mode-current-column-highlight-face ((t (:background "#384551"
                                                   :foreground nil))))
     '(emmet-preview-output ((t (:inherit unspecified))))
     )))

(provide 'init-ui)
;;; init-ui.el ends here

;;; init-ui.el --- ui settings -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(leaf doom-modeline
  :doc "deps: all-the-icons"
  :hook (after-init-hook . doom-modeline-mode)
  :init
  (line-number-mode)
  (column-number-mode)
  (setq doom-modeline-icon nil
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

;; doom-theme
(leaf doom-themes
  :require t
  :config
  (load-theme 'doom-vibrant t)
  ;; modify faces
  (custom-set-faces
   '(cursor ((t (:inherit 'unspecified :background "#fcce7b" :foreground "#2e2730"))))
   ;; awesome-tray
   '(awesome-tray-module-git-face ((t (:inherit error :weight bold))))
   '(awesome-tray-module-mode-name-face ((t (:inherit font-lock-string-face :weight bold))))
   '(awesome-tray-module-buffer-name-face ((t (:inherit font-lock-type-face :weight bold))))
   '(awesome-tray-module-location-face ((t (:inherit font-lock-keyword-face :weight bold))))
   '(awesome-tray-module-evil-face ((t (:inherit default :weight bold))))
   ;;marginalia
   '(marginalia-type ((t (:inherit font-lock-constant-face))))
   '(marginalia-key ((t (:inherit font-lock-keyword-face :weight bold))))
   '(marginalia-modified ((t (:inherit font-lock-string-face))))
   '(marginalia-date ((t (:inherit font-lock-keyword-face))))
   ;;consult
   '(consult-file ((t (:inherit font-lock-doc-face))))
   ;;diff-hl
   '(diff-hl-change ((t (:background nil))))
   '(diff-hl-insert ((t (:background nil))))
   '(diff-hl-delete ((t (:background nil))))
   ;;indent-guide
   '(indent-guide-face ((t (:inherit font-lock-comment-face))))
   ;; orderless
   '(orderless-match-face-0 ((t (:inherit font-lock-type-face :weight bold))))
   '(orderless-match-face-1 ((t (:inherit error :weight bold))))
   '(orderless-match-face-2 ((t (:inherit font-lock-string-face :weight bold))))
   '(orderless-match-face-3 ((t (:inherit font-lock-keyword-face :weight bold))))
   ;; color-rg
   '(color-rg-font-lock-header-line-text ((t (:foreground "#8f60a2" :bold t))))
   '(color-rg-font-lock-header-line-keyword ((t (:foreground "#98c379" :bold t))))
   '(color-rg-font-lock-header-line-edit-mode ((t (:foreground "#56b6c2" :bold t))))
   '(color-rg-font-lock-header-line-directory ((t (:foreground "#61afef" :bold t :underline t))))
   '(color-rg-font-lock-file ((t (:foreground "#61afef" :bold t :underline t))))
   '(color-rg-font-lock-match ((t (:foreground "#98c379" :bold t))))
   '(color-rg-font-lock-command ((t (:foreground "#8f60a2" :bold t))))
   ))

(provide 'init-ui)

;;; init-ui.el ends here

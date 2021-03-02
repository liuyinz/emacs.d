;;; init-ui.el --- ui settings -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

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

(leaf awesome-tray
  :hook (after-init-hook . awesome-tray-mode)
  :init
  (setq awesome-tray-mode-line-active-color "#bbc2cf"
        awesome-tray-mode-line-inactive-color "#62686e"
        awesome-tray-buffer-name-buffer-changed t
        awesome-tray-buffer-read-only-style "[RO]"
        awesome-tray-input-method-en-style ""
        awesome-tray-refresh-idle-delay 0.01
        awesome-tray-git-update-duration 2
        awesome-tray-active-modules '("location"
                                      "rvm"
                                      "buffer-read-only"
                                      "buffer-name"
                                      "git"
                                      "mode-name"
                                      "evil"
                                      ))
  :config
  ;; HACK respect default-frame-alist width and fullscrren settings
  (defun awesome-tray-width-patch ()
    (with-selected-frame (selected-frame)
      (frame-width)))
  (advice-add 'awesome-tray-get-frame-width :override #'awesome-tray-width-patch)

  ;; HACK change git info string
  (defun module-git-info-advice (str)
    "reformat string"
    (if (not (string-empty-p str))
        (substring str 4 nil)
      ""))
  (advice-add 'awesome-tray-module-git-info :filter-return #'module-git-info-advice)

  ;; HACK change location info
  (defun module-location-info-advice ()
    "reformat location string"
    (format "%s:%s,%s"
            (format-mode-line "%l")
            (format-mode-line "%c")
            (substring (format-mode-line "%p") 0 3)))
  (advice-add 'awesome-tray-module-location-info :override #'module-location-info-advice))

(provide 'init-ui)

;;; init-ui.el ends here

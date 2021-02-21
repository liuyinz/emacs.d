;;; init-ui.el --- ui settings -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; doom-theme
(leaf doom-themes
  :require t
  :config
  (load-theme 'doom-vibrant t)
  (with-eval-after-load 'evil
    (set-face-attribute 'evil-ex-search nil
                        :inherit nil
                        :foreground "#282c34"
                        :background "#8f60a2"
                        :weight 'bold)
    (set-face-attribute 'evil-ex-lazy-highlight nil
                        :inherit nil
                        :foreground "#282c34"
                        :background "#98c379"
                        :weight 'bold))
  (with-eval-after-load 'awesome-tray
    (set-face-attribute 'awesome-tray-module-git-face nil
                        :foreground "#f76582"
                        :weight 'bold)
    (set-face-attribute 'awesome-tray-module-mode-name-face nil
                        :foreground "#7bc275"
                        :weight 'bold)
    (set-face-attribute 'awesome-tray-module-buffer-name-face nil
                        :foreground "#fcce7b"
                        :weight 'bold)
    (set-face-attribute 'awesome-tray-module-location-face nil
                        :foreground "#51afef"
                        :weight 'bold)
    (set-face-attribute 'awesome-tray-module-evil-face nil
                        :foreground "#bbc2cf"
                        :weight 'bold))
  )

(leaf awesome-tray
  :blackout t
  :hook (after-init-hook . awesome-tray-mode)
  :init
  (setq awesome-tray-mode-line-active-color "#bbc2cf"
        awesome-tray-mode-line-inactive-color "#62686e"
        awesome-tray-buffer-name-buffer-changed t
        ;; awesome-tray-file-path-show-filename t
        awesome-tray-buffer-read-only-style "[RO]"
        awesome-tray-input-method-en-style ""
        ;; awesome-tray-file-path-full-dirname-levels 1
        awesome-tray-active-modules '("location"
                                      "input-method"
                                      "rvm"
                                      "buffer-read-only"
                                      "buffer-name"
                                      "git"
                                      "mode-name"
                                      "evil"
                                      )))

(provide 'init-ui)

;;; init-ui.el ends here

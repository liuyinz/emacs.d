;;; init-ui.el --- ui settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; doom-theme
(leaf doom-themes
  :require t
  :config
  (load-theme 'doom-one t)
  (doom-themes-set-faces nil
    '(evil-ex-search :foreground "#282c34" :background "#8f60a2" :bold t)
    '(evil-ex-lazy-highlight :foreground "#282c34" :background "#98c379" :bold t))
  )

(leaf awesome-tray
  :blackout t
  :hook (after-init-hook . awesome-tray-mode)
  :init
  (setq awesome-tray-mode-line-active-color "#8f60a2"
        awesome-tray-buffer-name-buffer-changed nil
        awesome-tray-file-path-show-filename t
        awesome-tray-buffer-read-only-style "[RO]"
        awesome-tray-input-method-en-style ""
        awesome-tray-file-path-full-dirname-levels 1
        awesome-tray-active-modules '("location"
                                      "input-method"
                                      "rvm"
                                      "buffer-read-only"
                                      "file-path"
                                      "git"
                                      "mode-name"
                                      "evil"
                                      )))

(provide 'init-ui)

;;; init-ui.el ends here

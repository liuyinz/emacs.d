;;; init-ui.el --- ui settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; modeline
(leaf doom-modeline
  :doc "deps: all-the-icons"
  :blackout t
  :hook (after-init-hook . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil
        doom-modeline-persp-name nil
        doom-modeline-irc nil
        doom-modeline-project-detection 'ffip
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
  (load-theme 'doom-one t)
  (doom-themes-set-faces nil
    '(evil-ex-search :foreground "#282c34" :background "#8f60a2" :bold t)
    '(evil-ex-lazy-highlight :foreground "#282c34" :background "#98c379" :bold t))
  )

;;
(provide 'init-ui)

;;; init-ui.el ends here

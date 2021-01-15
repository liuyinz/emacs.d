;;; init-ui.el --- ui settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; modeline
(use-package doom-modeline
  :straight t
  :delight t
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

;;doom-theme
(use-package doom-themes
  :straight t
  :demand
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(provide 'init-ui)

;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;; (set-fontset-font (frame-parameter nil 'font)
;; charset (font-spec :family "Source Han Serif"))
;; (setq face-font-rescale-alist '(("Source Han Serif" . 0.95))))
;;; init-ui.el ends here

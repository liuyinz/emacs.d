;;; early-init.el --- settings before frame initialize -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Do not initialise installed packages
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; don't load sitestart.el
(setq site-run-file nil)

;; load newst file always
(setq load-prefer-newer t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; disable icon and text in frame title
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;; ;; set tile to filepath
;; (setq frame-title-format
;;       '((:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))
;;         (:eval (if (buffer-modified-p)
;;                    " *"))))

;; disable tooltip-mode
(tooltip-mode -1)

(setq default-frame-alist '((menu-bar-lines . 1)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (background-mode . dark)
                            (font . "Sarasa Mono SC 16")
                            (background-color . "#242730")
                            (foreground-color . "#bbc2cf")
                            (fullscreen . maximized)
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)
                            (ns-use-native-fullscreen . nil)))

(provide 'early-init)
;;; early-init.el ends here

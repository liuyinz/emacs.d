;;; early-init.el --- settings before frame initialize -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Defer garbage collection further back in the startup process
;; (setq gc-cons-threshold most-positive-fixnum)

;; for native-comp branch
;; (setq comp-speed 2)

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

(setq default-frame-alist '((menu-bar-lines . 1)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (background-mode . dark)
                            (font . "Sarasa Mono SC 16")
                            (background-color . "#282C34")
                            (foreground-color . "#979EAB")
                            (fullscreen . maximized)
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)
                            (ns-use-native-fullscreen . nil)))

(provide 'early-init)
;;; early-init.el ends here

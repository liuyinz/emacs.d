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

;; set tile to filepath
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        (:eval (if (buffer-modified-p)
                   " *"))))

;; Calculating width differently in terminal and window system
;; NOTE see@https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Layout.html
;; see@https://www.gnu.org/software/emacs/manual/html_node/elisp/Layout-Parameters.html
;; (frame-outer-width)
;; = (frame-border-width) + (frame-native-width)
;; = (frame-border-width) + (frame-internal-border-width) + (frame-inner-width)
;; = (frame-border-width) + (frame-internal-border-width) + (frame-scroll-bar-width) + (frame-fringe-width) + (frame-text-width);;(frame-width)
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        ;; margin belong to buffer, fringe belong to windows frame
        ;; see@https://www.gnu.org/software/emacs/manual/html_node/elisp/Fringe-Size_002fPos.html
        (left-fringe . 0)
        (right-fringe . 0)
        (minibuffer . t)
        (font . "Sarasa Mono SC 16")
        (alpha . (98 . 100))
        (fullscreen . fullheight)
        (width . 90) ; (frame-text-width) half 90,full 180
        (background-mode . dark)
        (background-color . "#242730")
        (foreground-color . "#bbc2cf")
        ;; (ns-appearance . dark)
        (ns-transparent-titlebar . t)
        (ns-use-native-fullscreen . nil)
        ))

(provide 'early-init)
;;; early-init.el ends here

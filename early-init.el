;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(require 'init-const)

;; for native-comp branch
; (setq comp-speed 2)
; (when (boundp 'comp-eln-load-path)
;   (setcar comp-eln-load-path (expand-file-name "eln-cache/" my-dir-cache)))

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; startup.el,don't load sitestart.el
(setq site-run-file nil)
;; package.el
(setq package-check-signature nil
      package-user-dir my-dir-elpa
      package-archives elpa-tsinghua
      package-quickstart t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)
(setq default-frame-alist '((menu-bar-lines . 0)
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
                            (ns-use-native-fullscreen . nil)
                            ))

; (dolist (p '((menu-bar-lines . 0)
;              (tool-bar-lines . 0)
;              (vertical-scroll-bars)
;              (horizontal-scroll-bars)
;              (font . "Sarasa Mono SC 16")
;              (background-mode . dark)
;              (background-color . "#2B2C34")
;              (foreground-color . "#979EAB")
;              (fullscreen . maximized)))
;   (push p default-frame-alist))

; (when (featurep 'ns)
;   (push '(ns-transparent-titlebar . t) default-frame-alist)
;   (push '(ns-use-native-fullscreen . nil) default-frame-alist)
;   (push '(ns-appearance . dark) default-frame-alist))

(provide 'early-init)

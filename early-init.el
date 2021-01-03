;; Defer garbage collection further back in the startup process
; (setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(require 'init-const)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
; (setq package-enable-at-startup t)
;; (advice-add #'package--ensure-init-file :override #'ignore)

(setq package-user-dir my-dir-elpa)
(setq package-archives elpa-tsinghua)
(setq package-quickstart t
      package-quickstart-file (expand-file-name "quickstart.el" my-dir-cache))
(setq package-check-signature nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-inhibit-implied-resize t)
(setq default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars)
                            (horizontal-scroll-bars)
                            (background-mode . dark)
                            (font . "Sarasa Mono SC 16")
                            (background-color . "#282C34")
                            (foreground-color . "#979EAB")
                            (fullscreen . maximized)
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)
                            (ns-use-native-fullscreen . nil)
                            ))

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

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

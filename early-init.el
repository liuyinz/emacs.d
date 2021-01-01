;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil)
;; (advice-add #'package--ensure-init-file :override #'ignore)

;; (setq package-quickstart t
      ;; package-quickstart-file "~/.emacs.d/.cache/package-quickstart.el")

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
;; (advice-add #'x-apply-session-resources :override #'ignore)

(dolist (p '((menu-bar-lines . 0)
             (tool-bar-lines . 0)
             (vertical-scroll-bars)
             (horizontal-scroll-bars)
             (font . "Sarasa Mono SC 16")
             (background-mode . dark)
             (background-color . "#2B2C34")
             (foreground-color . "#979EAB")
             (fullscreen . maximized)))
  (push p default-frame-alist))

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-use-native-fullscreen . nil) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))

(provide 'early-init)

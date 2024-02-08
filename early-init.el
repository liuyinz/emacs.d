;;; early-init.el --- settings before frame initialize -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;; Do not initialise installed packaes
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; load newst file always
(setq load-prefer-newer t)

(when (and (featurep 'native-compile)
           (fboundp 'startup-redirect-eln-cache))
  ;; disable native-comp
  (setq native-comp-jit-compilation nil)
  ;; change eln-cache position
  (startup-redirect-eln-cache
   (convert-standard-filename
	(expand-file-name  ".cache/var/eln-cache/" user-emacs-directory))))

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; set tile to filepath
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        (:eval (if (buffer-modified-p)
                   " *"))))

;; SEE https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Layout.html#index-frame_002dedges
;; frame position in pixels
;; (frame-edges nil 'outer-edges)
;; = "outer-border" + (frame-edges nil 'native-edges)
;; = "outer-border" + "internal-border" + (frame-edges nil 'inner-edges)

;; SEE https://www.gnu.org/software/emacs/manual/html_node/elisp/Layout-Parameters.html
;; frame width in pixels
;; (frame-outer-width)
;; = (frame-native-width) + "external-border,gui only"|"outer-border,child-frame or tooltip only"
;; = (frame-internal-border-width)|(frame-border-width) + (frame-inner-width)
;; = (frame-internal-border-width)|(frame-border-width) + (frame-scroll-bar-width) + (frame-fringe-width) + (frame-text-width)|(frame-width)

;; SEE https://www.gnu.org/software/emacs/manual/html_node/elisp/Fringe-Size_002fPos.html
;; margin belong to buffer, fringe belong to windows frame
;; (frame-width) = (window-margins) + "maximal column number"

(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (left-fringe . 0)
        (right-fringe . 0)
        (internal-border-width . 0)
        (width . 90) ; (frame-text-width) half 90,full 180
        (fullscreen . fullheight)
        ;; (background-mode . dark)
        ;; (background-color . "#1d252c")
        ;; (foreground-color . "#bbc2cf")

        ;; (alpha . (98 . 100))

        ;; NOTE only work in X-windows
        ;; (alpha-background . 20)
        ))

(when (featurep 'ns)
  ;; disable icon and text in frame title
  (setq ns-use-proxy-icon nil)

  (push '(ns-transparent-titlebar . t) default-frame-alist)
  ;; (push '(ns-appearance . dark) default-frame-alist)
  (push '(ns-use-native-fullscreen . nil) default-frame-alist))

(when (featurep 'mac)

  ;; SEE https://gist.github.com/railwaycat/3498096
  (setq mac-option-modifier 'meta
        mac-command-modifier 'hyper)

  ;; HACK https://emacs-china.org/t/emacs-mac-port/15056/3?u=cheunghsu
  (set-face-background 'default "#1d252c")
  (dolist (f (face-list)) (set-face-stipple f "alpha:40%"))
  (setq face-remapping-alist (append face-remapping-alist '((default my/default-blurred))))
  (defface my/default-blurred
    '((t :inherit 'default :stipple "alpha:40%"))
    "Like 'default but blurred."
    :group 'my)
  )

;;; early-init.el ends here

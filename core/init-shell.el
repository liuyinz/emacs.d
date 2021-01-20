;;; init-shell.el --- shell setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; beautiful term mode & friends
(use-package vterm
  :straight t
  :init
  (setq vterm-always-compile-module t
        vterm-kill-buffer-on-exit t
        vterm-clear-scrollback-when-clearing nil
        vterm-max-scrollback 10000)
  (setenv "COLORTERM" "truecolor")
  ;; :config
  ;; (add-to-list 'vterm-keymap-exceptions "M-p")
  )

(use-package vterm-toggle
  :straight t
  :commands vterm-toggle
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '("^v?term.*"

                 ;; (display-buffer-reuse-window display-buffer-in-side-window)
                 ;; (side . bottom)

                 ;;display-buffer-in-direction/direction/dedicated added in emacs27
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . bottom)
                 (dedicated . t)

                 (reusable-frames . visible)
                 (window-height . 0.4))))

(provide 'init-shell)
;;; init-shell.el ends here

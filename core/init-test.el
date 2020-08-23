;; beautiful term mode & friends
(use-package vterm
  :init
  (setq vterm-always-compile-module t
        vterm-kill-buffer-on-exit t
        vterm-clear-scrollback-when-clearing nil
        vterm-max-scrollback 10000)
  ;; :config
  ;; (add-to-list 'vterm-keymap-exceptions "M-p")
  )

(use-package vterm-toggle
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '("^v?term.*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (dedicated . t)
                 (reusable-frames . visible)
                 (window-height . 0.4))))

(provide 'init-test)

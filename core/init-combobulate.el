;;; init-combobulate.el --- summary -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2024-05-30 15:47:01

;;; Commentary:

;;; Code:

(leaf combobulate
  :hook
  ((jtsx-tsx-mode-hook jtsx-jsx-mode-hook) . combobulate-mode)
  :init
  (setq combobulate-proffer-allow-numeric-selection t
        combobulate-flash-node nil
        combobulate-proffer-indicators "ox")
  :defer-config
  ;; BUG esc failed
  (keymap-set combobulate-proffer-map "ESC" 'cancel)
  (keymap-set combobulate-proffer-map "C-e" 'cancel)
  )

(provide 'init-combobulate)
;;; init-combobulate.el ends here

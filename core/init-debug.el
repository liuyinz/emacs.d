;;; init-debug.el --- minimal config to debug -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; -------------------------- Default ------------------------------

(column-number-mode)
(line-number-mode)
(size-indication-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(load-theme 'wombat)

(recentf-mode)
(global-set-key "\C-o" #'recentf-open-files)
(setq recentf-max-saved-items nil)

;; --------------------------- Debug -------------------------------




(provide 'init-debug)
;;; init-debug.el ends here

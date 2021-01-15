;;; init-rg.el --- Rg setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package color-rg
  :straight (:type git :host github :repo "manateelazycat/color-rg")
  :commands (color-rg-search-project color-rg-search-input)
  :init
  (setq color-rg-mac-load-path-from-shell nil))

(provide 'init-rg)
;;; init-rg.el ends here

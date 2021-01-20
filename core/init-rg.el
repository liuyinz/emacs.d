;;; init-rg.el --- Rg setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package color-rg
  :straight (:type git :host github :repo "manateelazycat/color-rg")
  :commands (color-rg-search-project-with-type
             color-rg-search-input
             color-rg-search-input-in-project
             color-rg-search-input-in-current-file)
  :init
  (setq color-rg-mac-load-path-from-shell nil))

(provide 'init-rg)
;;; init-rg.el ends here

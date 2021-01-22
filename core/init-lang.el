;;; init-prog.el --- language setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf sh-script
  :hook (sh-mode-hook . my-sh-mode-setup)
  :config
  (defun my-sh-mode-setup ()
    (when (and (string-match "\\.sh$" buffer-file-name)
               (executable-find "shellcheck"))
      (flycheck-select-checker 'sh-shellcheck))))

(leaf lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (setq lua-indent-level 2)
  (setq lua-indent-string-contents t)
  (setq lua-prefix-key nil))

(leaf nxml-mode
  :mode ("\\.xaml$" . xml-mode))

(leaf yaml-mode)
;; (use-package cask-mode)
;; (use-package csharp-mode)
;; (use-package csv-mode)
;; (use-package julia-mode)
;; (use-package mermaid-mode)
(leaf plantuml-mode)
;; (use-package powershell)
;; (use-package rmsbolt)
;; (use-package scala-mode)
;; (use-package swift-mode)
(leaf vimrc-mode)

(provide 'init-lang)
;;; init-prog.el ends here

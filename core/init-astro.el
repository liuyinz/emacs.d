;;; init-astro.el --- setup for astro -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2024-10-25 17:07:04

;;; Commentary:

;;; Code:

;; Setup major mode
(add-to-list 'auto-mode-alist '("\\.astro\\'" . web-astro-mode))
(define-derived-mode web-astro-mode web-mode "Astro")

;; Setup lsp
(with-eval-after-load 'lsp-bridge
  (add-to-list 'lsp-bridge-default-mode-hooks 'web-astro-mode-hook)
  (add-to-list 'lsp-bridge-single-lang-server-extension-list '(("astro") . "astro-ls")))

(provide 'init-astro)
;;; init-astro.el ends here

;;; init-bridge.el --- Setup for lsp-bridge -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-07-09 08:07:53

;;; Commentary:

;;; Code:

;; REQUIRE pip install epc
(leaf lsp-bridge
  :hook (after-init-hook . global-lsp-bridge-mode)
  :defer-config
  (setq lsp-bridge-enable-signature-help t)

  ;; (push (cons '("css") "emmet-ls") lsp-bridge-lang-server-extension-list)

  ;; ;; Debug: REQUIRE brew install gdb
  ;; (setq lsp-bridge-enable-log t
  ;;       lsp-bridge-enable-debug t)

  )

(provide 'init-bridge)
;;; init-bridge.el ends here

;;; init-lsp.el --- setting for lsp  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)


;; Microsoft python-language-server support
;; (leaf lsp-python-ms
;;   :hook (python-mode . (lambda () (require 'lsp-python-ms)))
;;   :init
;;   (when (executable-find "python3")
;;     (setq lsp-python-ms-python-executable-cmd "python3")))

(provide 'init-lsp)
;;; init-lsp.el ends here

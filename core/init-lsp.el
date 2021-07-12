;;; init-lsp.el --- setting for lsp  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; lsp-mode
(leaf lsp-mode
  :doc "deps: ht lv spinner dash f"
  :commands lsp-deferred lsp
  :hook ((js-mode-hook
          css-mode-hook
          yaml-mode-hook
          html-mode-hook) . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "s-l")
  (setq lsp-enable-snippet t)
  (setq lsp-enable-links nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-file-watchers nil)         ;; performance matters
  (setq lsp-enable-text-document-color nil)   ;; as above
  (setq lsp-enable-symbol-highlighting nil)   ;; as above
  (setq lsp-enable-on-type-formatting nil)    ;; as above
  (setq lsp-enable-indentation nil)
  (setq lsp-headerline-breadcrumb-enable nil) ;; keep headline clean
  (setq lsp-modeline-code-actions-enable nil) ;; keep modeline clean
  (setq lsp-modeline-diagnostics-enable nil)  ;; as above
  (setq lsp-keep-workspace-alive nil)         ;; auto kill lsp server
  (setq lsp-eldoc-enable-hover nil)           ;; disable eldoc hover
  (setq lsp-log-io nil)                       ;; debug only
  (setq lsp-auto-guess-root t)                ;; auto guess root
  :config
  (leaf lsp-modeline :require t)

  ;; HACK disable fuzzy match
  ;; see@https://github.com/emacs-lsp/lsp-mode/issues/2563#issuecomment-767987191
  (advice-add #'lsp-completion--regex-fuz :override #'identity)
  )

(leaf lsp-pyright
  :doc "deps: ht dash"
  :hook (python-mode-hook . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred))))

(leaf ccls
  :doc "deps : ccls"
  :hook ((c-mode-hook
          c++-mode-hook
          objc-mode-hook
          cuda-mode-hook) . (lambda () (require 'ccls) (lsp-deferred))))

(provide 'init-lsp)
;;; init-lsp.el ends here

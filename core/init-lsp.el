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
  ;; :bind (:map lsp-mode-map
  ;;        ("C-c f" . lsp-format-region)
  ;;        ("C-c d" . lsp-describe-thing-at-point)
  ;;        ("C-c a" . lsp-execute-code-action)
  ;;        ("C-c r" . lsp-rename))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-links nil)                 ;; no clickable linkscondy, 10 months ago: • Refine cc-mode
  (setq lsp-enable-folding nil)               ;; use `hideshow' instead
  (setq lsp-enable-snippet nil)               ;; no snippets, it requires `yasnippet'
  (setq lsp-enable-file-watchers nil)         ;; performance matters
  (setq lsp-enable-text-document-color nil)   ;; as above
  (setq lsp-enable-symbol-highlighting nil)   ;; as above
  (setq lsp-enable-on-type-formatting nil)    ;; as above
  (setq lsp-enable-indentation nil)           ;; don't change my code without my permission
  (setq lsp-headerline-breadcrumb-enable nil) ;; keep headline clean
  (setq lsp-modeline-code-actions-enable nil) ;; keep modeline clean
  (setq lsp-modeline-diagnostics-enable nil)  ;; as above
  (setq lsp-log-io nil)                       ;; debug only
  (setq lsp-auto-guess-root t)                ;; auto guess root
  (setq lsp-keep-workspace-alive nil)         ;; auto kill lsp server
  (setq lsp-eldoc-enable-hover nil))          ;; disable eldoc hover

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

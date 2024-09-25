;;; init-lsp.el --- setting for lsp  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------- lsp-mode -----------------------------

;; SEE https://emacs-lsp.github.io/lsp-mode/
(leaf lsp-mode
  :hook
  ((bash-ts-mode-hook
    html-mode-hook
    web-mode-hook
    css-ts-mode-hook
    js-ts-mode-hook
    typescript-ts-mode-hook
    python-ts-mode-hook
    yaml-ts-mode-hook) . lsp-deferred)
  (lsp-mode-hook . lsp-mode-setup)
  :init

  (defun lsp-mode-setup ()
    (lsp-completion-mode)
    (lsp-diagnostics-mode)
    (lsp-enable-which-key-integration))

  ;; debugging
  ;; (setq lsp-log-io t)

  (setq lsp-keymap-prefix "C-c l")

  ;; completions
  (setq lsp-completion-enable t)
  (setq lsp-completion-provider :none)

  ;; diagnostics
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-diagnostics-flycheck-default-level 'warning)

  ;; headerline
  (setq lsp-headerline-breadcrumb-enable t)

  ;; performance
  ;; SEE https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setenv "LSP_USE_PLISTS" "true")
  (setq lsp-use-plists t)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  (setq lsp-warn-no-matched-clients nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-idle-delay 0.5)

  ;; SEE https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)

  ;; (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-ui-doc-show-with-cursor nil)
  ;; (setq lsp-ui-doc-show-with-mouse nil)
  ;; (setq lsp-ui-sideline-enable nil)
  ;; (setq lsp-ui-sideline-show-code-actions nil)
  ;; (setq lsp-ui-sideline-show-hover nil)

  ;; -------------------------- feature ------------------------------

  (setq lsp-enable-snippet nil)
  (setq lsp-enable-links nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-enable-dap-auto-configure nil)
  (setq lsp-before-save-edits nil)
  (setq lsp-lens-enable nil)

  (setq lsp-enable-imenu t
        lsp-imenu-show-container-name nil
        lsp-imenu-detailed-outline nil
        lsp-imenu-index-function #'lsp-imenu-create-categorized-index)

  ;; (leaf lsp-modeline :require t)
  ;; (leaf consult-lsp :require t)

  :defer-config

  (setq lsp-disabled-clients '(lsp-volar))
  (require 'init-lsp-langs))

(provide 'init-lsp)
;;; init-lsp.el ends here

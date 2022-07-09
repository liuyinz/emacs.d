;;; init-lsp.el --- setting for lsp  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------- lsp-mode -----------------------------

;; SEE https://emacs-lsp.github.io/lsp-mode/
(leaf lsp-mode
  :hook ((sh-mode-hook
          html-mode-hook
          css-mode-hook
          js-mode-hook
          yaml-mode-hook) . lsp-deferred)
  :init

  ;; --------------------------- debug -------------------------------

  ;; debugging
  ;; (setq lsp-log-io t)

  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-keymap-prefix nil)
  (setq lsp-completion-enable t)
  (setq lsp-completion-provider :none)
  ;; (setq lsp-diagnostics-provider :none)

  ;; ------------------------ performance ----------------------------

  ;; REQUIRE export LSP_USE_PLISTS=true
  (setq lsp-use-plists t)

  ;; performance enhancing
  (setq lsp-warn-no-matched-clients nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-enable-file-watchers nil)

  ;; SEE https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  ;; (setq lsp-completion-show-detail nil)
  (setq lsp-completion-show-kind nil)
  ;; (setq lsp-headerline-breadcrumb-enable nil)

  ;; -------------------------- feature ------------------------------

  (setq lsp-enable-snippet nil)
  (setq lsp-enable-links nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-enable-dap-auto-configure nil)

  (setq lsp-enable-imenu t
        lsp-imenu-show-container-name nil
        lsp-imenu-detailed-outline nil
        lsp-imenu-index-function #'lsp-imenu-create-categorized-index)

  ;; -------------------------- server ------------------------------

  (defun my/emmet-ls-setup ()
    "Setup for emmet-ls"
    (setq-local corfu-auto-delay 0.8
                corfu-preselect-first t
                corfu-doc-delay 0.1)
    (setq-local lsp-completion-show-detail nil))
  (add-hook 'lsp-emmet-ls-after-open-hook #'my/emmet-ls-setup)

  (leaf lsp-pyright
    :hook (python-mode-hook . (lambda ()
                                (require 'lsp-pyright)
                                (lsp-deferred))))

  ;; ;; REQUIRE deps : ccls
  ;; (leaf ccls
  ;;   :hook ((c-mode-hook c++-mode-hook) . (lambda ()
  ;;                                          (require 'ccls)
  ;;                                          (lsp-deferred))))

  :defer-config

  (leaf lsp-modeline :require t)
  (leaf consult-lsp :require t)

  )

(provide 'init-lsp)
;;; init-lsp.el ends here

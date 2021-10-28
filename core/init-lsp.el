;;; init-lsp.el --- setting for lsp  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------- Default ------------------------------

(use-package lsp-mode
  :hook ((js2-mode-hook bash-mode-hook go-mode-hook) . lsp-deferred)
  :init
  (setq lsp-keymap-prefix nil)
  (setq lsp-completion-enable t)
  (setq lsp-enable-snippet nil)
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

  ;; ;; HACK disbale lsp in md/org-mode
  ;; (defun ad/disable-lsp-in-md-org (fn)
  ;;   "Call `FN' when needed"
  ;;   (unless (string-match "\\.\\(?:md\\|org\\)\\'" (buffer-file-name))
  ;;     (funcall fn)))
  ;; (advice-add 'lsp :around #'ad/disable-lsp-in-md-org)

  :config
  (use-package lsp-modeline :demand t)

  ;; -------------------------- Imenu -------------------------------

  (setq lsp-enable-imenu t
        lsp-imenu-show-container-name nil
        lsp-imenu-detailed-outline nil
        lsp-imenu-index-function #'lsp-imenu-create-categorized-index)

  (with-eval-after-load 'consult-imenu
    (appendq! consult-imenu-config
              '((js2-mode :types
                  ((?c "Classes"    font-lock-type-face)
                   (?f "Functions"  font-lock-function-name-face)
                   (?s "Constants"  font-lock-constant-face)
                   (?m "Methods"    font-lock-string-face)
                   (?p "Properties" font-lock-builtin-face)
                   (?v "Variables"  font-lock-variable-name-face)))
                (python-mode :types
                  ((?c "Classes"    font-lock-type-face)
                   (?f "Functions"  font-lock-function-name-face)
                   (?v "Variables"  font-lock-variable-name-face)))
                (sh-mode :types
                  ((?f "Functions" font-lock-function-name-face)
                   (?v "Variables" font-lock-variable-name-face)))
                )))
  )

;; -------------------------- Server ------------------------------

(use-package lsp-pyright
  :hook (python-mode-hook . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred))))

;; REQUIRE deps : ccls
(use-package ccls
  :hook ((c-mode-hook
          c++-mode-hook
          objc-mode-hook
          cuda-mode-hook) . (lambda () (require 'ccls) (lsp-deferred))))

(provide 'init-lsp)
;;; init-lsp.el ends here

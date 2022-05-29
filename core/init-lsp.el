;;; init-lsp.el --- setting for lsp  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------- lsp-mode -----------------------------

(leaf lsp-mode
  :hook ((js-mode-hook sh-mode-hook go-mode-hook) . lsp-deferred)
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  ;; SEE https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq lsp-keymap-prefix nil)
  (setq lsp-completion-enable t)
  (setq lsp-completion-provider :none)
  (setq lsp-idle-delay 0.3)
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
  (setq lsp-log-io t)                       ;; debug only
  (setq lsp-auto-guess-root t)                ;; auto guess root

  ;; REQUIRE export LSP_USE_PLISTS=true
  (setq lsp-use-plists t)

  ;; ;; HACK disbale lsp in md/org-mode
  ;; (defun ad/disable-lsp-in-md-org (fn)
  ;;   "Call `FN' when needed"
  ;;   (unless (string-match "\\.\\(?:md\\|org\\)\\'" (buffer-file-name))
  ;;     (funcall fn)))
  ;; (advice-add 'lsp :around #'ad/disable-lsp-in-md-org)

  (leaf lsp-pyright
    :hook (python-mode-hook . (lambda ()
                                (require 'lsp-pyright)
                                (lsp-deferred))))

  ;; REQUIRE deps : ccls
  (leaf ccls
    :hook ((c-mode-hook
            c++-mode-hook) . (lambda () (require 'ccls) (lsp-deferred))))

  :defer-config
  (leaf lsp-modeline :require t)
  (leaf consult-lsp :require t)

  (setq lsp-enable-imenu t
        lsp-imenu-show-container-name nil
        lsp-imenu-detailed-outline nil
        lsp-imenu-index-function #'lsp-imenu-create-categorized-index)

  (with-eval-after-load 'consult-imenu
    (appendq! consult-imenu-config
              '((js-mode :types
                  ((?c "Classes"    font-lock-type-face)
                   (?f "Functions"  font-lock-function-name-face)
                   (?s "Constants"  font-lock-constant-face)
                   (?m "Methods"    font-lock-string-face)
                   (?p "Properties" font-lock-builtin-face)
                   (?v "Variables"  font-lock-variable-name-face)
                   (?e "Fields"     font-lock-warning-face)))
                (python-mode :types
                  ((?c "Classes"    font-lock-type-face)
                   (?f "Functions"  font-lock-function-name-face)
                   (?v "Variables"  font-lock-variable-name-face)))
                (sh-mode :types
                  ((?f "Functions" font-lock-function-name-face)
                   (?v "Variables" font-lock-variable-name-face)))
                )))
  )

;; --------------------------- eglot -------------------------------

;; (leaf eglot
;;   :hook ((python-mode-hook
;;           js-mode-hook
;;           bash-mode-hook
;;           go-mode-hook) . eglot-ensure)
;;   :defer-config
;;   (setq eglot-stay-out-of '(flymake company))
;;   (setq eglot-events-buffer-size 0)

;;   ;; add taliwindcss server
;;   ;; (add-to-list 'eglot-server-programs '((web-mode :language-id "html") . ("tailwindcss-language-server")))

;;   (with-eval-after-load 'consult-imenu
;;     (appendq! consult-imenu-config
;;               '((js-mode :types
;;                   ((?c "Class"     font-lock-type-face)
;;                    (?f "Function"  font-lock-function-name-face)
;;                    (?s "Constant"  font-lock-constant-face)
;;                    (?m "Method"    font-lock-string-face)
;;                    (?p "Property"  font-lock-builtin-face)
;;                    (?v "Variable"  font-lock-variable-name-face)
;;                    (?e "Fields"     font-lock-warning-face)))
;;                 (python-mode :types
;;                   ((?c "Class"     font-lock-type-face)
;;                    (?f "Function"  font-lock-function-name-face)
;;                    (?v "Variable"  font-lock-variable-name-face)))
;;                 (sh-mode :types
;;                   ((?f "Function"  font-lock-function-name-face)
;;                    (?v "Variable"  font-lock-variable-name-face)))
;;                 )))
;;   )

;; ------------------------ lsp-bridge ----------------------------

;; ;; BUG https://emacs-china.org/t/lsp-bridge/20786/934?u=cheunghsu
;; ;; REQUIRE pip install epc
;; (leaf lsp-bridge
;;   :hook ((js-mode-hook
;;           bash-mode-hook
;;           go-mode-hook
;;           python-mode-hook
;;           web-mode-hook
;;           css-mode-hook) . lsp-bridge-mode)
;;   :defer-config
;;   (setq lsp-bridge-enable-auto-import t
;;         ;; lsp-bridge-enable-log t
;;         ;; lsp-bridge-enable-debug t
;;         )

;;   ;; (require 'lsp-bridge-orderless)

;;   )

(provide 'init-lsp)
;;; init-lsp.el ends here

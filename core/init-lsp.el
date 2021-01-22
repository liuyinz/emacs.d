(require 'init-const)

(leaf lsp-mode
  :blackout
  :commands (lsp-install-server
             lsp
             lsp-deferred
             lsp-format-buffer
             lsp-organize-imports)
  :hook (((python-mode
           js2-mode
           css-mode
           web-mode
           html-mode
           sh-mode) . (lsp-deferred))
         (lsp-mode . (lambda ()
                       ;; Format and organize imports
                       ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
                       (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :bind (:lsp-mode-map
         ("C-c C-d" . lsp-describe-thing-at-point)
         ([remap xref-find-definitions] . lsp-find-definition)
         ([remap xref-find-references] . lsp-find-references))
  :init
  (setq gc-cons-threshold 100000000)
  ;; @see https://github.com/emacs-lsp/lsp-mode#performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-diagnostic-package :none
        ;; completion
        lsp-prefer-capf nil
        lsp-auto-guess-root t
        ;; lsp-keymap-prefix nil
        ;;disable file wathcer when large file
        lsp-enable-file-watchers nil
        ;; enable log only for debug
        lsp-log-io nil
        lsp-enable-text-document-color nil
        lsp-signature-auto-activate  nil
        lsp-enable-folding nil
        ;; Don't modify our code without our permission
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        ;; turn off for better performance
        lsp-enable-symbol-highlighting nil
        ;; Disable eldoc displays in minibuffer
        lsp-eldoc-enable-hover nil
        ;; auto kill server
        lsp-keep-workspace-alive nil)
  ;; For `lsp-clients'
  (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  (unless (executable-find "rls")
    (setq lsp-rust-rls-server-command '("rustup" "run" "stable" "rls"))))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :init (setq lsp-ui-doc-enable t
;;               lsp-ui-doc-use-webkit nil
;;               lsp-ui-doc-delay 0.2
;;               lsp-ui-doc-include-signature t
;;               lsp-ui-doc-position 'at-point
;;               lsp-ui-doc-border (face-foreground 'default)
;;               lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

;;               lsp-ui-sideline-enable t
;;               lsp-ui-sideline-show-hover nil
;;               lsp-ui-sideline-show-diagnostics nil
;;               lsp-ui-sideline-ignore-duplicate t

;;               lsp-ui-imenu-enable t
;;               lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
;;                                     ,(face-foreground 'font-lock-string-face)
;;                                     ,(face-foreground 'font-lock-constant-face)
;;                                     ,(face-foreground 'font-lock-variable-name-face)))
;;   :config
;;   ;; (set-face-attribute 'lsp-ui-sideline-code-action ((t (:inherit warning))))
;;   (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

;;   ;; `C-g'to close doc
;;   (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

;;   ;; Reset `lsp-ui-doc-background' after loading theme
;;   (add-hook 'after-load-theme-hook
;;             (lambda ()
;;               (setq lsp-ui-doc-border (face-foreground 'default))
;;               (set-face-background 'lsp-ui-doc-background
;;                                    (face-background 'tooltip)))))

;; ;; Ivy integration
;; (use-package lsp-ivy
;;   :after lsp-mode
;;   :bind (:map lsp-mode-map
;;               ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
;;               ("C-s-." . lsp-ivy-global-workspace-symbol)))

;; (use-package dap-mode
;;   :functions dap-hydra/nil
;;   :blackout
;;   :bind (:map lsp-mode-map
;;               ("<f5>" . dap-debug)
;;               ("M-<f5>" . dap-hydra))
;;   :hook ((after-init . dap-mode)
;;          (dap-mode . dap-ui-mode)
;;          (dap-session-created . (lambda (_args) (dap-hydra)))
;;          (dap-stopped . (lambda (_args) (dap-hydra)))
;;          (dap-terminated . (lambda (_args) (dap-hydra/nil)))

;;          (python-mode . (lambda () (require 'dap-python)))
;;          (ruby-mode . (lambda () (require 'dap-ruby)))
;;          (go-mode . (lambda () (require 'dap-go)))
;;          (java-mode . (lambda () (require 'dap-java)))
;;          ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
;;          (php-mode . (lambda () (require 'dap-php)))
;;          (elixir-mode . (lambda () (require 'dap-elixir)))
;;          ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
;;          (powershell-mode . (lambda () (require 'dap-pwsh)))))

;; Microsoft python-language-server support
(leaf lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :init
  (when (executable-find "python3")
    (setq lsp-python-ms-python-executable-cmd "python3")))

(provide 'init-lsp)

;;; init-bridge.el --- Setup for lsp-bridge -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-07-09 08:07:53

;;; Commentary:

;;; Code:

(leaf lsp-bridge
  :hook (after-init-hook . global-lsp-bridge-mode)
  :init
  (defun lsp-bridge-mode-setup ()
    "Setup lsp-bridge-mode"
    ;; Allow char : in buffer which enable emmet-ls completions.
    (when (ignore-errors (string-match-p "emmet" (lsp-bridge-has-lsp-server-p)))
      (setq-local lsp-bridge-completion-hide-characters
                  (delete ":" lsp-bridge-completion-hide-characters)))
    ;; disable diagnostics feature in bash-language-server,
    ;; SEE https://github.com/bash-lsp/bash-language-server/issues/983
    ;; (when (member major-mode '(sh-mode bash-mode bash-ts-mode))
    ;;   (setq-local lsp-bridge-enable-diagnostics nil))
    )
  (add-hook 'lsp-bridge-mode-hook #'lsp-bridge-mode-setup -100)

  :defer-config

  ;; ;; Debug: REQUIRE brew install gdb
  ;; (setq lsp-bridge-enable-log t)
  ;; (setq lsp-bridge-enable-debug t)

  (setq acm-candidate-match-function 'orderless-flex)
  (setq lsp-bridge-enable-diagnostics t
        lsp-bridge-disable-backup nil)
  (setq lsp-bridge-enable-completion-in-string t)
  (appendq! lsp-bridge-default-mode-hooks
            '(snippet-mode-hook
              git-commit-mode-hook
              mhtml-mode-hook
              html-mode-hook
              js-json-mode-hook
              vue-ts-mode-hook))

  ;; Setup server
  (setq lsp-bridge-user-langserver-dir
        (expand-file-name "lsp-bridge/single" my/dir-ext))
  (setq lsp-bridge-user-multiserver-dir
        (expand-file-name "lsp-bridge/multi" my/dir-ext))

  ;; Setup language
  ;; REQUIRE pthon3.11 -m pip install epc orjson sexpdata six paramiko ruff-lsp
  (setq lsp-bridge-python-command "python3.11")
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")

  (setq lsp-bridge-get-multi-lang-server-by-project #'my/bridge-multi-server-detect)
  (defun my/bridge-multi-server-detect (project-path filepath)
    "Detect right server config for jsx/tsx project."
    (save-excursion
      ;; detect for web dev
      (let* ((tailwindcss-p (directory-files
                             project-path
                             'full
                             "tailwind\\.config\\.\\(j\\|cj\\|mj\\|t\\)s\\'"))
             (ext (file-name-extension filepath))
             (jsx-p (or (string= ext "jsx")
                        (memq major-mode '(jtsx-jsx-mode  js-jsx-mode))))
             (tsx-p (or (string= ext "tsx")
                        (memq major-mode '(jtsx-tsx-mode
                                           jtsx-typescript-mode
                                           tsx-ts-mode))))
             (html-p
              (or (memq ext '("htm" "html"))
                  (memq major-mode '(mhtml-mode html-mode html-ts-mode))))
             (css-p
              (or (memq ext '("css"))
                  (memq major-mode '(css-mode css-ts-mode less-css-mode scss-mode))))
             (server (cond
                      ;; for jsx/tsx
                      ((and tailwindcss-p jsx-p) "javascriptreact_tailwindcss")
                      ((and tailwindcss-p tsx-p) "typescriptreact_tailwindcss")
                      ;; for html
                      ((and tailwindcss-p html-p) "html_emmet_tailwindcss")
                      (html-p "html_emmet")
                      ;; for css
                      ((and tailwindcss-p css-p) "css_emmet_tailwindcss")
                      (css-p "css_emmet"))))
        server)))

  ;; SEE https://github.com/Microsoft/vscode-eslint#settings-options
  ;; SEE https://github.com/neoclide/coc-css
  ;; SEE https://github.com/neoclide/coc-json#configuration-options
  ;; SEE https://github.com/redhat-developer/yaml-language-server#language-server-settings
  ;; NOTE yaml-language-server, coc-json implements schemastore supports, however
  ;; vscode-json-language-server not yet, which means need to add schemastore catalog
  ;; to vscode-json-language-server.json manually.

  ;; ISSUE https://github.com/redhat-developer/yaml-language-server/issues/807
  ;; yaml server init error: "Cannot read properties of undefined (reading ’length’)"

  ;; priority order
  ;; (appendq! lsp-bridge-single-lang-server-extension-list nil)

  (appendq! lsp-bridge-single-lang-server-mode-list
            '(((toml-ts-mode conf-toml-mode) . "toml-language-server")))

  (setq lsp-bridge-multi-lang-server-extension-list nil)

  (leaf acm
    :init
    (setq acm-enable-quick-access nil
          acm-enable-tabnine nil
          acm-enable-doc nil)
    ;; yasnippet
    (setq acm-completion-backend-merge-order
          '("template-first-part-candidates"
            "mode-first-part-candidates"
            "template-second-part-candidates"
            "mode-second-part-candidates"))

    (setq acm-backend-yas-candidates-number 3
          acm-backend-yas-match-by-trigger-keyword t
          acm-backend-yas-show-trigger-keyword " [%s]")

    ;; FIXME wrapper of `lsp-bridge-toggle-sdcv-helper'
    (defun acm-sdcv-toggle ()
      "docstring"
      (interactive)
      (let ((inhibit-message t))
        (lsp-bridge-toggle-sdcv-helper)
        (acm-update)
        (acm-menu-update)))

    (defun ad/acm-doc-toggle ()
      (interactive)
      (if (acm-frame-visible-p acm-doc-frame)
          (progn
            (acm-doc-hide)
            (setq acm-enable-doc nil))
        (setq acm-enable-doc t)
        (acm-doc-try-show)))
    (advice-add 'acm-doc-toggle :override #'ad/acm-doc-toggle)))

(leaf lsp-bridge-ref
  :hook (lsp-bridge-ref-mode-hook . (lambda () (meow-mode -1))))

(provide 'init-bridge)
;;; init-bridge.el ends here

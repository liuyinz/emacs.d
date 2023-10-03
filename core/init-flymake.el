;;; init-flymake.el --- Config for lint -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2023-08-01 17:01:50

;;; Commentary:

;;; Code:

(leaf flymake
  :hook ((prog-mode-hook markdown-mode-hook nxml-mode-hook) . flymake-mode)
  :init
  (setq flymake-fringe-indicator-position nil
        elisp-flymake-byte-compile-load-path load-path
        flymake-no-changes-timeout 1.5))

(leaf flymake-collection
  :hook (after-init-hook . flymake-collection-hook-setup)
  :init
  ;; REQUIRE
  ;; pip install ruff-lsp yamllint
  ;; brew install shellcheck tidy-html5 jq
  ;; gem install mdl
  ;; luarocks install luacheck
  ;; npm install -g less
  (setq flymake-collection-hook-config
        '(((python-ts-mode python-mode) flymake-collection-ruff)
          ((yaml-mode yaml-ts-mode) flymake-collection-yamllint)
          ((web-mode html-ts-mdoe) flymake-collection-html-tidy)
          ((js-ts-mode typescript-ts-mode) flymake-collection-eslint)
          ((json-mode json-ts-mode) flymake-collection-jq)
          ((markdown-mode gfm-mode) flymake-collection-markdownlint)
          ((lua-mode lua-ts-mode) flymake-collection-luacheck)
          ((ruby-mode ruby-ts-mode) flymake-collection-rubocop)
          (sql-mode flymake-collection-sqlint)
          (nxml-mode flymake-collection-xmllint)
          (less-mode flymake-collection-less))))

(leaf package-lint-flymake
  :hook (emacs-lisp-mode-hook . package-lint-flymake-setup))

(leaf flymake-relint
  :hook ((emacs-lisp-mode-hook lisp-interaction-mode-hook) . flymake-relint-setup))

(leaf flymake-bridge
  :hook (lsp-bridge-mode-hook . flymake-bridge-setup))

(provide 'init-flymake)
;;; init-flymake.el ends here

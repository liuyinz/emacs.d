;;; init-lang.el --- language setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;;; ------------------------- Builtin ------------------------------

(use-package sh-script
  :init
  (setq sh-basic-offset 2
        sh-shell-file (executable-find "bash"))

  ;; HACK create `bash-mode-hook'
  (defvar bash-mode-hook nil "Hook run after enter sh-mode[Bash]")
  (defun run-bash-mode-hook (&rest _)
    "Run `bash-mode-hook' when enabled."
    (when (eq sh-shell 'bash)
      (run-hooks 'bash-mode-hook)))
  (add-hook 'sh-mode-hook #'run-bash-mode-hook)

  )

(use-package conf-mode
  :mode "\\.\\(ini\\|conf\\|.*rc\\)\\'" )

(use-package python-mode
  :mode "\\.pythonrc\\'")

(use-package make-mode
  :mode ("\\(makefile\\|\\.mk\\)\\'" . makefile-gmake-mode))

;; -------------------------- Plugin ------------------------------

(use-package json-mode
  :mode "\\.\\(json\\|versionrc\\)\\'")

(use-package lua-mode
  :init
  (setq lua-indent-level 2)
  (setq lua-indent-string-contents t)
  (setq lua-prefix-key nil))


(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

(use-package logview
  :hook (logview-mode-hook . auto-revert-tail-mode))

;; (use-package mermaid-mode)

(use-package eldoc-toml
  :hook (conf-toml-mode-hook . eldoc-toml-mode))

;;; -------------------------- Macos -------------------------------

(use-package applescript-mode
  :init
  ;; TODO add `indent-line-function'
  (setq as-indent-offset 2))

(provide 'init-lang)
;;; init-lang.el ends here

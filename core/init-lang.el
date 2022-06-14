;;; init-lang.el --- language setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------- Builtin ------------------------------

(leaf sh-script
  :init
  (setq sh-basic-offset 2
        sh-shell-file (executable-find "bash"))

  ;; define `bash-mode-hook'
  (defvar bash-mode-hook nil "Hook run after enter sh-mode[Bash]")
  (defun run-bash-mode-hook (&rest _)
    "Run `bash-mode-hook' when enabled."
    (when (eq sh-shell 'bash)
      (run-hooks 'bash-mode-hook)))
  (add-hook 'sh-mode-hook #'run-bash-mode-hook)

  )

(leaf conf-mode
  :mode "\\.\\(ini\\|conf\\|.*rc\\)\\'" )

(leaf python-mode
  :mode "\\.pythonrc\\'")

(leaf make-mode
  :mode ("\\(makefile\\|\\.mk\\)\\'" . makefile-gmake-mode))

;; (leaf cperl-mode
;;   :init
;;   (defalias 'perl-mode 'cperl-mode)
;;   )

;; -------------------------- Plugin ------------------------------

(leaf jsonian
  :mode "\\.\\(jsonc\\|versionrc\\)\\'"
  :init
  (setq jsonian-spaces-per-indentation 2)
  :defer-config
  (with-eval-after-load 'flycheck
    (jsonian-enable-flycheck))
  (with-eval-after-load 'so-long
    (jsonian-no-so-long-mode))
  )

(leaf yaml-mode
  :mode "\\.yamllint\\'"
  :init
  (setq yaml-imenu-generic-expression '((nil  "^\\(:?[0-9a-zA-Z_-]+\\):" 1))))

(leaf lua-mode
  :init
  (setq lua-indent-level 2)
  (setq lua-indent-string-contents t)
  (setq lua-prefix-key nil))


(leaf vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

(leaf mermaid-mode)

(leaf eldoc-toml
  :hook (conf-toml-mode-hook . eldoc-toml-mode))

;; -------------------------- Macos -------------------------------

(leaf applescript-mode
  :init
  ;; TODO add `indent-line-function'
  (setq as-indent-offset 2))

(provide 'init-lang)
;;; init-lang.el ends here

;;; init-lang.el --- language setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------- Builtin ------------------------------

(leaf sh-script
  :init
  (setq sh-basic-offset 2
        sh-shell-file (executable-find "bash")))

(leaf conf-mode
  :mode "\\.\\(ini\\|conf\\|.*rc\\)\\'" "enchant.ordering")

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
  (setq jsonian-default-indentation 2)
  :defer-config
  (with-eval-after-load 'flycheck
    (jsonian-enable-flycheck))
  (with-eval-after-load 'so-long
    (jsonian-no-so-long-mode)))

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

(provide 'init-lang)
;;; init-lang.el ends here

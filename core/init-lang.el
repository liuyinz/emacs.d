;;; init-lang.el --- language setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------ Tree-sitter ----------------------------

(leaf treesit)

(leaf treesit-auto
  :require t
  :config
  (setq treesit-auto-install t)
  (setq treesit-auto-langs
        '(bash c c-sharp clojure cmake commonlisp cpp css dockerfile
               elixir go gomod html javascript java json julia kotlin
               heex python ruby rust toml tsx typescript yaml lua
               ;; make perl markdown
               ))

  (setq treesit-extra-load-path `(,(concat my/dir-cache "etc/tree-sitter")))
  ;; HACK Set lib dir to extra path
  (defun ad/setup-treesit-path (fn)
    "Install treesit lib to extra path when call install function FN."
    (let ((treesit--install-language-grammar-out-dir-history
           (or treesit-extra-load-path nil)))
      (funcall fn)))
  (advice-add 'treesit-auto-install-all :around #'ad/setup-treesit-path)

  (global-treesit-auto-mode))

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

(leaf yaml-ts-mode :mode "\\.\\(yamllint\\|yml\\)\\'")

;; -------------------------- Plugin ------------------------------


(leaf lua-ts-mode
  :mode "\\.lua\\'"
  :init
  (setq lua-ts-mode-indent-offset 2))

(leaf vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

(leaf mermaid-mode)

(leaf eldoc-toml
  :hook (conf-toml-mode-hook . eldoc-toml-mode))

(provide 'init-lang)
;;; init-lang.el ends here

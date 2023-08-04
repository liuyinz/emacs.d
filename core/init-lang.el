;;; init-lang.el --- language setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------ Tree-sitter ----------------------------

(setq treesit-extra-load-path `(,(concat my/dir-cache "etc/tree-sitter")))

(leaf treesit
  :init
  ;; Use the full theming potential of treesit
  (setq treesit-font-lock-level 4)

  ;; ;; tweak the new funcall face
  ;; (custom-theme-set-faces
  ;;  ;; for current theme
  ;;  (or (car custom-enabled-themes) 'user)
  ;;
  ;;  ;; funcall face
  ;;  `(font-lock-function-call-face
  ;;    ((t :inherit font-lock-function-name-face
  ;;        :foreground "hot pink"
  ;;        :background "black"))))
  )

(leaf treesit-auto
  :hook (after-init-hook . global-treesit-auto-mode)
  :init
  (setq treesit-auto-install nil)
  (setq treesit-auto-langs
        '(bash c c-sharp clojure cmake commonlisp cpp css dockerfile
               ;; make perl markdown
               elixir go gomod html javascript java json julia kotlin
               heex python ruby rust toml tsx typescript yaml lua))

  (defun treesit-auto-install-missing ()
    "Install missing grammar in `treesit-auto-langs'."
    (interactive)
    (when-let* ((missing (cl-remove-if (lambda (lang) (treesit-ready-p lang t))
                                       treesit-auto-langs))
                (treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))
      (message "The following tree-sitter grammars are/were missing: %s"
               (mapconcat 'symbol-name missing ", "))
      (mapc (lambda (lang)
              (treesit-install-language-grammar lang (car treesit-extra-load-path)))
            missing)))
  )

;; ------------------------- Builtin ------------------------------

(leaf sh-script
  :init
  (setq sh-basic-offset 2
        sh-shell-file (executable-find "bash")))

(leaf conf-mode
  :mode "\\.\\(ini\\|conf\\|.*rc\\)\\'" "enchant.ordering")

(leaf python
  :mode "\\.pythonrc\\'")

(leaf make-mode
  :mode ("\\(makefile\\|\\.mk\\)\\'" . makefile-gmake-mode))

;; (leaf cperl-mode
;;   :init
;;   (defalias 'perl-mode 'cperl-mode)
;;   )

(leaf yaml-ts-mode :mode "\\.\\(yamllint\\|yml\\)\\'")

(leaf nxml-mode
  :hook (nxml-mode-hook . nxml-mode-setup)
  :init
  (defun nxml-mode-setup ()
    (setq-local fill-column 150)))

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

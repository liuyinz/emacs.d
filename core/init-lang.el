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
               elixir go gomod html javascript java json julia kotlin
               heex python ruby rust toml tsx typescript yaml lua
               ;; third-party
               jq typst mermaid))

  (defun treesit-auto-install-missing (&optional all)
    "Install missing grammar in `treesit-auto-langs'.
If optional arg ALL if non-nil, reinstall all grammars."
    (interactive "P")
    (if-let* ((to-install
               (or (and all treesit-auto-langs)
                   (cl-remove-if (lambda (lang) (treesit-ready-p lang t))
                                 treesit-auto-langs)))
              (treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))
        (progn
          (if all
              (message "Reinstall all tree-siiter grammars in treesit-auto-langs.")
            (message "The following tree-sitter grammars are/were missing: %s"
                     (mapconcat 'symbol-name to-install ", ")))
          (mapc (lambda (lang)
                  (treesit--install-language-grammar-1
                   (car treesit-extra-load-path) lang
                   (car (alist-get lang treesit-language-source-alist))))
                to-install)
          (treesit-auto-add-to-auto-mode-alist))
      (message "All grammars are installed already.")))
  :defer-config
  (prependq! treesit-auto-recipe-list
             `(,(make-treesit-auto-recipe
                 :lang 'jq
                 :ts-mode 'jq-ts-mode
                 :remap 'jq-mode
                 :url "https://github.com/nverno/tree-sitter-jq"
                 :ext "\\.jq\\'")
               ,(make-treesit-auto-recipe
                 :lang 'typst
                 :ts-mode 'typst-ts-mode
                 :remap 'typst-mode
                 :url "https://github.com/uben0/tree-sitter-typst"
                 :ext "\\.typ\\'")
               ,(make-treesit-auto-recipe
                 :lang 'mermaid
                 :ts-mode 'mermaid-ts-mode
                 :remap 'mermaid-mode
                 :url "https://github.com/monaqa/tree-sitter-mermaid"
                 :ext "\\.\\(mmd\\|mermaid\\)")))
  (treesit-auto-add-to-auto-mode-alist))

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

(leaf yaml-ts-mode
  :mode "\\.\\(yamllint\\|clang-format\\)\\'")

(leaf json-ts-mode :mode "\\.\\(eslintrc\\(\\.json\\)?\\)\\'")

(leaf rust-ts-mode)

(leaf toml-ts-mode)

(leaf nxml-mode
  :hook (nxml-mode-hook . nxml-mode-setup)
  :init
  (defun nxml-mode-setup ()
    (setq-local fill-column 150)))

;; -------------------------- Plugin ------------------------------

(leaf jq-ts-mode)

(leaf lua-ts-mode
  :init
  (setq lua-ts-mode-indent-offset 2))

(leaf vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

(leaf mermaid-ts-mode)

(leaf typst-ts-mode
  :init
  (setq typst-ts-mode-watch-options "--open"))

(leaf tera-mode :mode "\\.tera\\'")

(provide 'init-lang)
;;; init-lang.el ends here

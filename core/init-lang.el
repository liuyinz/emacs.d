;;; init-lang.el --- language setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------- Builtin ------------------------------

(leaf sh-script
  :init
  (setq sh-basic-offset 2
        sh-shell-file (executable-find "bash")))

(leaf conf-mode
  :mode ("\\.\\(ini\\|conf\\)\\'" "enchant.ordering"
         (("\\.tidyrc\\'") . conf-colon-mode)))

(leaf python
  :bind
  (:inferior-python-mode-map
   ("C-l" . comint-clear-buffer))
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

(leaf lua-ts-mode
  :init
  (setq lua-ts-mode-indent-offset 2))

(leaf c-ts-mode
  :init
  (setq c-ts-mode-indent-offset 2))

;; -------------------------- Plugin ------------------------------

;; (leaf swift-ts-mode :mode "\\.swift\\'")

;; (leaf jq-ts-mode)

;; (leaf mermaid-ts-mode :mode "\\.\\(mmd\\|mermaid\\)")

;; (leaf zig-ts-mode :mode "\\.zig\\'")

(leaf typst-ts-mode
  :init
  (setq typst-ts-mode-watch-options "--open"))

(leaf tera-mode :mode "\\.tera\\'")

(leaf moonbit-mode :mode "\\.mbt\\'")

(leaf csv-mode
  ;; :hook (csv-mode-hook . csv-mode-setup)
  :init
  ;; ;; BUG goto-address match comma wrongly in csv-mode
  ;; SEE https://www.reddit.com/r/emacs/comments/18tn9vp/help_how_to_exculde_comma_in_gotoaddressmode_url/
  ;; (defun csv-mode-setup ()
  ;;   (setq-local goto-address-url-regexp
  ;;               (concat "\\<"
  ;;                       (regexp-opt goto-address-uri-schemes t)
  ;;                       (string-replace "," "" thing-at-point-url-path-regexp))))
  )

(provide 'init-lang)
;;; init-lang.el ends here

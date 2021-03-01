;;; init-lang.el --- language setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf js-mode
  :defun (flycheck-select-checker . flycheck)
  :interpreter ("node" . js2-mode)
  :config
  (leaf nodejs-repl :require t)
  (when (executable-find "eslint")
    (flycheck-select-checker 'javascript-eslint)))

(leaf typescript-mode :mode "\\.ts[x]\\'")

(leaf css-mode
  :init
  (setq css-indent-offset 2)
  (setq css-fontify-colors nil))

;; ;; SCSS mode
;; (leaf scss-mode
;;   :init (setq scss-compile-at-save nil))

;; (leaf less-css-mode
;;   :init (setq less-css-compile-at-save nil))

(leaf sh-script
  :hook (sh-mode-hook . my-sh-mode-setup)
  :config
  (defun my-sh-mode-setup ()
    (when (and (string-match "\\.sh$" buffer-file-name)
               (executable-find "shellcheck"))
      (flycheck-select-checker 'sh-shellcheck))))

(leaf lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (setq lua-indent-level 2)
  (setq lua-indent-string-contents t)
  (setq lua-prefix-key nil))

(leaf yaml-mode :mode "\\.yaml\\'" "\\.yml\\'")

(leaf plantuml-mode)
(leaf vimrc-mode)

(provide 'init-lang)
;;; init-lang.el ends here

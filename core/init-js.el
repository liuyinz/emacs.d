;;; init-js.el --- js setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; JavaScript
(leaf js2-mode
  :defun (flycheck-select-checker . flycheck)
  :mode ("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode)
  :interpreter ("node" . js2-mode)
                ("node" . js2-jsx-mode)
  :hook (js2-mode-hook . my-js2-mode-setup)
  :init
  (setq js-indent-level 2)
  (setq js2-mode-show-strict-warnings nil)
  :config
  (defun my-js2-mode-setup ()
    (js2-imenu-extras-mode)
    (js2-highlight-unused-variables-mode)
    ;; load repl
    (require 'nodejs-repl)
    (when (executable-find "eslint")
      (flycheck-select-checker 'javascript-eslint))))

(leaf nodejs-repl
  :bind (:js2-mode-map
         ("C-x C-e" . nodejs-repl-send-last-expression)
         ("C-c C-l" . nodejs-repl-send-line)
         ("C-c C-r" . nodejs-repl-send-region)
         ("C-c C-f" . nodejs-repl-load-file)))

(leaf js2-refactor
  :doc "deps: multiple-cursors"
  :hook (js2-mode-hook . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

(leaf typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))

;; Run Mocha or Jasmine tests
;; (use-package mocha
;;   :config (use-package mocha-snippets))

;; Major mode for CoffeeScript code
;; (use-package coffee-mode
;;   :config (setq coffee-tab-width 2))

(provide 'init-js)
;;; init-js.el ends here

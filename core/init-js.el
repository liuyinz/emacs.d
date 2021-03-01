;;; init-js.el --- js setting -*- lexical-binding: t no-byte-compile: t -*-
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
  :commands (nodejs-repl-send-last-expression
             nodejs-repl-send-line
             nodejs-repl-send-region
             nodejs-repl-load-file))

(leaf typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))

(provide 'init-js)
;;; init-js.el ends here

(declare-function flycheck-select-checker 'flycheck)

;; JavaScript
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook (js2-mode . my-js2-mode-setup)
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

(use-package nodejs-repl
  :bind (:map js2-mode-map
         ("C-x C-e" . nodejs-repl-send-last-expression)
         ("C-c C-l" . nodejs-repl-send-line)
         ("C-c C-r" . nodejs-repl-send-region)
         ("C-c C-f" . nodejs-repl-load-file)))

(use-package js2-refactor
  :blackout
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))

;; Run Mocha or Jasmine tests
(use-package mocha
  :config (use-package mocha-snippets))

;; Major mode for CoffeeScript code
(use-package coffee-mode
  :config (setq coffee-tab-width 2))

(provide 'init-js)

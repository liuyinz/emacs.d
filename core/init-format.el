;;; init-format.el --- format setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ;; Format HTML, CSS and JavaScript/JSON
;; ;; Install: npm -g install prettier
(use-package prettier-js
  :straight t
  :commands prettier-js
  ;; :hook ((js-mode js2-mode json-mode web-mode css-mode sgml-mode html-mode)
  ;; . prettier-js-mode)
  :config
  (setq prettier-js-args '( "--printWidth" "80"
                            "--tabWidth" "2"
                            "--useTabs" "false"
                            "--semi" "true"
                            "--singleQuote" "true"
                            "--quoteProps" "as-needed"
                            "--jsxSingleQuote" "false"
                            "--bracketSpacing" "true"
                            "--jsxBracketSameLine" "false"
                            "--requirePragma" "false"
                            "--insertPragma" "false"
                            "--proseWrap" "preserve"
                            "--htmlWhitespaceSensitivity" "ignore"
                            )))

;; format
(use-package format-all
  :straight t
  :commands format-all-buffer)

(defun my-format ()
  "Formating files."
  (interactive)
  (whitespace-cleanup)
  (cond
   ((member major-mode '(web-mode
                         css-mode
                         html-mode
                         js-mode
                         js2-mode
                         json-mode
                         sgml-mode
                         yaml-mode)) (prettier-js))
   ((member major-mode '(elisp-mode)) (elisp-format))
   (t (format-all-buffer))))

(use-package editorconfig
  :straight t
  :delight
  :hook (after-init-hook . editorconfig-mode))

(provide 'init-format)
;;; init-format.el ends here

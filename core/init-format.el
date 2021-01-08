;; ;; Format HTML, CSS and JavaScript/JSON
;; ;; Install: npm -g install prettier
(use-package prettier-js
  :blackout
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
  :commands format-all-buffer)

(defun my-format ()
  "formating files"
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
   (t (format-all-buffer))))

(use-package editorconfig
  :blackout
  :hook (after-init . editorconfig-mode))

(provide 'init-format)

;;; init-eglot.el --- setting for eglot  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:


(leaf eglot
  :hook ((python-mode-hook
          js2-mode-hook
          bash-mode-hook
          go-mode-hook) . eglot-ensure)
  :defer-config
  
  ;; disable initialisation
  (setq eglot-stay-out-of '(flymake))
  
  ;; add taliwindcss server
  ;; (add-to-list 'eglot-server-programs '((web-mode :language-id "html") . ("tailwindcss-language-server")))

  (with-eval-after-load 'consult-imenu
    (appendq! consult-imenu-config
              '((js2-mode :types
                  ((?c "Class"     font-lock-type-face)
                   (?f "Function"  font-lock-function-name-face)
                   (?s "Constant"  font-lock-constant-face)
                   (?m "Method"    font-lock-string-face)
                   (?p "Property"  font-lock-builtin-face)
                   (?v "Variable"  font-lock-variable-name-face)))
                (python-mode :types
                  ((?c "Class"     font-lock-type-face)
                   (?f "Function"  font-lock-function-name-face)
                   (?v "Variable"  font-lock-variable-name-face)))
                (sh-mode :types
                  ((?f "Function"  font-lock-function-name-face)
                   (?v "Variable"  font-lock-variable-name-face)))
                )))
  )

(provide 'init-eglot)

;;; init-eglot.el ends here

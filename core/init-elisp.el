;;; init-elisp.el --- Emacs lisp settings -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf elisp-mode
  :hook (emacs-lisp-mode-hook . elisp-setup)
  :init
  (defun elisp-setup ()
    (setq-local imenu-generic-expression
                (append imenu-generic-expression
                        `(("Customs" ,(concat "^\\s-*(defcustom\\s-+\\("
                                              lisp-mode-symbol-regexp "\\)") 1)
                          ("Faces" ,(concat "^\\s-*(defface\\s-+\\("
                                            lisp-mode-symbol-regexp "\\)") 1)
                          ("Commands" ,(concat "^\\s-*(defun\\s-+\\("
                                               lisp-mode-symbol-regexp
                                               "\\)\\(.*\n\\)+?\\s-*(interactive[) ].*$") 1)
                          ("Leafs" ,(concat "^\\s-*(leaf\\s-+\\("
                                            lisp-mode-symbol-regexp "\\)") 1))))))

(leaf simple
  :init
  (setq eval-expression-print-length nil
        eval-expression-print-level nil))

(leaf pp
  :init
  (setq pp-default-function 'pp-28)

  (defun pp-macroexpand-all-expression (expression)
    "Macroexpand EXPRESSION all level and pretty-print its value."
    (interactive
     (list (read--expression "Macroexpand: ")))
    (pp-display-expression (macroexpand-all expression) "*Pp Macroexpand Output*"
                           pp-use-max-width)))

(leaf macroexpand
  :init
  (setq macrostep-expand-in-separate-buffer t
        macrostep-expand-compiler-macros nil))

;; SEE https://emacs-china.org/t/2-3-4/11875/5?u=cheunghsu
(leaf lisp-keyword-indent
  :hook (after-init-hook . lisp-keyword-indent-mode)
  :init (setq lisp-indent-function 'lisp-indent-function))

;; add extra font-lock for elisp
(leaf elispfl
  :after elisp-mode
  :config
  (elispfl-mode)
  (elispfl-ielm-mode))

(leaf democratize
  :init
  (advice-add 'helpful-update
              :after
              #'democratize-insert-examples-into-helpful-buffer)
  :defer-config
  (mapc #'democratize-library (democratize--list-non-democratized-libraries)))

(leaf helpful
  :init
  (setq helpful-switch-buffer-function #'display-buffer)
  :bind
  ([remap describe-key]      . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-symbol]   . helpful-symbol))

(leaf dash
  :hook (after-init-hook . global-dash-fontify-mode)
  :init
  (with-eval-after-load 'info-look
    (dash-register-info-lookup)))

(leaf info-colors
  :hook (Info-selection-hook . info-colors-fontify-node))

(leaf macrostep
  :init
  (setq macrostep-expand-in-separate-buffer t))

(leaf psearch)

(provide 'init-elisp)
;;; init-elisp.el ends here

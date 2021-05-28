;;; init-minibuffer.el --- minibuffer sets -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; @https://github.com/purcell/emacs.d/blob/master/lisp/init-minibuffer.el
(leaf vertico
  :hook (after-init-hook . vertico-mode)
  :init
  (setq vertico-cycle t
        vertico-count 15))

(leaf marginalia
  :hook (vertico-mode-hook . marginalia-mode)
  :config
  (setq-default marginalia-annotators '(marginalia-annotators-heavy nil)))

;; filtering
(leaf orderless
  :require t
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))

  (setq orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-strict-initialism)
        orderless-component-separator #'orderless-escapable-split-on-space
        orderless-style-dispatchers '(initialism-if-at without-if-bang))

  ;; @https://github.com/oantolin/orderless/blob/master/README.org#style-dispatchers
  ;; dispatchers
  (defun without-if-bang (pattern _index _total)
    "!pattern : exclude pattern."
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (defun initialism-if-at (pattern _index _total)
    "@pattern : first letter of word in order."
    (cond
     ((equal "@" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "@" pattern)
      `(orderless-initialism . ,(substring pattern 1))))))

(leaf consult
  :require t
  :init
  (setq consult-async-min-input 1)
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-find-command "fd --color=never --full-path ARG OPTS")

  ;; disable preview
  (setq consult-preview-key nil)
  ;; Optionally configure the register formatting.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (require 'consult-imenu)
  (require 'consult-compile)
  (require 'consult-register)

  (leaf consult-flycheck :require t))

(leaf embark
  :after consult
  :require t
  ;; :init (setq embark-prompter 'embark-completing-read-prompter)
  :config
  (leaf embark-consult :require t)

  ;; HACK Open source code of `symbol' in other window
  (advice-add 'embark-find-definition :before #'open-in-other-window))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here

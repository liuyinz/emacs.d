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
  :after vertico
  :init
  (setq completion-styles '(basic partial-completion orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))

  (setq orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-strict-initialism))

  :config

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
      `(orderless-initialism . ,(substring pattern 1)))))

  (setq orderless-component-separator #'orderless-escapable-split-on-space
        orderless-style-dispatchers '(initialism-if-at without-if-bang))

  ;; @https://github.com/oantolin/orderless#company
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))
  (advice-add 'company-capf--candidates :around #'just-one-face)

  ;; ;; HACK  don't use orderless in company
  ;; ;; @https://www.reddit.com/r/emacs/comments/nichkl/how_to_use_different_completion_styles_in_the/
  ;; (defun fix-company-completion-style ()
  ;;   "Set different style in company"
  ;;   (if (member 'basic completion-styles)
  ;;       (setq-local completion-styles '(orderless))
  ;;     (setq-local completion-styles '(basic partial-completion orderless))))
  ;; (add-hook 'company-completion-started-hook #'fix-company-completion-style)
  ;; (add-hook 'company-after-completion-hook #'fix-company-completion-style)
  )

(leaf consult
  :require t
  :after vertico
  :init
  (setq consult-async-min-input 1)
  (setq consult-find-command "fd --color=never --full-path ARG OPTS")
  (setq consult-project-root-function #'projectile-project-root)

  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<")

  :config
  (require 'consult-imenu)
  (require 'consult-xref)
  (require 'consult-compile)
  (require 'consult-register)

  (with-eval-after-load 'flycheck
    (leaf consult-flycheck :require t))

  (when (bound-and-true-p vertico-mode)
    (setq completion-in-region-function #'consult-completion-in-region))

  ;; ;; add morek buffer to hidden source
  (appendq! consult-buffer-filter '("\\`\\*.*\\*\\'"
                                    "\\`.*\\.el\\.gz\\'"
                                    "\\`magit[:-].*\\'"
                                    "\\`COMMIT_EDITMSG\\'"))
  ;; ;; disable preview
  ;; (setq consult-preview-key nil)

  ;; Optionally configure the register formatting.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; preview setting
  (consult-customize
   consult-recent-file consult-bookmark consult--source-file
   consult--source-project-file consult--source-bookmark
   consult-ripgrep consult-git-grep consult-grep
   :preview-key nil
   ;; consult-theme :preview-key 'any
   ))

(leaf embark
  :commands embark-act embark-become
  ;; :init (setq embark-prompter 'embark-completing-read-prompter)
  :config
  (leaf embark-consult :require t)

  ;; HACK Open source code of `symbol' in other window
  (advice-add 'embark-find-definition :before #'open-in-other-window))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here

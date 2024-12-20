;;; init-embark.el --- embark setup -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-06-12 20:33:40

;;; Commentary:

;;; Code:

(leaf embark
  :after vertico
  :init
  (setq embark-prompter 'embark-keymap-prompter
        embark-keymap-prompter-key ","
        embark-mixed-indicator-delay 0.3)
  (setq embark-verbose-indicator-display-action '(display-buffer-reuse-window))
  (setq embark-indicators '(embark-mixed-indicator embark-highlight-indicator))
  :bind
  ("M-l" . embark-act)
  (:embark-general-map
   ("C-c C-a" . marginalia-cycle))
  (:vertico-map
   ("C-l" . embark-act)
   ("C-c C-o" . embark-export)
   ("C-c C-a" . marginalia-cycle))
  (:embark-expression-map
   ("M" . pp-macroexpand-all-expression))
  (:embark-library-map
   ("o" . find-library-other-window))
  (:embark-identifier-map
   ("d" . lsp-bridge-find-def-other-window))
  :defer-config
  ;; HACK Open source code of `symbol' in other window
  (dolist (cmd '(embark-find-definition))
    (advice-add cmd :before #'open-in-other-window))
  )

(leaf embark-consult
  :after embark consult)

(provide 'init-embark)
;;; init-embark.el ends here

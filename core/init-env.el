;;; init-env.el --- Environment setup -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2023-11-07 15:37:39

;;; Commentary:

;;; Code:

;; (leaf envrc
;;   :mode ("\\.envrc\\'" . envrc-file-mode)
;;   :hook (after-init-hook . envrc-global-mode)
;;   :init
;;   ;; write command to add envrc for node
;;   ;; for npm/pnpm project
;;   ;; create or add path
;;   ;; allow envrc
;;   )

(leaf mise
  :hook (after-init-hook . global-mise-mode)
  :init
  (setq mise-debug t))

(provide 'init-env)
;;; init-env.el ends here

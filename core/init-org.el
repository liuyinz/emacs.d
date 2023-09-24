;;; init-org.el --- summary -*- lexical-binding: t no-byte-compile: t -*-

;; Author: 食無魚
;; Created: 2021-07-31 17:39:06

;;; Commentary:

;;; Code:

(leaf org
  :init
  (setq org-edit-src-content-indentation 0)
  :defer-config
  (keymap-global-set "C-c l" 'org-store-link)
  (keymap-global-set "C-c a" 'org-agenda)
  (keymap-global-set "C-c c" 'org-capture))

(provide 'init-org)
;;; init-org.el ends here

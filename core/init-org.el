;;; init-org.el --- summary -*- lexical-binding: t no-byte-compile: t -*-

;; Author: 食無魚
;; Created: 2021-07-31 17:39:06

;;; Commentary:

;;; Code:

(use-package org
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))

(provide 'init-org)
;;; init-org.el ends here

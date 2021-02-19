;;; init-org.el --- setting for org-mode -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf org
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))

(provide 'init-org)
;;; init-org.el ends here

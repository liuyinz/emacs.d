;;; init-rebound.el --- Fix minor mode comflicts -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-18 20:49:08

;;; Commentary:

;;; Code:

(leaf rebound-mode
  :hook (after-init-hook . rebound-mode)
  :init
  (setq rebound-alist
        '((writerom-mode  . ((diff-hl-margin-local-mode -1)))
          (redacted-mode  . ((topsy-mode -1)
                             (binky-margin-local-mode -1)
                             (diff-hl-margin-local-mode -1)
                             (hide-mode-line-mode 1)
                             (hl-line-mode -1)
                             (global-hl-line-mode -1)
                             (blink-cursor-mode -1)
                             (read-only-mode 1 buffer-read-only)))
          (smerge-mode    . ((diff-hl-mode -1)
                             (hl-line-mode -1)
                             (save-place-local-mode -1)
                             (highlight-parentheses-mode -1)))))
  )

(provide 'init-rebound)
;;; init-rebound.el ends here

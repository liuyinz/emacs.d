;;; init-rebound.el --- Fix minor mode comflicts -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-18 20:49:08

;;; Commentary:

;;; Code:

(leaf rebound-mode
  :hook (after-init-hook . rebound-mode)
  :defer-config
  (setq rebound-common-alist
        '(;; built-in
          (hl-line   . ((hl-line-mode -1)
                        (global-hl-line-mode -1)))
          (read-only . ((read-only-mode 1 buffer-read-only)))
          (cursor    . ((blink-cursor-mode -1)))
          ;; third-party
          (topsy     . ((topsy-mode -1)))
          (mode-line . ((hide-mode-line-mode 1)))
          (margin    . ((diff-hl-margin-local-mode -1)
                        (binky-margin-local-mode -1)))
          (paren     . ((highlight-parentheses-mode -1)))))

  (setq rebound-alist
        '((olivetti-mode  . (margin mode-line topsy (focus-mode -1)))
          (redacted-mode  . (margin hl-line topsy cursor mode-line paren read-only))
          (smerge-mode    . (margin paren (save-place-local-mode -1 save-place-mode))))))

(provide 'init-rebound)
;;; init-rebound.el ends here

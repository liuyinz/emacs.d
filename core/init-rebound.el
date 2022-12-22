;;; init-rebound.el --- Fix minor mode comflicts -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-18 20:49:08

;;; Commentary:

;;; Code:

(leaf rebound-mode
  :hook (after-init-hook . rebound-mode)
  :defer-config
  (setq rebound-common-alist
        '((margin    . ((diff-hl-margin-local-mode -1)
                        (binky-margin-local-mode -1)))
          (line      . ((hl-line-mode -1)
                        (global-hl-line-mode -1)
                        (topsy-mode -1)))
          (mode-line . ((hide-mode-line-mode 1)))
          (cursor    . ((blink-cursor-mode -1)))
          (paren     . ((highlight-parentheses-mode -1)))))

  (setq rebound-alist
        `((writeroom-mode . ((binky-margin-local-mode -1)
                             (diff-hl-margin-mode -1)))
          (redacted-mode  . (,@(rebound-extract '(margin line cursor mode-line))
                             (read-only-mode 1 buffer-read-only)))
          (smerge-mode    . (,@(rebound-extract '(margin paren))
                             (save-place-local-mode -1))))))

(provide 'init-rebound)
;;; init-rebound.el ends here

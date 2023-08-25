;;; init-rebounce.el --- rebounce setup -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-18 20:49:08

;;; Commentary:

;;; Code:

(leaf rebounce
  :hook (after-init-hook . rebounce-mode)
  :defer-config
  (setq rebounce-quiet t)
  (setq rebounce-groups
        '(;; built-in
          (hl-line-no . ((hl-line-mode -1)
                         (global-hl-line-mode -1)))
          (read-only  . ((read-only-mode 1)))
          (cursor-no  . ((blink-cursor-mode -1)))
          ;; third-party
          (topsy-no   . ((topsy-mode -1)))
          (mode-line  . ((hide-mode-line-mode 1)))
          (margin-no  . ((diff-hl-margin-local-mode -1)
                         (binky-margin-local-mode -1)))
          (paren-no   . ((highlight-parentheses-mode -1)))))

  (setq rebounce-alist
        '((olivetti-mode . (margin-no kk mode-line hl-line-no topsy-no (focus-mode 1)))
          (redacted-mode . (margin-no hl-line-no topsy-no cursor-no
                                      mode-line paren-no read-only))
          (smerge-mode   . (margin-no paren-no (save-place-local-mode -1))))))

(provide 'init-rebounce)
;;; init-rebounce.el ends here

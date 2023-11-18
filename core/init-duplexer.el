;;; init-duplexer.el --- rebounce setup -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-18 20:49:08

;;; Commentary:

;;; Code:

(leaf duplexer
  :hook (after-init-hook . duplexer-mode)
  :defer-config
  (setq duplexer-quiet t)
  (setq duplexer-groups
        '(;; built-in
          (hl-line-no . ((hl-line-mode -1)
                         (global-hl-line-mode -1)))
          (read-only  . ((read-only-mode 1)))
          (cursor-no  . ((blink-cursor-mode -1)))
          ;; third-party
          (margin-no  . ((diff-hl-margin-local-mode -1)
                         (binky-margin-local-mode -1)))
          (paren-no   . ((highlight-parentheses-mode -1)))))

  (setq duplexer-alist
        '((olivetti-mode . (margin-no kk hl-line-no (focus-mode 1)))
          (redacted-mode . (margin-no hl-line-no cursor-no paren-no read-only))
          (smerge-mode   . (margin-no paren-no (save-place-local-mode -1))))))

(provide 'init-duplexer)
;;; init-duplexer.el ends here

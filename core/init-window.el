;;; init-window.el --- window setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf shackle
  :hook (after-init-hook . shackle-mode)
  :init
  (setq shackle-default-rule nil)
  (setq shackle-rules '(
                        ;; builtin
                        (("*Warnings*" "*Messages*") :size 0.3 :align 'below)
                        (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 :align 'below)
                        ;; third-party
                        ("*vterm*" :align 'below :size 0.4)
                        ("*quickrun*" :select t :size 0.3 :align 'below)
                        (" *Flycheck checkers*" :select t :size 0.3 :align 'below)
                        ((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
                         :select t :size 0.25 :align 'below)
                        )))

(leaf transpose-frame :require t)

(provide 'init-window)
;;; init-window.el ends here

;;; init-window.el --- window setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'display-buffer-alist
             '("^\\*quickrun"
               (display-buffer-reuse-window
	            display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))

(leaf shackle
  :hook (after-init-hook . shackle-mode)
  :init
  (setq shackle-default-rule nil)
  (setq shackle-rules
        ;; see @https://github.com/seagle0128/.emacs.d/blob/320ae719a1acb84c047e94bb6ee3f33e426f7b47/lisp/init-window.el#L204
        '(
          ;; builtin
          (("*Warnings*" "*Messages*") :size 0.3 :align 'below)
          (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 :align 'below)
          ("\\*[Wo]*Man.*\\*" :regexp t :popup t :select t :size 0.5 :align 'below)

          ;; third-party
          ;; ("*evil-marks*" :align 'below :size 0.4)

          ("*vterm*" :align 'below :size 0.4)
          ;; ("*quickrun*" :select t :size 0.4 :align 'below)
          ("*Python*" :select t :size 0.4 :align 'below)
          ("*nodejs*" :select t :size 0.4 :align 'below)

          ("*format-all-errors*" :size 0.3 :align 'below)

          (" *Flycheck checkers*" :select t :size 0.3 :align 'below)
          ((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
           :select t :size 0.25 :align 'below)

          (" *command-log*" :size 0.4 :align 'left)
          )))

(leaf transpose-frame :require t)

(leaf zoom :require t)

(provide 'init-window)
;;; init-window.el ends here

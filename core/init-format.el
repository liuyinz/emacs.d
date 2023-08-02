;;; init-format.el --- format config -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2023-08-01 17:07:24

;;; Commentary:

;;; Code:

;; -------------------------- Format ------------------------------

;; (leaf editorconfig
;;   :hook (after-init-hook . editorconfig-mode))

;; ;; ISSUE https://github.com/lassik/emacs-format-all-the-code/issues/220
;; (leaf format-all
;;   ;; :require t
;;   :init
;;   (setq format-all-debug t)
;;   (advice-add 'format-all-buffer :before #'format-all-ensure-formatter)
;;
;;   (defun my/format ()
;;     "Formatting current buffer."
;;     (interactive)
;;     (cl-case major-mode
;;       (gitconfig-mode (run-general! indent-region indent-whole-buffer))
;;       (t (run-general! format-all-region format-all-buffer))))
;;   :defer-config
;;   (prependq! format-all-default-formatters
;;              '(("JSONC" prettier)
;;                ;; SEE https://google.github.io/styleguide/shellguide.html
;;                ("Shell" (shfmt "-i" "2" "-bn" "-ci"))))
;;   )

(leaf apheleia
  :require t
  :init
  (setq apheleia-hide-log-buffers t)
  (defun my/format ()
    "Formatting current buffer."
    (interactive)
    (cl-case major-mode
      ((gitconfig-mode emacs-lisp-mode lisp-interaction-mode)
       (run-general! indent-region indent-whole-buffer))
      (t (call-interactively #'apheleia-format-buffer))))
  :config
  (alist-set! apheleia-formatters
              '((shfmt . ("shfmt" "-i" "2" "-bn" "-ci"))))
  (alist-set! apheleia-mode-alist
              '((python-mode . (black isort))
                (python-ts-mode . (black isort))
                ((sh-mode bash-ts-mode) . shfmt)
                (markdown-mode . prettier-markdown)
                (gfm-mode . prettier-markdown)
                (ruby-ts-mode . rubocop))))

(provide 'init-format)
;;; init-format.el ends here

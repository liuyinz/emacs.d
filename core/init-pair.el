;;; init-pair.el --- Setup pair editing -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-22 17:28:22

;;; Commentary:

;;; Code:

;; disable show-paren-mode by default
(leaf paren
  :hook (after-init-hook . (lambda () (show-paren-mode -1)))
  :init
  (setq show-paren-style 'parenthesis
        show-paren-context-when-offscreen 'overlay))

(leaf elec-pair
  :hook (after-init-hook . electric-pair-mode))

(leaf elec-pair-extra
  :hook (electric-pair-mode-hook . elec-pair-extra-setup)
  :init
  (setq elec-pair-extra-rules
        '((typescript-ts-mode :pair ((?\< . ?\>))
                              :inhibit ((?\< . " <")))
          ;; (rust-ts-mode       :pair (?')
          ;;                     :inhibit ((?\' . "&'")))
          )))

(leaf isolate
  :require t
  :defer-config
  (add-hook 'isolate-add-mode-hook #'my/meow-motion-temporary))

(provide 'init-pair)
;;; init-pair.el ends here

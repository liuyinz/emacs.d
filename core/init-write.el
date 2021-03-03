;;; init-write.el --- Enjoy Writing!! -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;; commentary

;;; Code:

(leaf markdown-mode
  :doc "deps : edit-indirect"
  :mode
  ("README\\.md\\'" . gfm-mode)
  (("\\.md\\'" "\\.markdown\\'") . markdown-mode)
  :init
  (setq markdown-command "pandoc"))

(leaf writeroom :commands writeroom-mode)

(provide 'init-write)

;;; init-write.el ends here

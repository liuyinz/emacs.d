;;; init-vue.el --- Setup vue configs -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2023-10-05 21:46:58

;;; Commentary:

;;; Code:

;; define a new major
(define-derived-mode web-vue-mode web-mode "wVue"
  "A major mode derived from `web-mode'.
For editing .vue files with LSP support.")

(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-vue-mode))

(provide 'init-vue)
;;; init-vue.el ends here


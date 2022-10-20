;;; init-bridge.el --- Setup for lsp-bridge -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-07-09 08:07:53

;;; Commentary:

;;; Code:

;; REQUIRE pip install epc
(leaf lsp-bridge
  :hook (after-init-hook . global-lsp-bridge-mode)
  :defer-config
  (setq acm-enable-quick-access nil
        acm-enable-tabnine nil
        acm-backend-yas-candidates-number 10
        acm-snippet-insert-index 0
        acm-backend-yas-match-by-trigger-keyword t
        acm-backend-yas-show-trigger-keyword " [%s]")
  (setq lsp-bridge-enable-diagnostics nil)

  ;; ;; Debug: REQUIRE brew install gdb
  ;; (setq lsp-bridge-enable-log t
  ;;       lsp-bridge-enable-debug t)

  ;; Setup multi server
  (appendq! lsp-bridge-multi-lang-server-extension-list
            '((("css" "less" "scss") . "css_emmet")
              (("html") . "html_emmet")))
)

(provide 'init-bridge)
;;; init-bridge.el ends here

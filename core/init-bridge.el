;;; init-bridge.el --- Setup for lsp-bridge -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-07-09 08:07:53

;;; Commentary:

;;; Code:

;; REQUIRE pip install epc orjson six
(leaf lsp-bridge
  :hook (after-init-hook . global-lsp-bridge-mode)
  :defer-config

  ;; ;; Debug: REQUIRE brew install gdb
  ;; (setq lsp-bridge-enable-log t)
  ;; (setq lsp-bridge-enable-debug t)

  (setq lsp-bridge-enable-diagnostics nil
        lsp-bridge-disable-backup nil)

  ;; customize mode-line info
  (advice-add 'lsp-bridge--mode-line-format
              :filter-return
              (lambda (s) (ignore-errors (s-capitalize (substring s 4 10)))))
  (setcdr (assoc 'lsp-bridge-mode mode-line-misc-info)
          '(lsp-bridge--mode-line-format))


  (appendq! lsp-bridge-default-mode-hooks
            '(snippet-mode-hook
              git-commit-mode-hook
              markdown-mode-hook
              mhtml-mode-hook
              html-mode-hook))

  ;; Setup multi server

  (appendq! lsp-bridge-multi-lang-server-extension-list
            '((("css" "less" "scss") . "css_emmet")
              (("html") . "html_emmet")))

  ;; (appendq! lsp-bridge-multi-lang-server-mode-list
  ;;           '(((css-mode less-css-mode scss-mode) . "css_emmet")
  ;;             ((web-mode mhtml-mode html-mode) . "html_emmet")))

  (leaf acm
    :bind
    (:acm-mode-map
     ((kbd "C-t h") . acm-doc-toggle)
     ((kbd "C-t d") . acm-sdcv-toggle)
     )
    :init
    (setq acm-enable-quick-access nil
          acm-enable-tabnine nil
          acm-enable-doc nil)

    ;; yasnippet
    (setq acm-snippet-insert-index 1
          acm-backend-yas-candidates-number 3
          acm-backend-yas-match-by-trigger-keyword t
          acm-backend-yas-show-trigger-keyword " [%s]")

    ;; (defun acm-insert-common-or-next ()
    ;;   "Insert common prefix of menu or select next candidate."
    ;;   (interactive)
    ;;   (let ((inhibit-message t)
    ;;         (num (length (acm-get-input-prefix))))
    ;;     (acm-insert-common)
    ;;     (when (= num (length (acm-get-input-prefix)))
    ;;       (acm-select-next))))
    ;; (put 'acm-insert-common-or-next 'completion-predicate #'ignore))

    ;; FIXME wrapper of `lsp-bridge-toggle-sdcv-helper'
    (defun acm-sdcv-toggle ()
      "docstring"
      (interactive)
      (let ((inhibit-message t))
        (lsp-bridge-toggle-sdcv-helper)
        (acm-update)
        (acm-menu-update)))
    :config

    ;; BUG use in terminal
    ;; (unless (display-graphic-p)
    ;;   (require 'acm-terminal))

    )
  )

(provide 'init-bridge)
;;; init-bridge.el ends here

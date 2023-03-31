;;; init-bridge.el --- Setup for lsp-bridge -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-07-09 08:07:53

;;; Commentary:

;;; Code:

;; REQUIRE pip install epc orjson six
(leaf lsp-bridge
  :hook (after-init-hook . global-lsp-bridge-mode)
  :init
  ;; HACK
  (defun ah/lsp-bridge-reset-hide-characters ()
    "Allow char : in buffer which enable emmet-ls completions."
    (when (ignore-errors (string-match-p "emmet" (lsp-bridge-has-lsp-server-p)))
      (setq-local lsp-bridge-completion-hide-characters
                  (delete ":" lsp-bridge-completion-hide-characters))))
  (add-hook 'lsp-bridge-mode-hook #'ah/lsp-bridge-reset-hide-characters)

  ;; (defun ad/lsp-bridge-not-match-hide-characters ()
  ;;   (let ((char (ignore-errors (char-to-string (char-before)))))
  ;;     (or (and lsp-bridge-completion-obey-trigger-characters-p
  ;;              (member char (if (boundp 'acm-backend-lsp-completion-trigger-characters)
  ;;                               (symbol-value 'acm-backend-lsp-completion-trigger-characters))))
  ;;         (and (string-equal char ":")
  ;;              (cl-intersection '("emmet-ls" "emmet-ls-css")
  ;;                               acm-backend-lsp-server-names
  ;;                               :test #'equal))
  ;;         (not (member char lsp-bridge-completion-hide-characters)))))
  ;; (advice-add 'lsp-bridge-not-match-hide-characters :override #'ad/lsp-bridge-not-match-hide-characters)

  :defer-config

  ;; ;; Debug: REQUIRE brew install gdb
  ;; (setq lsp-bridge-enable-log t)
  ;; (setq lsp-bridge-enable-debug t)

  (setq lsp-bridge-python-command "python3.10")

  (setq lsp-bridge-enable-diagnostics nil
        lsp-bridge-disable-backup nil)

  (appendq! lsp-bridge-default-mode-hooks
            '(snippet-mode-hook
              git-commit-mode-hook
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
    (setq acm-completion-backend-merge-order
          '("template-first-part-candidates"
            "mode-first-part-candidates"
            ;; "tabnine-candidates"
            "template-second-part-candidates"
            "mode-second-part-candidates"))
    
    (setq acm-backend-yas-candidates-number 3
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

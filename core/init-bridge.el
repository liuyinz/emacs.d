;;; init-bridge.el --- Setup for lsp-bridge -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-07-09 08:07:53

;;; Commentary:

;;; Code:

(leaf lsp-bridge-ref
  :hook (lsp-bridge-ref-mode-hook . (lambda () (meow-mode -1))))

(leaf lsp-bridge
  :hook (after-init-hook . global-lsp-bridge-mode)
  :bind
  ("M-d" . lsp-bridge-doc-toggle)
  :init
  ;; Setup language
  ;; REQUIRE pthon3.11 -m pip install epc orjson sexpdata six paramiko ruff-lsp
  (setq lsp-bridge-python-command "python3.11")
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")

  (setq lsp-bridge-enable-inlay-hint t)

  ;; (setq lsp-bridge-completion-popup-predicates nil)
  ;; ;; Debug: REQUIRE brew install gdb
  ;; (setq lsp-bridge-enable-log t)
  ;; (setq lsp-bridge-enable-debug t)

  (defun my/bridge-server-setup ()
    (with-current-buffer (current-buffer)
      (when (bound-and-true-p acm-backend-lsp-server-names)
        (let ((servers acm-backend-lsp-server-names))
          ;; enable :,], in emmet completion
          ;; FIXME try to separate emmet
          (when (member "emmet-ls" servers)
            (setq-local lsp-bridge-completion-hide-characters
                        (seq-difference lsp-bridge-completion-hide-characters
                                        '(":" "(" ")" "[" "]" "{" "}" "\""))))
          ;; enable - in tailwindcss completion
          (when (member "tailwindcss" servers)
            (modify-syntax-entry ?- "w"))))))

  (add-hook 'lsp-bridge-mode-hook
            (lambda ()
              (run-with-timer 3 nil #'my/bridge-server-setup)))

  (defun lsp-bridge-doc-toggle ()
    (interactive)
    (if (acm-frame-visible-p lsp-bridge-popup-documentation-frame)
        (acm-hide)
      (lsp-bridge-popup-documentation)))

  :defer-config
  (my/load-features 'init-bridge-detect)

  ;; Setup server
  (setq lsp-bridge-user-langserver-dir
        (expand-file-name "lsp-bridge/single" my/dir-ext))
  (setq lsp-bridge-user-multiserver-dir
        (expand-file-name "lsp-bridge/multi" my/dir-ext))

  (setq lsp-bridge-enable-diagnostics t
        lsp-bridge-disable-backup nil)
  (setq lsp-bridge-enable-completion-in-string t)
  (appendq! lsp-bridge-default-mode-hooks
            '(snippet-mode-hook
              git-commit-mode-hook
              mhtml-mode-hook
              html-mode-hook
              js-json-mode-hook
              vue-ts-mode-hook
              astro-mode-hook
              jtsx-jsx-mode-hook
              jtsx-tsx-mode-hook
              jtsx-typescript-mode-hook))

  (leaf acm
    :bind
    (:acm-mode-map
     ("C-j" . nil)
     ("C-k" . nil))
    :init
    (setq acm-enable-quick-access nil
          acm-enable-tabnine nil
          acm-enable-doc nil)
    (setq acm-candidate-match-function 'orderless-flex)
    ;; yasnippet
    (setq acm-completion-backend-merge-order
          '("template-first-part-candidates"
            "template-second-part-candidates"
            "mode-first-part-candidates"
            "mode-second-part-candidates"))
    (setq acm-backend-yas-candidates-number 3
          acm-backend-yas-match-by-trigger-keyword t
          acm-backend-yas-show-trigger-keyword " [%s]")

    :defer-config
    ;; enable acm cycle style
    (defun ad/acm-select-prev ()
      (interactive)
      (acm-menu-update
       (cond ((> acm-menu-index 0)
              (setq-local acm-menu-index (1- acm-menu-index)))
             ((> acm-menu-offset 0)
              (setq-local acm-menu-offset (1- acm-menu-offset)))
             (t (call-interactively #'acm-select-last)))))
    (advice-add 'acm-select-prev :override #'ad/acm-select-prev)

    (defun ad/acm-select-next ()
      (interactive)
      (acm-menu-update
       (cond ((< acm-menu-index (1- (length acm-menu-candidates)))
              (setq-local acm-menu-index (1+ acm-menu-index)))
             ((< (+ acm-menu-offset acm-menu-index) (1- (length acm-candidates)))
              (setq-local acm-menu-offset (1+ acm-menu-offset)))
             (t (call-interactively #'acm-select-first)))))
    (advice-add 'acm-select-next :override #'ad/acm-select-next)

    ;; FIXME wrapper of `lsp-bridge-toggle-sdcv-helper'
    (defun acm-sdcv-toggle ()
      "docstring"
      (interactive)
      (let ((inhibit-message t))
        (lsp-bridge-toggle-sdcv-helper)
        (acm-update)
        (acm-menu-update)))

    (defun ad/acm-doc-toggle ()
      (interactive)
      (if (acm-frame-visible-p acm-doc-frame)
          (progn
            (acm-doc-hide)
            (setq acm-enable-doc nil))
        (setq acm-enable-doc t)
        (acm-doc-try-show)))
    (advice-add 'acm-doc-toggle :override #'ad/acm-doc-toggle))
  )

(provide 'init-bridge)
;;; init-bridge.el ends here

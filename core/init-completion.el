;;; init-completion.el --- setting for completion -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

(leaf company
  :hook (after-init-hook . global-company-mode)
  :init
  (setq company-tooltip-width-grow-only t
        company-tooltip-align-annotations t
        company-tooltip-limit 15
        company-format-margin-function #'company-text-icons-margin
        company-idle-delay 0
        company-minimum-prefix-length 2
        company-require-match nil
        company-selection-wrap-around t
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-search-regexp-function #'company-search-words-in-any-order-regexp)
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t)

  (setq company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode))

  (setq company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))

  (setq company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev company-gtags company-etags))

  :config
  (with-no-warnings
    (with-eval-after-load 'yasnippet
      (require 'company-yasnippet)

      (defun my-company-yasnippet ()
        "Hide the current completeions and show snippets."
        (interactive)
        (company-cancel)
        (call-interactively 'company-yasnippet)))))

;; yasnippet
(leaf yasnippet
  :require t
  :hook
  ((prog-mode-hook markdown-mode-hook) . yas-minor-mode)
  (git-commit-mode-hook . (lambda () (yas-activate-extra-mode 'git-commit-mode)))
  :init
  (setq yas-indent-line 'fixed)
  ;; yas-also-indent-empty-lines t
  ;; yas-indent-line 'auto
  ;; yas-also-auto-indent-first-line t
  (setq yas-new-snippet-default "\
# -*- mode: snippet -*-
# name: ${1:name}
# contributor : ${2:`user-full-name`<`user-mail-address`>}
# key: ${3:key}
# --
$0`(yas-escape-text yas-selected-text)`")

  :config
  ;; mode-switch between lisp-interaction-mode and snippet-mode
  (defun my-yasnippet-switch ()
    (interactive)
    (if (equal major-mode 'snippet-mode)
        (lisp-interaction-mode)
      (snippet-mode))
    (evil-insert))

  (yas-reload-all))

(provide 'init-completion)
;;; init-completion.el ends here

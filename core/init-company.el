;;; init-company.el --- company setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

(leaf company
  :blackout t
  :commands company-cancel
  :hook ((prog-mode-hook conf-mode-hook eshell-mode-hook) . company-mode)
  :init
  (setq company-tooltip-width-grow-only t
        company-idle-delay 0
        company-require-match nil
        company-selection-wrap-around t
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-search-regexp-function #'company-search-words-in-any-order-regexp)

  (setq company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode))

  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-common-frontend
                            company-echo-metadata-frontend))

  (setq company-backends '(company-capf
                           company-semantic
                           (company-dabbrev-code
                            company-gtags
                            company-etags
                            company-keywords)
                           company-files
                           company-dabbrev))

  (defun company-yas ()
    "Hide the current completeions and insert snippets."
    (interactive)
    (company-cancel)
    (yas-expand)))

;; yasnippet
(leaf yasnippet
  :blackout t
  :hook (prog-mode-hook . yas-minor-mode)
  :init
  (setq yas-triggers-in-field t
        ;; yas-also-indent-empty-lines t
        ;; yas-indent-line 'auto
        ;; yas-also-auto-indent-first-line t
        yas-indent-line 'fixed)
  (setq yas-new-snippet-default "\
# -*- mode: snippet -*-
# name: ${1:name}
# contributor : ${2:`user-full-name`<`user-mail-address`>}
# key: ${3:key}
# --
$0`(yas-escape-text yas-selected-text)`")

  ;; mode-switch between lisp-interaction-mode and snippet-mode
  (defun my-yasnippet-switch ()
    (interactive)
    (if (equal major-mode 'snippet-mode)
        (lisp-interaction-mode)
      (snippet-mode))
    (evil-insert))

  :config
  ;; no-littering setting changed original
  (add-to-list 'yas-snippet-dirs 'my-dir-snippet)
  (yas-reload-all))

(provide 'init-company)
;;; init-company.el ends here

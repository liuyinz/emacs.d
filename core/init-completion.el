;;; init-completion.el --- setting for completion -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

(leaf company
  :blackout t
  :hook (after-init-hook . global-company-mode)
  :init
  (setq company-tooltip-width-grow-only t
        company-tooltip-align-annotations t
        company-tooltip-limit 15
        company-idle-delay 0
        company-require-match nil
        company-selection-wrap-around t
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-search-regexp-function #'company-search-words-in-any-order-regexp)
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t)

  (setq company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode))

  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-common-frontend
                            company-echo-metadata-frontend))

  (setq company-backends '(company-capf
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

(leaf prescient
  :blackout prescient-persisit-mode
  :hook (after-init-hook . prescient-persist-mode)
  :init (setq prescient-history-length 300))

;; Better sorting and filtering
(leaf company-prescient
  :blackout company-prescient-mode
  :hook (company-mode-hook . company-prescient-mode))

(leaf company-box
  :doc "deps : frame-local"
  :hook (company-mode-hook . company-box-mode)
  :init
  (setq company-box-enable-icon t
        company-box-max-candidates 20
        company-box-scrollbar nil
        company-box-show-single-candidate 'never))

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
  (yas-reload-all))

(provide 'init-completion)
;;; init-completion.el ends here

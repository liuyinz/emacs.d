;;; init-completion.el --- setting for completion -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

(leaf company
  :hook (after-init-hook . global-company-mode)
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 15
        company-tooltip-width-grow-only t
        company-format-margin-function #'company-text-icons-margin
        company-text-icons-format "%s "
        company-selection-wrap-around t
        company-idle-delay 0
        company-echo-delay 0
        company-minimum-prefix-length 1
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-search-regexp-function #'company-search-words-in-any-order-regexp)
  (setq-default company-dabbrev-other-buffers 'all)

  (setq company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode))

  ;; (setq company-frontends '(company-pseudo-tooltip-frontend
  ;;                           company-echo-metadata-frontend))

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
        (call-interactively 'company-yasnippet))

      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun my-company-enbale-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

      (defun my-lsp-fix-company-capf ()
        "Remove redundant `comapny-capf'."
        (setq company-backends
              (remove 'company-backends (remq 'company-capf company-backends))))
      (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

      (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq command 'prefix)
            (when-let ((prefix (funcall fun 'prefix)))
              (unless (memq (char-before (- (point) (length prefix)))
                            '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
                prefix))
          (progn
            (when (and (bound-and-true-p lsp-mode)
                       arg (not (get-text-property 0 'yas-annotation-patch arg)))
              (let* ((name (get-text-property 0 'yas-annotation arg))
                     (snip (format "%s (Snippet)" name))
                     (len (length arg)))
                (put-text-property 0 len 'yas-annotation snip arg)
                (put-text-property 0 len 'yas-annotation-patch t arg)))
            (funcall fun command arg))))
      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline))))

;; Better sorting and filtering
(leaf company-prescient
  :hook (company-mode-hook . company-prescient-mode))

;; yasnippet
(leaf yasnippet
  :hook
  (after-init-hook . yas-global-mode)
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
    (evil-insert)))

(provide 'init-completion)
;;; init-completion.el ends here

;;; init-completion.el --- setting for completion -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

(leaf company
  :hook ((after-init-hook . global-company-mode))
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 15
        company-tooltip-width-grow-only t
        company-tooltip-minimum-width 30
        company-tooltip-width-grow-only t
        company-tooltip-idle-delay 0
        company-idle-delay 0
        company-format-margin-function #'company-text-icons-margin
        company-text-icons-format "%s "
        company-selection-wrap-around t
        company-minimum-prefix-length 1
        company-require-match nil
        company-search-regexp-function #'company-search-words-in-any-order-regexp)

  (setq company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode))

  (setq company-backends  '(company-capf
                            ;; (company-capf company-citre :separate)
                            (company-dabbrev-code company-keywords company-files)
                            company-dabbrev))

  (setq company-transformers '(company-sort-prefer-same-case-prefix))

  ;; ISSUE https://github.com/oantolin/orderless/issues/48#issuecomment-856750410
  (defun ad/company-capf-keep-unchanged (orig-fn &rest args)
    (let ((completion-styles '(basic partial-completion)))
      (apply orig-fn args)))
  (advice-add 'company-capf :around #'ad/company-capf-keep-unchanged)

  ;; ---------------------- company-dabbrev -------------------------
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-invisible t
        company-dabbrev-other-buffers 'all)

  ;; ------------------- company-dabbrev-code -----------------------
  (setq company-dabbrev-code-everywhere t)

  ;;SEE https://emacs-china.org/t/emacs-makefile-mode-company/18138/8
  (defun makefile-company-setup ()
    ;; (setq-local company-dabbrev-other-buffers nil)
    (setq-local company-dabbrev-ignore-case t)
    (setq-local company-keywords-ignore-case t)
    (setq-local company-dabbrev-code-ignore-case t)
    (setq-local company-backends '((company-dabbrev-code
                                    company-keywords
                                    company-yasnippet
                                    company-dabbrev :separate))))
  (add-hook 'makefile-mode-hook #'makefile-company-setup)

    ;; ------------------------ company-tng ----------------------------
  (leaf company-tng
    :hook (after-init-hook . company-tng-mode))
  )

(leaf yasnippet
  :hook (after-init-hook . yas-global-mode)
  :init
  (setq yas-minor-mode-map nil)
  (setq yas-alias-to-yas/prefix-p nil)
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

  ;; silent message in start.
  (advice-add #'yas-reload-all :around #'ad/silent-message)

  :defer-config

  (leaf yasnippet-collection
    :require t
    :config
    (yasnippet-collection-initialize))

  ;; enable commit snippets
  (add-hook 'git-commit-mode-hook
            (lambda () (yas-activate-extra-mode 'git-commit-mode)))

  ;; FIXME inspired by `markdown-edit-code-block', delete chars after abort?
  (defun yas-edit-elisp-indirect ()
    "Insert elisp code in `snippet-mode' with `edit-indirect'."
    (interactive)

    ;; `edit-indirect-guess-mode-function' is dynamic scope, need require
    ;; before use let binding, SEE https://emacs-china.org/t/emacs/15580/2?u=cheunghsu
    (require 'edit-indirect)

    (unless (use-region-p) (insert "` `"))
    (let* ((visual (use-region-p))
           (begin  (if visual (region-beginning) (- (point) 2)))
           (end    (if visual (region-end) (- (point) 1)))
           (edit-indirect-guess-mode-function
            (lambda (_parent-buffer _beg _end)
              (funcall 'lisp-interaction-mode))))
      (when visual (search-forward "`" nil t 1))
      (save-excursion
        (edit-indirect-region begin end 'display-buffer)
        (unless visual (delete-char -1)))
      ))
  )

;; (leaf citre
;;   :init
;;   (require 'citre-config)
;;   (setq citre-completion-case-sensitive nil
;;         citre-default-create-tags-file-location 'global-cache
;;         citre-use-project-root-when-creating-tags t
;;         citre-prompt-language-for-ctags-command t
;;         citre-project-root-function #'projectile-project-root))

(provide 'init-completion)
;;; init-completion.el ends here

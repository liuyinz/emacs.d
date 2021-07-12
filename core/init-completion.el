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
        company-tooltip-idle-delay 0
        company-minimum-prefix-length 1
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-search-regexp-function #'company-search-words-in-any-order-regexp)

  (setq-default company-dabbrev-other-buffers 'all)

  (setq company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode))

  (setq company-backends  '((company-capf company-citre :separate)
                            (company-dabbrev-code company-keywords company-files)
                            company-dabbrev))

  :config

  ;; Remove duplicate candidate.
  (add-to-list 'company-transformers #'delete-dups)

  ;; HACK ,see @https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-company.el
  (with-no-warnings
    ;; fix lsp setting
    (defun my-lsp-fix-company-capf ()
      "Remove redundant `comapny-capf'."
      (setq company-backends
            (remove 'company-backends (remq 'company-capf company-backends))))
    (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

    ;; yasnippet
    (with-eval-after-load 'yasnippet
      (require 'company-yasnippet)

      (defun my-company-yasnippet ()
        "Hide the current completeions and show snippets."
        (interactive)
        (company-cancel)
        (call-interactively 'company-yasnippet))

      ;; Add `yasnippet' support for all company backends.
      (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (or (not company-mode/enable-yas)
                (and (listp backend) (member 'company-yasnippet backend)))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))
      (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

    ;; fix inline with company-yasnippet
    (defun my-company-yasnippet-disable-inline (fn cmd &optional arg &rest _ignore)
      "Enable yasnippet but disable it inline."
      (if (eq cmd  'prefix)
          (when-let ((prefix (funcall fn 'prefix)))
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
          (funcall fn cmd  arg))))
    (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)

    ;; HACK see@https://github.com/universal-ctags/citre/wiki/Use-Citre-together-with-lsp-mode#combine-completions-from-citre-and-lsp
    (defun company-citre (-command &optional -arg &rest _ignored)
      "Completion backend of Citre.  Execute COMMAND with ARG and IGNORED."
      (interactive (list 'interactive))
      (cl-case -command
        (interactive (company-begin-backend 'company-citre))
        (prefix (and (bound-and-true-p citre-mode)
                     (or (citre-get-symbol) 'stop)))
        (meta (citre-get-property 'signature -arg))
        (annotation (citre-capf--get-annotation -arg))
        (candidates (all-completions -arg (citre-capf--get-collection -arg)))
        (ignore-case (not citre-completion-case-sensitive))))

    ))

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

  ;; HACK silent message in start.
  (advice-add #'yas-reload-all :around #'silent-message-advice)

  :config
  ;; mode-switch between lisp-interaction-mode and snippet-mode
  (defun my-yasnippet-switch ()
    (interactive)
    (if (equal major-mode 'snippet-mode)
        (lisp-interaction-mode)
      (snippet-mode))
    (evil-insert)))

(leaf citre
  :init
  (require 'citre-config)
  (setq citre-completion-case-sensitive nil
        citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t
        citre-project-root-function #'projectile-project-root))

(provide 'init-completion)
;;; init-completion.el ends here

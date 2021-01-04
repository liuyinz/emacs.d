(require 'init-const)

;; yasnippet
(use-package yasnippet
  :blackout yas-minor-mode
  :bind (:map yas-minor-mode-map
         ("C-t" . my-yasnippet-switch))
  :hook (after-init . yas-global-mode)
  :init
  ;; silent yas message
  ; (advice-add 'yas--load-snippet-dirs :around #'silent-message-advice)
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
  :config
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map)
  ;; no-littering setting changed original
  (add-to-list 'yas-snippet-dirs 'my-dir-snippets)

  ;; mode-switch between lisp-interaction-mode and snippet-mode
  (defun my-yasnippet-switch ()
    (interactive)
    (if (equal major-mode 'snippet-mode)
        (lisp-interaction-mode)
      (snippet-mode))
    (evil-insert))
  )

(use-package company
  ;; :disabled
  :blackout
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :functions (company-search-words-in-any-order-regexp)
  :commands company-cancel
  :hook (after-init . global-company-mode)
  :bind (
         :map company-mode-map
         ("C-;" . company-yasnippet)
         :map company-active-map
         ("C-;" . my-company-yasnippet)
         ("TAB" . company-complete-common-or-cycle)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-l" . company-filter-candidates)
         ("C-s" . company-show-location)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :init
  (setq company-tooltip-limit 12
        tab-always-indent 'complete
        company-tooltip-align-annotations t
        company-idle-delay 0.0
        company-echo-delay nil
        company-minimum-prefix-length 1
        company-abort-manual-when-too-short t
        company-require-match nil
        company-selection-wrap-around t
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-search-regexp-function #'company-search-words-in-any-order-regexp)

  (setq company-backends '(company-capf
                           company-semantic
                           (company-dabbrev-code
                            company-gtags
                            company-etags
                            company-keywords)
                           company-files
                           company-dabbrev
                           ))

  (setq company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))

  (setq company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode))

  ;; @https://emacs-china.org/t/company-yasnippet/12232/13?u=cheunghsu
  (defun my-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (let ((backend company-backend))
      (company-cancel)
      (company-begin-backend
       (if (eq backend 'company-yasnippet)
           (car company-backends)
         'company-yasnippet))))

  :config
  ;; `yasnippet' integration
  (with-no-warnings
    (with-eval-after-load 'yasnippet
      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun my-company-enbale-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends (mapcar #'company-backend-with-yas company-backends)))
      ;; Enable in current backends
      (my-company-enbale-yas)
      ;; Support `company-lsp'
      (advice-add #'lsp--auto-configure :after #'my-company-enbale-yas)

      (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq command 'prefix)
            (when-let ((prefix (funcall fun 'prefix)))
              (unless (memq (char-before (- (point) (length prefix)))
                            '(?. ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
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

(provide 'init-company)

;;; init-edit.el --- edit setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

;; Hungry deletion
(leaf hungry-delete
  :hook (after-init-hook . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; ;; On-the-fly spell checker
;; (leaf flyspell
;;   :if (executable-find "aspell")
;;   :hook (((text-mode-hook outline-mode-hook) . flyspell-mode)
;;          (prog-mode-hook . flyspell-prog-mode)
;;          )
;;   :init (setq flyspell-issue-message-flag nil
;;               flyspell-doublon-as-error-flag nil
;;               ispell-program-name "aspell"
;;               ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

;; Jump to things in Emacs tree-style
(leaf avy
  :hook (after-init-hook . avy-setup-default)
  :config
  (setq avy-all-windows t
        avy-all-windows-alt t
        avy-background t
        avy-style 'at-full
        avy-keys '(?a ?s ?d ?f ?h ?j ?k ?l ?q ?u ?w ?i ?e ?o))
  (setq avy-orders-alist
        '((avy-goto-char . avy-order-closest)
          (avy-goto-word-0 . avy-order-closest)
          (avy-goto-paren . avy-order-closest)))

  ;; HACK go-to paren
  ;; @https://github.com/abo-abo/avy/wiki/custom-commands#jumping-to-an-open-paren
  ;; @https://stackoverflow.com/a/50063226/13194984
  (defun avy-goto-paren ()
    (interactive)
    (let ((avy-command this-command))   ; for look up in avy-orders-alist
      (avy-jump "[]\[(){}]")))

  ;; Pinyin support
  (leaf ace-pinyin
    :doc "deps: avy pinyinlib"
    :require t
    :init (setq ace-pinyin-simplified-chinese-only-p nil)
    :config (ace-pinyin-global-mode +1))
  )

(leaf vundo
  :hook (after-init-hook . vundo-ascii-mode)
  :init (setq vundo--window-max-height 5))

;; undo history
(leaf undohist
  :hook (after-init-hook . undohist-initialize)
  :init (setq undohist-ignored-files '("\\.git/COMMIT_EDITMSG$")))

(leaf wgrep
  ;; :hook (grep-mode-hook . wgrep-change-to-wgrep-mode)
  :init
  (setq wgrep-change-readonly-file t)
  (setq wgrep-auto-save-buffer t))

(leaf rg
  :doc "deps : transient wgrep"
  :commands rg-menu
  :init (setq rg-ignore-case 'smart)
  ;; :hook (rg-mode-hook . (lambda ()
  ;;                         (setq-local comment-start "#"
  ;;                                     comment-end "#")))
  :config

  (with-no-warnings
    ;; Integration with evil-ex-substitute
    (defun rg-replace ()
      "Replace in Rg-mode."
      (interactive)
      (wgrep-change-to-wgrep-mode)
      (evil-ex (concat "%s/" (cl-struct-slot-value 'rg-search 'pattern rg-cur-search)))
      (wgrep-finish-edit))

    ;; HACK restore if evil-ex failed
    (advice-add 'abort-recursive-edit :before
                (lambda ()
                  (let ((name (buffer-name (window-buffer (previous-window)))))
                    (when (string= name (concat "*" rg-buffer-name "*"))
                      (with-current-buffer name
                        (wgrep-abort-changes)
                        (beginning-of-buffer)))))))
  (rg-menu-transient-insert "Rerun" "R" "replace" #'rg-replace)

  ;; (add-to-list 'hs-special-modes-alist '(rg-mode "^File:.+$" "\n" "#" nil nil))
  )

(leaf valign
  :hook ((org-mode-hook markdown-mode-hook) . valign-mode))

(provide 'init-edit)
;;; init-edit.el ends here

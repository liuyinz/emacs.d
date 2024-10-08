;;; init-corfu.el --- setting for corfu -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf corfu
  :hook (after-init-hook . global-corfu-mode)
  :bind
  (:corfu-map
   ([tab]    . nil)
   ("TAB"    . nil)
   ([escape] . corfu-reset)
   ("ESC"    . corfu-reset)
   ("C-SPC"  . corfu-insert-separator)
   ("C-n"    . corfu-complete-common-or-next)
   ("C-h"    . corfu-popupinfo-toggle)
   ("C-k"    . corfu-popupinfo-scroll-up)
   ("C-j"    . corfu-popupinfo-scroll-down)
   ("C-;"    . corfu-switch-yas))
  :init
  ;; appearance
  (setq corfu-min-width 20
        corfu-max-width 80
        corfu-bar-width 0.2
        corfu-left-margin-width 1
        corfu-right-margin-width 1
        corfu-count 15
        corfu-scroll-margin 2)
  ;; logic
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-cycle t
        ;; corfu-preview-current t
        corfu-preselect 'directory
        corfu-on-exact-match 'show)

  (defun corfu-switch-yas ()
    "Switch completion backend between yasnippet and default.
If no corfu frame exists, call completion with yasnippet directly."
    (interactive)
    (if (not (and (frame-live-p corfu--frame)
                  (frame-visible-p corfu--frame)
                  (eq (corfu--metadata-get 'company-doc-buffer)
                      'yasnippet-capf--doc-buffer)))
        (let ((completion-at-point-functions '(yasnippet-capf t)))
          (completion-at-point))
      (completion-at-point)))

  (defun corfu-complete-common-or-next ()
    "Complete common prefix or go to next candidate."
    (interactive)
    (if (= corfu--total 1)
        (progn
          (corfu--goto 1)
          (corfu-insert))
      (let* ((input (car corfu--input))
             (str (if (thing-at-point 'filename) (file-name-nondirectory input) input))
             (pt (length str))
             (common (try-completion str corfu--candidates)))
        (if (and (> pt 0)
                 (stringp common)
                 (not (string= str common)))
            (insert (substring common pt))
          (corfu-next)))))
  (put 'corfu-complete-common-or-next 'completion-predicate #'ignore)

  :config

  ;; ;; ;; ISSUE https://github.com/oantolin/orderless/issues/48#issuecomment-856750410
  ;; (defun ad/corfu-style-keep-unchanged (orig-fn &rest args)
  ;;   (let ((completion-styles '(basic orderless)))
  ;;     (apply orig-fn args)))
  ;; (advice-add 'corfu--recompute-candidates :around #'ad/corfu-style-keep-unchanged)

  )

(leaf corfu-popupinfo
  :hook (global-corfu-mode-hook . corfu-popupinfo-mode)
  :defer-config
  ;; only toggle popupinfo by manually
  (setq corfu-popupinfo-delay nil)
  (setq corfu-popupinfo-hide nil
        corfu-popupinfo-max-height corfu-count))

(leaf corfu-history
  :hook (global-corfu-mode-hook . corfu-history-mode)
  :init (add-to-list 'savehist-additional-variables 'corfu-history))

;; (leaf cape
;;   :require t
;;   :init
;;   (setq cape-dabbrev-min-length 4
;;         cape-dabbrev-check-other-buffers 'some)
;;   :config
;;   (require 'cape-keyword)
;;   (appendq! completion-at-point-functions
;;             '(
;;               cape-elisp-symbol
;;               cape-elisp-block
;;               cape-file
;;               cape-dabbrev
;;               ;; cape-abbrev
;;               ;; cape-keyword
;;               ;; cape-history
;;               ;; cape-sgml
;;               ;; cape-rfc1345
;;               ;; cape-line
;;               ;; cape-ispell
;;               ;; cape-tex
;;               ;; cape-dict
;;               )))

(leaf yasnippet-capf)

(provide 'init-corfu)
;;; init-corfu.el ends

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
   ([?\C- ]  . corfu-insert-separator)
   ("\C- "   . corfu-insert-separator)
   ("\C-n"   . corfu-complete-common-or-next)
   ("\C-h"   . corfu-doc-toggle)
   ("\C-f"   . corfu-doc-scroll-up)
   ("\C-b"   . corfu-doc-scroll-down)
   ("\C-s"   . corfu-english-helper-search))
  :init
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-preselect-first nil
        corfu-cycle t
        corfu-min-width 20

        ;; hide scroll-bar
        corfu-bar-width 0
        corfu-right-margin-width 0)

  (leaf corfu-history
    :hook (global-corfu-mode-hook . corfu-history-mode)
    :init (add-to-list 'savehist-additional-variables 'corfu-history))

  (leaf corfu-docframe
    :hook (global-corfu-mode-hook . corfu-docframe-mode)
    :defer-config
    (setq corfu-docframe-delay 0.5
          corfu-docframe-max-height corfu-count))

  :config

  ;; ;; ISSUE https://github.com/oantolin/orderless/issues/48#issuecomment-856750410
  ;; (defun ad/corfu-style-keep-unchanged (orig-fn &rest args)
  ;;   (let ((completion-styles '(basic orderless)))
  ;;     (apply orig-fn args)))
  ;; (advice-add 'corfu--recompute-candidates :around #'ad/corfu-style-keep-unchanged)

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

  (leaf kind-icon
    :init
    (setq kind-icon-use-icons nil
          kind-icon-blend-background nil
          kind-icon-default-face 'corfu-default)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  (leaf cape
    :require t
    :config
    (appendq! completion-at-point-functions
              '(cape-keyword
                cape-symbol
                cape-file
                cape-abbrev
                cape-dabbrev
                cape-history
                ;; cape-sgml
                ;; cape-rfc1345
                ;; cape-line
                ;; cape-ispell
                ;; cape-tex
                ;; cape-dict
                )))

  )

(provide 'init-corfu)
;;; init-corfu.el ends here

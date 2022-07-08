;;; init-completion.el --- setting for completion -*- lexical-binding: t no-byte-compile: t -*-
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

  (leaf corfu-doc
    :hook (global-corfu-mode-hook . corfu-doc-mode)
    :defer-config
    (setq corfu-doc-auto t
          corfu-doc-delay 0.8
          coruf-doc-transition 'hide
          corfu-doc-max-height corfu-count))

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
      (let* ((str (car corfu--input))
             (pt (cdr corfu--input))
             (common (try-completion str corfu--candidates)))
        (if (and (stringp common)
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
                cape-sgml
                ;; cape-rfc1345
                ;; cape-line
                ;; cape-ispell
                ;; cape-tex
                ;; cape-dict
                )))

  )

(leaf yasnippet
  :commands yas-expand
  :hook (after-init-hook . yas-global-mode)
  :bind
  (:yas-keymap
   ([tab] . yas-next-field)
   ("TAB" . yas-next-field))
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

  ;; quit first if corfu--frame is live
  (advice-add #'yas-expand :before (lambda ()
                                     (when (frame-live-p corfu--frame)
                                       (corfu-quit))))
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
;;         citre-prompt-language-for-ctags-command t)

;;   (with-eval-after-load 'projectile
;;     (setq citre-project-root-function #'projectile-project-root))
;;   )

(provide 'init-completion)
;;; init-completion.el ends here

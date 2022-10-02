;;; init-yas.el --- Setup for yasnippet -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-07-09 08:08:36

;;; Commentary:

;;; Code:

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
  (with-eval-after-load 'corfu
    (advice-add #'yas-expand :before (lambda ()
                                       (when (frame-live-p corfu--frame)
                                         (corfu-quit)))))

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

(provide 'init-yas)
;;; init-yas.el ends here

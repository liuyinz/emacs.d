;;; init-font.el --- font setup -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2024-02-09 05:28:51

;;; Commentary:

;;; Code:

(defvar my/default-font "LXGW WenKai Mono Light 17")
(defvar my/chinese-font-family "LXGW WenKai Mono")

;; setup default frame
(add-to-list 'default-frame-alist `(font . ,my/default-font))

(leaf nerd-icons)

(leaf nerd-icons-completion
  :hook (after-init-hook . nerd-icons-completion-mode)
  :init
  (setq nerd-icons-completion-icon-size 0.8)
  :defer-config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(leaf nerd-icons-ibuffer
  :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode)
  :init
  (setq nerd-icons-ibuffer-icon-size 0.8)
  (setq nerd-icons-ibuffer-formats
        '((mark " " (icon 2 2)
                " " (name 20 20 :left :elide)
                " " modified read-only locked
                " " (size-h 7 -1 :right)
                " " (mode+ 16 16 :left :elide)
                " " filename-and-process+)
          (mark " " name))))

;; ;; setup chinese patch
;; (defun my/patch-chinese-charset ()
;;   "Patch chinese character with lxgw font."
;;   `(dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font
;;      ;; "fontset-startup"
;;      (frame-parameter nil 'font)
;;      charset
;;      (font-spec :family ,my/chinese-font-family))))
;; (add-hook 'after-make-graphic-frame-hook #'my/patch-chinese-charset)

(provide 'init-font)
;;; init-font.el ends here

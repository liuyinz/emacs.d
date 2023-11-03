;;; init-gui.el --- summary -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-04-15 13:51:24

;;; Commentary:

;;; Code:

;; ;; setup chinese patch
(defun my/patch-lxgw-chinese-charset ()
  "Patch chinese character with lxgw font."
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     ;; "fontset-startup"
     (frame-parameter nil 'font)
     charset
     (font-spec :family "LXGW WenKai"))))
(add-hook 'after-make-graphic-frame-hook #'my/patch-lxgw-chinese-charset)

(leaf exec-path-from-shell
  :hook (after-make-graphic-frame-hook . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
        ;; Only need to load .zshenv variable
        exec-path-from-shell-arguments nil)
  :defer-config

  ;; SEE https://github.com/manateelazycat/cache-path-from-shell/blob/master/cache-path-from-shell.el
  (defvar cache-path-from-shell-loaded-p nil)
  (defun ad/cache-path-from-shell (fn &rest _)
    "Cache $PATH once for all."
    (unless cache-path-from-shell-loaded-p
      (funcall fn)
      (setq cache-path-from-shell-loaded-p t)))
  (advice-add 'exec-path-from-shell-initialize :around #'ad/cache-path-from-shell))

;; REQUIRE pip3 install my-cookies
(leaf leetcode
  :init
  (setq leetcode-prefer-language "javascript"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory "~/Documents/repo/leetcode"))

;; REQUIRE npm install -g @mermaid-js/mermaid-cli
(leaf mermaid-mode)

(leaf org-mordern)

(provide 'init-gui)
;;; init-gui.el ends here

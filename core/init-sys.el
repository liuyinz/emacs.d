;;; init-sys.el --- leaf,compile,shell,littering  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

(require 'leaf)
(eval-and-compile
  (setq leaf-expand-minimally nil)
  (setq leaf-defaults '(:ensure nil)))

;; Garbage Collector Magic Hack
(leaf gcmh
  :require t
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x1000000) ; 16MB
  :config
  (gcmh-mode 1))

;; keep ~/.emacs.d clean
(leaf no-littering
  :require t
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" my-dir-cache)
        no-littering-var-directory (expand-file-name "var/" my-dir-cache))
  :config
  ;; exclude these in recentf
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))

  ;; save auto-save file if needed
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; restore yasnippet settings
  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '(my-dir-snippet))))

;; Environment
(leaf exec-path-from-shell
  :hook (after-make-window-frame-hook . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
        ;; Only need to load .zshenv variable
        exec-path-from-shell-arguments nil)
  :config

  ;; SEE https://github.com/manateelazycat/cache-path-from-shell/blob/master/cache-path-from-shell.el
  (defvar cache-path-from-shell-loaded-p nil)
  (defun ad/cache-path-from-shell (fn &rest _)
    "Cache $PATH once for all."
    (unless cache-path-from-shell-loaded-p
      (funcall fn)
      (setq cache-path-from-shell-loaded-p t)))
  (advice-add 'exec-path-from-shell-initialize
              :around
              #'ad/cache-path-from-shell))

(provide 'init-sys)
;;; init-sys.el ends here

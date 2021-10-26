;;; init-sys.el --- Optimization and env-path -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz@gmail.com>
;; Created: 2021-10-16 18:17:36

;;; Commentary:

;;; Code:

;; Garbage Collector Magic Hack
(use-package gcmh
  :demand t
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x1000000) ; 16MB
  :config
  (gcmh-mode 1))

(use-package exec-path-from-shell
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
  (advice-add 'exec-path-from-shell-initialize :around #'ad/cache-path-from-shell))

;; keep ~/.emacs.d clean
(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" my/dir-cache)
        no-littering-var-directory (expand-file-name "var/" my/dir-cache))
  :config
  ;; save auto-save file if needed
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; exclude these in recentf
  (with-eval-after-load 'recentf
    (appendq! recentf-exclude
              `(,no-littering-var-directory ,no-littering-etc-directory))))

(provide 'init-sys)
;;; init-sys.el ends here

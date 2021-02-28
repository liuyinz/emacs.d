;;; init-sys.el --- system setting  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

(require 'leaf)
(eval-and-compile
  (setq leaf-expand-minimally nil)
  (setq leaf-defaults '(:ensure nil)))

(leaf leaf-keywords
  :require t
  :config
  (leaf-keywords-init)
  (leaf blackout :require t))

(leaf auto-compile
  :blackout t
  :hook (after-init-hook . auto-compile-on-load-mode))

;; Environment
(leaf exec-path-from-shell
  :require t
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
        exec-path-from-shell-arguments '("-l"))
  :config
  ;; HACK , Cache $PATH once for all,
  ;; inspired by @https://github.com/manateelazycat/cache-path-from-shell/blob/master/cache-path-from-shell.el
  (defvar cache-path-from-shell-loaded-p nil)
  (defun cache-path-from-shell-advice (fn &rest _)
    (when (not cache-path-from-shell-loaded-p)
      (funcall fn)
      (setq cache-path-from-shell-loaded-p t)))
  (advice-add 'exec-path-from-shell-initialize :around #'cache-path-from-shell-advice)

  (exec-path-from-shell-initialize))


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

(provide 'init-sys)
;;; init-sys.el ends here

;;; init-sys.el --- system setting  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

(require 'leaf)
(eval-and-compile
  (setq leaf-expand-minimally nil)
  (setq leaf-defaults '(:ensure nil)))

(leaf leaf-keywords
  :require t
  :defun leaf-keywords-init
  :config (leaf-keywords-init))

(leaf blackout :require t)

(leaf auto-compile
  :require t
  :defun (auto-compile-on-load-mode auto-compile-on-save-mode)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Environment
(leaf exec-path-from-shell
  :require t
  :defvar (exec-path-from-shell-check-startup-files
           exec-path-from-shell-variables
           exec-path-from-shell-arguments)
  :defun exec-path-from-shell-initialize
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
        exec-path-from-shell-arguments '("-l"))
  :config
  ;; Cache $PATH once for all, refer @https://github.com/manateelazycat/cache-path-from-shell/blob/master/cache-path-from-shell.el
  (defvar cache-path-from-shell-loaded-p nil)
  (defadvice exec-path-from-shell-initialize (around cache-path-from-shell-advice activate)
    (if cache-path-from-shell-loaded-p
        (message "All shell environment variables has loaded in Emacs, yow!")
      (setq cache-path-from-shell-loaded-p t)
      ad-do-it))
  (exec-path-from-shell-initialize))

;; keep ~/.emacs.d clean
(leaf no-littering
  :require t
  :defvar (recentf-exclude
           my-dir-cache
           no-littering-var-directory
           no-littering-etc-directory)
  :defun (no-littering-expand-etc-file-name
          no-littering-expand-var-file-name)
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
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; (use-package with-proxy
;;   :init
;;   (setq with-proxy-http-server "127.0.0.1:7890"))

(provide 'init-sys)
;;; init-sys.el ends here

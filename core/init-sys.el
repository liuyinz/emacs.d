;;; init-sys.el --- leaf,compile,shell,littering  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

(require 'leaf)
(eval-and-compile
  (setq leaf-expand-minimally nil)
  (setq leaf-defaults '(:ensure nil)))

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

(leaf packed
  :config
  (defvar packed-ignore-directory-regexp
    "\\(?:^\\.\\|CVS\\|RCS\\|^t$\\|^tests?$\\|^vendor$\\|^script$\\)")
  (defun packed-ignore-directory-p-patch (directory)
    (or (string-match packed-ignore-directory-regexp
                      (file-name-nondirectory
                       (directory-file-name directory)))
        (file-exists-p (expand-file-name ".nosearch" directory))))
  (advice-add 'packed-ignore-directory-p :override #'packed-ignore-directory-p-patch))

(leaf auto-compile
  :doc "deps: packed"
  :commands toggle-auto-compile
  :init
  (setq auto-compile-visit-failed nil
        auto-compile-ding nil
        auto-compile-update-autoloads t
        auto-compile-toggle-recompiles nil))

;; Environment
(leaf exec-path-from-shell
  :hook (after-make-console-frame-hook . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
        exec-path-from-shell-arguments '("-l"))
  :config
  ;; HACK , Cache $PATH once for all
  ;; @https://github.com/manateelazycat/cache-path-from-shell/blob/master/cache-path-from-shell.el
  (defvar cache-path-from-shell-loaded-p nil)
  (defun cache-path-from-shell-advice (fn &rest _)
    (when (not cache-path-from-shell-loaded-p)
      (funcall fn)
      (setq cache-path-from-shell-loaded-p t)))
  (advice-add 'exec-path-from-shell-initialize :around #'cache-path-from-shell-advice))

;; (leaf restart-emacs
;;   :init
;;   (setq restart-emacs-daemon-with-tty-frames-p nil
;;         restart-emacs-restore-frames t)
;;   :commands restart-emacs restart-emacs-start-new-emacs)

(provide 'init-sys)
;;; init-sys.el ends here

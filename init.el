;;; init.el --- init startup  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;;; -------------------------- Debug -------------------------------

;; (setq debug-on-error t)
;; (debug-on-entry 'load-file)

;;; ------------------------- Warning ------------------------------

(dolist (func '(define-minor-mode))
  (advice-add func :around #'ad/silent-message))

;; avoid cl depreciated warning
(setq byte-compile-warnings '(not docstrings free-vars obsolete))

;;; ------------------------- Loading ------------------------------

(when (version< emacs-version "28")
  (error "Please upgrade your emacs-version above 28 !"))

;; add user config dir to load-path
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))

(require 'init-const)
(require 'init-lib)

;; TODO add emacs-plug to update submodules async
;; add submodules to load-path
(defun add-subdirs-to-load-path (dir)
  "Recursive add `DIR` to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path my-dir-lib)

;; load custom.el if exists.
(setq custom-file (expand-file-name "etc/custom.el" my-dir-cache))
(when (file-exists-p custom-file)
  (load custom-file nil :no-message))

(with-temp-message ""
  (require 'init-benchmark)
  (require 'init-sys)
  (require 'init-default)
  (require 'init-frame)
  (require 'init-evil)
  (require 'init-completion)
  (require 'init-minibuffer)
  ;; ui
  (require 'init-ui)
  (require 'init-highlight)
  (require 'init-window)
  ;; (require 'init-ibuffer)
  (require 'init-dired)
  (require 'init-edit)
  (require 'init-tool)
  (require 'init-write)
  ;; programing
  (require 'init-ide)
  (require 'init-vcs)
  (require 'init-project)
  ;; language
  (require 'init-lsp)
  (require 'init-lang)
  (require 'init-web)
  (require 'init-elisp)
  (require 'init-markdown)
  (require 'init-org)
  (require 'init-transient)
  (require 'init-key))
;;; init.el ends here

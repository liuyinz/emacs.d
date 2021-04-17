;;; init.el --- init startup  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)
;; (debug-on-entry 'load-file)

;; avoid cl depreciated warning
(setq byte-compile-warnings '(cl-function))

(when (version< emacs-version "27")
  (error "Please upgrade your emacs-version above 27 !"))

;; add user config dir to load-path
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))

;; gc magic hack
(require 'init-gcmh)

(require 'init-const)
(require 'init-lib)

;; for native-comp branch
;; (setq comp-speed 2)
;; (setq comp-deferred-compilation nil)
;; (when (boundp 'comp-eln-load-path)
;;   (setq comp-eln-load-path (expand-file-name "var/eln-cache/" my-dir-cache)))

;; add submodules to load-path
(defun add-subdirs-to-load-path (dir)
  "Recursive add `DIR` to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path my-dir-module)

;; load custom.el if exists.
(setq custom-file (expand-file-name "etc/custom.el" my-dir-cache))
(when (file-exists-p custom-file)
  (load custom-file nil :no-message))

;; load core config
(with-temp-message ""
  ;; (require 'init-benchmark)
  (require 'init-sys)
  (require 'init-default)
  (require 'init-evil)
  (require 'init-completion)
  (require 'init-selectrum)
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
  (require 'init-elisp)
  (require 'init-markdown)
  (require 'init-key))
;;; init.el ends here

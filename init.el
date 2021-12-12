;;; init.el --- init startup  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; -------------------------- Debug -------------------------------

;; (setq debug-on-error t)
;; (debug-on-entry 'load-file)

;; ------------------------- Warning ------------------------------

;; add user config dir to load-path
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))

(require 'init-const)

;; ensure dir exists
(dolist (dir `(,my/dir-cache
               ,my/dir-core
               ,my/dir-lib
               ,my/dir-ext))
  (make-directory dir t))

(require 'init-func)

(unless emacs/>=28p
  (error "Please upgrade your emacs-version above 28 !"))

;; avoid cl depreciated warning
(setq byte-compile-warnings '(not docstrings free-vars obsolete))

(dolist (func '(define-minor-mode))
  (advice-add func :around #'ad/silent-message))

;; load custom.el if exists.
(setq custom-file (expand-file-name "etc/custom.el" my/dir-cache))
(when (file-exists-p custom-file)
  (load custom-file nil :no-message))

;; ------------------------- Loading ------------------------------

(with-temp-message ""
  (require 'init-bootstrap)
  (require 'init-sys)
  (require 'init-frame)
  (require 'init-builtin)
  (require 'init-evil)
  (require 'init-completion)
  (require 'init-minibuffer)
  ;;   ;; ui
  (require 'init-ui)
  (require 'init-highlight)
  (require 'init-window)
  ;;   ;; (require 'init-ibuffer)
  (require 'init-dired)
  (require 'init-edit)
  (require 'init-tool)
  (require 'init-dev)
  ;;   ;; programing
  (require 'init-ide)
  (require 'init-vcs)
  (require 'init-project)
  ;;   ;; language
  (require 'init-lsp)
  (require 'init-web)
  (require 'init-lang)
  (require 'init-elisp)
  (require 'init-go)
  (require 'init-markdown)
  (require 'init-org)
  (require 'init-key)
  )
;;; init.el ends here

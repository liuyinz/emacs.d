;;; init.el --- init startup  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; -------------------------- Debug -------------------------------

;; (setq debug-on-error t)
;; (debug-on-entry 'load-file)

;; ------------------------- Warning ------------------------------

;; add user config dir to load-path
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))

(require 'init-define)

;; ensure dir exists
(dolist (dir `(,my/dir-cache
               ,my/dir-lib
               ,my/dir-ext))
  (make-directory dir t))

(unless emacs/>=28p
  (error "Please upgrade your emacs-version above 28 !"))

;; surpress warning
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
  (if (and (require 'vc-hooks nil t)
           (not (vc-up-to-date-p (expand-file-name "init-debug.el" my/dir-core))))
      (require 'init-debug)
    ;; normal loading
    (require 'init-builtin)
    (require 'init-meow)
    (require 'init-minibuffer)
    (require 'init-completion)
    (require 'init-ui)
    (require 'init-highlight)
    (require 'init-window)
    ;; (require 'init-ibuffer)
    (require 'init-dired)
    (require 'init-edit)
    (require 'init-tool)
    (require 'init-dev)
    ;; engineering
    (require 'init-ide)
    (require 'init-vcs)
    (require 'init-project)
    (require 'init-lsp)
    ;; langs
    (require 'init-web)
    (require 'init-lang)
    (require 'init-elisp)
    (require 'init-go)
    (require 'init-markdown)
    (require 'init-org)
    (require 'init-gui)
    (require 'init-key)
    ))
;;; init.el ends here

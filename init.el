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
               ,my/dir-ext
               ,my/dir-debug))
  (make-directory dir t))

(unless emacs/>=29.1p
  (error "Please upgrade your emacs-version above 29 !"))

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
  (if (my/debug-begin-p)
      (require 'init-debug)
    ;; normal loading
    (require 'init-elisp)
    (require 'init-builtin)
    (require 'init-duplexer)
    (require 'init-meow)
    (require 'init-minibuffer)
    (require 'init-consult)
    (require 'init-embark)
    (require 'init-outline)
    ;; (require 'init-citre)
    (require 'init-yas)
    (require 'init-bridge)
    ;; (require 'init-corfu)
    ;; (require 'init-lsp)
    (require 'init-ui)
    (require 'init-highlight)
    (require 'init-window)
    (require 'init-ibuffer)
    (require 'init-dired)
    (require 'init-undo)
    (require 'init-edit)
    (require 'init-jump)
    (require 'init-pair)
    (require 'init-spell)
    (require 'init-tool)
    (require 'init-passwd)
    (require 'init-package)
    (require 'init-benchmark)
    (require 'init-color)
    ;; engineering
    (require 'init-ide)
    (require 'init-flymake)
    (require 'init-format)

    (require 'init-vcs)
    (require 'init-reader)
    (require 'init-project)
    ;; (require 'init-projectile)
    ;; langs
    (require 'init-web)
    ;; (require 'init-vue)
    (require 'init-lang)
    ;; (require 'init-go)
    (require 'init-markdown)
    (require 'init-write)
    ;; (require 'init-org)
    (require 'init-gui)
    (require 'init-key)
    (require 'init-env)
    ))
;;; init.el ends here

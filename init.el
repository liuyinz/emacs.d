;;; init.el --- init startup  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)
;; (debug-on-entry 'load-file)

;; HACK ,@https://emacs.stackexchange.com/a/64560
(defun my/advice-silence-messages (func &rest args)
  "Invoke FUNC with ARGS, silencing all messages.  This is an `:around' advice for many different functions."
  (cl-letf (((symbol-function #'message) #'ignore))
    (apply func args)))
(dolist (func '(define-minor-mode))
  (advice-add func :around #'my/advice-silence-messages))

;; avoid cl depreciated warning
(setq byte-compile-warnings '(not docstrings free-vars obsolete))

(when (version< emacs-version "28")
  (error "Please upgrade your emacs-version above 28 !"))

;; add user config dir to load-path
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))

;; gc magic hack
(require 'init-gcmh)

(require 'init-const)
(require 'init-lib)

;; for native-comp branch
;; (setq comp-speed 2)
;; (setq comp-asyncjobs-number 7
;;       comp-deferred-compilation nil
;;       comp-async-report-warnings-errors nil)

;; (when (boundp 'comp-eln-load-path)
;;   (setcar comp-eln-load-path
;;           (expand-file-name ".cache/var/eln-cache/" user-emacs-directory)))

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
  ;; (require 'init-transient)
  (require 'init-key))
;;; init.el ends here

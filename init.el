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

;; user define variables
(require 'init-const)
;; gc magic hack
(require 'init-gcmh)

;; for native-comp branch
;; (setq comp-speed 2)
;; (setq comp-deferred-compilation nil)
;; (when (boundp 'comp-eln-load-path)
;;   (setq comp-eln-load-path (expand-file-name "eln-cache/" my-dir-cache)))

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

;; lazy-load large modules, use with (leaf :defer-config)
(defvar doom-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
here may cause noticeable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and can be
broken up into:
  (doom-load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))
This is already done by the lang/org module, however.
If you want to disable incremental loading altogether, either remove
`doom-load-packages-incrementally-h' from `emacs-startup-hook' or set
`doom-incremental-first-idle-timer' to nil. Incremental loading does not occur
in daemon sessions (they are loaded immediately at startup).")

(defvar doom-incremental-first-idle-timer 0.5
  "How long (in idle seconds) until incremental loading starts.
Set this to nil to disable incremental loading.")

(defvar doom-incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defvar doom-incremental-load-immediately (daemonp)
  "If non-nil, load all incrementally deferred packages immediately at startup.")

;;; Mutation
(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

(defun doom-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.
If NOW is non-nil, load PACKAGES incrementally, in `doom-incremental-idle-timer'
intervals."
  (if (not now)
      (appendq! doom-incremental-packages packages)
    (while packages
      (let ((req (pop packages)))
        (unless (featurep req)
          (message "Incrementally loading %s" req)
          (condition-case-unless-debug e
              (or (while-no-input
                    ;; If `default-directory' is a directory that doesn't exist
                    ;; or is unreadable, Emacs throws up file-missing errors, so
                    ;; we set it to a directory we know exists and is readable.
                    (let ((default-directory my-dir-cache)
                          (gc-cons-threshold most-positive-fixnum)
                          file-name-handler-alist)
                      (require req nil t))
                    t)
                  (push req packages))
            ((error debug)
             (message "Failed to load %S package incrementally, because: %s"
                      req e)))
          (if (not packages)
              (message "Finished incremental loading")
            (run-with-idle-timer doom-incremental-idle-timer
                                 nil #'doom-load-packages-incrementally
                                 packages t)
            (setq packages nil)))))))

(defun doom-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `doom-incremental-packages'.
If this is a daemon session, load them all immediately instead."
  (if doom-incremental-load-immediately
      (mapc #'require (cdr doom-incremental-packages))
    (when (numberp doom-incremental-first-idle-timer)
      (run-with-idle-timer doom-incremental-first-idle-timer
                           nil #'doom-load-packages-incrementally
                           (cdr doom-incremental-packages) t))))

(add-hook 'emacs-startup-hook #'doom-load-packages-incrementally-h)

(doom-load-packages-incrementally '(
                                    consult
                                    magit))

;; load core config
(with-temp-message ""
  (require 'init-benchmark)
  (require 'init-funcs)
  (require 'init-sys)

  ;; (require 'init-test)
  (require 'init-default)
  (require 'init-completion)
  (require 'init-selectrum)
  ;; ui
  (require 'init-ui)
  (require 'init-highlight)
  ;; (require 'init-frame)
  (require 'init-window)
  ;; (require 'init-ibuffer)
  (require 'init-dired)
  ;; (require 'init-org)
  (require 'init-edit)
  (require 'init-shell)
  (require 'init-tool)
  ;; programing
  (require 'init-vcs)
  (require 'init-ide)
  (require 'init-project)
  ;; (require 'init-lsp)
  ;; language
  (require 'init-lang)
  (require 'init-elisp)
  (require 'init-web)
  (require 'init-js)
  (require 'init-evil)
  (require 'init-key)
  )
;;; init.el ends here

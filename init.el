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

;; add user config dir to load-path
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))

(require 'init-const)
(require 'init-lib)

(unless emacs/>=28p
  (error "Please upgrade your emacs-version above 28 !"))

;; TODO add emacs-plug to update submodules async
;; add submodules to load-path
(defvar load-path-exclude-regexp
  "\\`\\(rcs\\|cvs\\|bin\\|tool\\|doc\\|document\\|documentation\\|test\\|demo\\|src\\|target\\|img\\|image\\|script\\|manual\\|screenshot\\|snapshot\\|git-hooks\\|data\\|travis\\|example\\|sample\\|font\\)\\(es\\|s\\)?\\'"
  "Regex used to exclude in `load-path'.")

(defun recursive-add-to-load-path ()
  "Recursively add all subdirectories of `default-directory' to `load-path'.
File match `load-path-exclude-regexp' would be excluded."
  (let (dirs
	    attrs
	    (pending (list default-directory)))
    (while pending
      (push (pop pending) dirs)
      (let* ((this-dir (car dirs))
	         (contents (directory-files this-dir))
	         (default-directory this-dir)
	         (canonicalized (if (fboundp 'w32-untranslated-canonical-name)
				                (w32-untranslated-canonical-name this-dir))))
	    (setq attrs (or canonicalized
			            (nthcdr 10 (file-attributes this-dir))))
	    (unless (member attrs normal-top-level-add-subdirs-inode-list)
	      (push attrs normal-top-level-add-subdirs-inode-list)
	      (dolist (file contents)
	        (and (string-match "\\`[[:alnum:]]" file)
		         ;; The lower-case variants of RCS and CVS are for DOS/Windows.
                 (not (string-match load-path-exclude-regexp file))
		         (file-directory-p file)
		         (let ((expanded (expand-file-name file)))
		           (or (file-exists-p (expand-file-name ".nosearch" expanded))
		               (setq pending (nconc pending (list expanded))))))))))
    (normal-top-level-add-to-load-path (cdr (nreverse dirs)))))

(defun add-subdirs-to-load-path (dir)
  "Recursive add `DIR` to `load-path'."
  (let ((default-directory (file-name-as-directory dir))
        (case-fold-search t))
    (add-to-list 'load-path dir)
    (recursive-add-to-load-path)))
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

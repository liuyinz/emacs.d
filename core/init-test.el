;;; init-test.el --- config for test -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------ Load init -----------------------------

(column-number-mode)
(line-number-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; add init-*.el to load-path
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))

;; const viriable
(require 'init-const)
(require 'init-lib)

;; add submodules to load-path
(defun add-subdirs-to-load-path (dir)
  "Recursive add `DIR` to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path my/dir-lib)

(load-theme 'wombat)

(require 'init-sys)
;; (require 'init-default)

;; --------------------------- Test -------------------------------


(provide 'init-test)
;;; init-test.el ends here

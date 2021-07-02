;;; init-test.el --- config for test -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; appearence
(column-number-mode)
(line-number-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; ---------------------- Load const, env and path ------------------------------
;; add init-*.el to load-path
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))

;; const viriable
(require 'init-const)

;; add submodules to load-path
(defun add-subdirs-to-load-path (dir)
  "Recursive add `DIR` to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path my-dir-lib)

(require 'init-sys)
;; ---------------------- Test module ------------------------------

(provide 'init-test)
;;; init-test.el ends here

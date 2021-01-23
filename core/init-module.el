;;; init-module.el --- submodule settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; add submodules to load-path
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path my-dir-module)

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(provide 'init-module)
;;; init-module.el ends here

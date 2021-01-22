;;; init-module.el --- submodule settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path my-dir-module)

(provide 'init-module)
;;; init-module.el ends here

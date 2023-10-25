;;; init-dev.el --- Devtool for Emacs Package -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2021-11-02 12:35:16

;;; Commentary:

;; SEE https://github.com/alphapapa/emacs-package-dev-handbook

;;; Code:

;; ----------------------- Demonstration ---------------------------

(leaf keycast)

(leaf profiler
  :init
  (setq profiler-report-leaf-mark  ">")
  (defun ad/profiler-bytes-h (str)
    "reformat with human-readeable size"
    (let ((s (cl-count ?, str)))
      (cond
       ((= s 1) (concat (substring str 0 -4) " K"))
       ((= s 2) (concat (substring str 0 -8) " M"))
       ((>= s 3) (concat (substring str 0 -12) " G"))
       (t str))))
  (advice-add 'profiler-format-number :filter-return #'ad/profiler-bytes-h))

(leaf etrace)

(provide 'init-dev)
;;; init-dev.el ends here

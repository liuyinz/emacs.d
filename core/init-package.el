;;; init-package.el --- Setup for emacs package development -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2023-11-23 05:06:29

;;; Commentary:

;;; Code:

(leaf package-lint)

(leaf package-lint-flymake
  :hook (emacs-lisp-mode-hook . elisp-package-setup)
  :init
  (defun elisp-package-setup ()
    "docstring"
    (when-let* ((filepath (buffer-file-name))
                ((string-prefix-p my/dir-lib filepath))
                ((package-lint-looks-like-a-package-p)))
      (package-lint-flymake-setup))))

;; TODO imitate markdown-toc to write a function to convert and update commnetary section to README.md
(leaf el2readme
  
  )


(provide 'init-package)
;;; init-package.el ends here

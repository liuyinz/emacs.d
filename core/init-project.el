;;; init-project.el --- project setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package find-file-in-project
  :init
  (setq ffip-use-rust-fd t)
  ;; (setq ffip-project-file '(".svn" ".hg" ".git")
  (setq ffip-match-path-instead-of-filename t)
  (setq ffip-filename-history t))

(provide 'init-project)
;;; init-project.el ends here

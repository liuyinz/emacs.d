;;; init-project.el --- Setup project -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-12 07:19:27

;;; Commentary:

;;; Code:

(leaf project
  :require t
  :init
  (setq project-vc-merge-submodules nil))

(leaf consult-project-extra)

(provide 'init-project)
;;; init-project.el ends here

;;; init-env.el --- Environment setup -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2023-11-07 15:37:39

;;; Commentary:

;;; Code:

;; SEE https://github.com/purcell/envrc#usage
;; add hook as late as possible to call it first.
(leaf envrc
  :hook (after-init-hook . envrc-global-mode))

(provide 'init-env)
;;; init-env.el ends here

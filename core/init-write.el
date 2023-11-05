;;; init-write.el --- Writing setup -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-23 07:39:21

;;; Commentary:

;;; Code:

(leaf focus)

(leaf olivetti
  :init
  (setq olivetti-body-width nil
        olivetti-style t))

;; TODO customize easy-hugo-buffer, refactor easy-hugo, tag filter
(leaf easy-hugo
  :init
  (setq easy-hugo-basedir  "~/Code/blog/"
        easy-hugo-postdir "content/posts/"
        easy-hugo-url  "https://liuyinz.github.io/"
        easy-hugo-preview-url "http://localhost:1313/"
        easy-hugo-server-flags "-D"
        easy-hugo-no-help t)
  :defer-config
  (easy-hugo-enable-menu))

(leaf denote
  :init )


(provide 'init-write)
;;; init-write.el ends here

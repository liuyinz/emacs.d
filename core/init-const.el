;;; init-const.el --- defconst settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst my-dir-core
  (expand-file-name "core" user-emacs-directory)
  "User dir for Emacs configs.")

(defconst my-dir-module
  (expand-file-name "module" user-emacs-directory)
  "User dir for submodules.")

(defconst my-dir-snippet
  (expand-file-name "snippet" user-emacs-directory)
  "User dir for code snippets.")

(defconst my-dir-cache
  (expand-file-name ".cache" user-emacs-directory)
  "User dir for recentf,places and so on.")

;; ensure dir exists
(dolist (dir (mapcar #'symbol-value '(my-dir-cache
                                      my-dir-core
                                      my-dir-module
                                      my-dir-snippet)))
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defconst my-homepage
  "https://github.com/liuyinz/.emacs.d"
  "The Github Page of mine.")

(setq user-full-name "食無魚")
(setq user-mail-address "liuyinz@gmail.com")

(defvar my-proxy "127.0.0.1:4780" "Set network proxy.")

(provide 'init-const)
;;; init-const.el ends here

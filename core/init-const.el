;;; init-const.el --- defconst settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst my-dir-core
  (expand-file-name "core" user-emacs-directory)
  "User dir for Emacs configs.")

(defconst my-dir-lib
  (expand-file-name "lib" user-emacs-directory)
  "User dir for submodules.")

(defconst my-dir-snippet
  (expand-file-name "snippets" user-emacs-directory)
  "User dir for code snippets.")

(defconst my-dir-ext
  (expand-file-name "ext" user-emacs-directory)
  "User dir for external tools.")

(defconst my-dir-cache
  (expand-file-name ".cache" user-emacs-directory)
  "User dir for recentf,places and so on.")

;; ensure dir exists
(dolist (dir `(,my-dir-cache
               ,my-dir-core
               ,my-dir-ext
               ,my-dir-lib
               ,my-dir-snippet))
  (make-directory dir t))

(defconst user-home-page
  "https://github.com/liuyinz"
  "The Github Page of mine.")

(setq user-full-name "食無魚")
(setq user-mail-address "liuyinz@gmail.com")

(provide 'init-const)
;;; init-const.el ends here

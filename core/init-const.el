;;; init-const.el --- defconst settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst my-dir-core
  (expand-file-name "core" user-emacs-directory)
  "User dir for Emacs configs.")

(defconst my-dir-ext
  (expand-file-name "ext" user-emacs-directory)
  "User dir for external tools.")

(defconst my-dir-snippets
  (expand-file-name "snippets" user-emacs-directory)
  "User dir for code snippets.")

(defconst my-dir-cache
  (expand-file-name ".cache" user-emacs-directory)
  "User dir for recentf,places and so on.")

;; (defconst my-dir-elpa
;;   (expand-file-name (concat "elpa-" emacs-version) my-dir-cache)
;;   "User dir for packages from melpa")

;; ensure dir exists
(dolist (dir (mapcar #'symbol-value '(my-dir-cache
                                      my-dir-core
                                      ;; my-dir-elpa
                                      my-dir-ext
                                      my-dir-snippets)))
  (unless (file-exists-p dir)
    (make-directory dir t)))

;; (defconst elpa-tsinghua '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;                           ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                           ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))
;;   "elpa source from tsinghua")
;;
;; (defconst elpa-tencent '(("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
;;                          ("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
;;                          ("org" . "http://mirrors.coud.tencent.com/elpa/org/"))
;;   "elpa source from tencent")
;;
;; (defconst elpa-emacs-china '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;                              ("melpa" . "http://elpa.emacs-china.org/melpa/")
;;                              ("org" . "http://elpa.emacs-china.org/org/"))
;;   "elpa source from emacs-china")
;;
;; (defconst elpa-ustc '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
;;                       ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
;;                       ("org" . "http://mirrors.ustc.edu.cn/elpa/org/"))
;;   "elpa source from ustc")

(defconst my-homepage
  "https://github.com/liuyinz/.emacs.d"
  "The Github Page of mine.")

(setq user-full-name "食無魚")

(setq user-mail-address "liuyinz@gmail.com")

(defvar my-proxy "127.0.0.1:4780" "Set network proxy.")

(provide 'init-const)
;;; init-const.el ends here

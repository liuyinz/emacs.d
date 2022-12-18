;;; init-secure.el --- Security related -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-22 16:49:36

;;; Commentary:

;;; Code:

(leaf auth-source
  :commands auth-source-user-and-password
  :init
  (setq auth-sources '("~/.authinfo")))

(leaf authinfo-color-mode
  :mode ("authinfo.gpg\\'" . authinfo-color-mode)
  :init (advice-add 'authinfo-mode :override #'authinfo-color-mode))

(provide 'init-secure)
;;; init-secure.el ends here

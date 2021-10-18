;;; init-borg.el --- Bootstrap borg -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz@gmail.com>
;; Created: 2021-10-16 02:37:13

;;; Commentary:

;;; Code:

;; ------------------------- bootstrap -----------------------------

(setq borg-rewrite-urls-alist
      '(("git@github.com:" . "https://github.com/")
        ("git@gitlab.com:" . "https://gitlab.com/")))
(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

;; ------------------------ use-package ----------------------------

(eval-when-compile
  (require 'use-package))
(setq use-package-hook-name-suffix nil
      use-package-always-defer t)

;; ------------------------ auto-compile ---------------------------
(use-package auto-compile
  :hook (after-init-hook . auto-compile-on-load-mode)
  :init
  (setq auto-compile-visit-failed nil
        auto-compile-ding nil
        auto-compile-update-autoloads t
        auto-compile-use-mode-line nil))

(provide 'init-borg)
;;; init-borg.el ends here

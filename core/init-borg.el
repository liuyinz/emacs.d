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

;; --------------------------- leaf -------------------------------
;; NOTE
;; - `:defer-config' in `leaf' == `:config' in `use-package',
;; - `:config' needed to use with `:require'
(require 'leaf)

;; -------------------------- compile ------------------------------

(leaf comp
  :init
  (setq native-comp-speed 2
        native-comp-async-report-warnings-errors nil))

;; (leaf auto-compile
;;   :require t
;;   :init
;;   (setq auto-compile-visit-failed nil
;;         auto-compile-ding nil
;;         auto-compile-update-autoloads t
;;         auto-compile-use-mode-line nil)
;;   :config
;;   (auto-compile-on-load-mode))

(provide 'init-borg)
;;; init-borg.el ends here

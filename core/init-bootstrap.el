;;; init-bootstrap.el --- Bootstrap modules -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>

;;; Commentary:

;;; Code:

;; --------------------------- borg -------------------------------

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

;; native-comp
(when (featurep 'native-compile)
  ;; if t, auto compile elc to eln
  (setq native-comp-deferred-compilation nil))

(leaf comp
  :init
  (setq native-comp-speed 2
        native-comp-async-report-warnings-errors nil))

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here

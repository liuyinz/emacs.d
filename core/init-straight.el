;;; init-straight.el --- straight.el settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq straight-base-dir my-dir-cache)
(setq straight-vc-git-default-clone-depth 1)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" my-dir-cache))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-recipe-repositories '(melpa gnu-elpa-mirror org-elpa))

(straight-use-package 'use-package)
(setq straight-use-package-by-default nil)
;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)  ; ESSENTIAL for `straight.el'
  (setq use-package-always-defer t)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-compute-statistics nil)
  (setq use-package-hook-name-suffix nil)
  (setq use-package-enable-imenu-support t))

;; provides `straight-x-clean-unused-repos' (part of `straight.el')
(use-package straight-x)

(setq package-check-signature nil)
(use-package gnu-elpa-keyring-update
  :straight t
  :demand)

(provide 'init-straight)
;;; init-straight.el ends here

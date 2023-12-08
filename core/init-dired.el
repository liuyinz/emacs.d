;;; init-dired.el --- dired setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :bind
  (:dired-mode-map
   ("C-c C-p" . wdired-change-to-wdired-mode)
   ;; ("C-c C-z f" . browse-url-of-file)
   ("{" . dired-omit-mode)
   ("}" . dired-hide-details-mode)
   ("[" . dirvish-layout-switch)
   ("]" . dirvish-layout-toggle))
  :init
  (setq dired-free-space nil)
  (setq dired-kill-when-opening-new-dired-buffer t)
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  ;;   ;; Suppress the warning: `ls does not support --dired'.
  ;; (setq dired-use-ls-dired nil)
  ;; Use GNU ls as `gls' from `coreutils' if available.
  (when (executable-find "gls")
    (setq insert-directory-program "gls")
    (setq dired-listing-switches "-alh --group-directories-first"))

  ;; SEE http://www.nextpoint.se/?p=808
  (defun dired-mark-empty-directories (&optional arg)
    "Mark empty directories.
A prefix argument means to unmark them instead."
    (interactive "P")
    (let ((dired-marker-char (if arg ?\s dired-marker-char)))
      (dired-mark-if
       (when-let ((dir (dired-get-filename t t)))
         (directory-is-empty-p dir))
       "empty directory")))
  )

(leaf dired-x
  :hook (dired-mode-hook . dired-omit-mode)
  :defer-config
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

(leaf diredfl
  :hook
  (dired-mode-hook . diredfl-mode)
  (dirvish-directory-view-mode-hook . diredfl-mode)
  :defer-config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(leaf dirvish
  :hook (after-init-hook . dirvish-override-dired-mode)
  :bind
  ("C-x d" . dirvish)
  :init
  (setq dirvish-attributes
        '(subtree-state collapse file-size)
        dirvish-use-mode-line t
        dirvish-use-header-line nil)
  :defer-config

  (leaf dirvish-extras
    :init
    (setq dirvish-layout-recipes
          `((0 0 0.8)
            (0 0 0.5)
            ,dirvish-default-layout))
    ))

(provide 'init-dired)
;;; init-dired.el ends here

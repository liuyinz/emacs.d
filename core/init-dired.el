;;; init-dired.el --- dired setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf dired
  :hook (dired-mode-hook . dired-mode-setup)
  :bind
  (:dired-mode-map
   ("C-c C-p" . wdired-change-to-wdired-mode)
   ;; ("C-c C-z f" . browse-url-of-file)
   ("{" . dired-omit-mode)
   ("}" . dired-hide-details-mode))
  :init
  (setq dired-free-space nil
        dired-kill-when-opening-new-dired-buffer t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; Use GNU ls as `gls' from `coreutils' if available.
  (when (executable-find "gls")
    (setq dired-use-ls-dired t)
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

  (defun dired-mode-setup ()
    "Dired mode setup."
    (dired-omit-mode)
    (dired-hide-details-mode)
    (diredfl-mode))

  )

(leaf dired-x
  :defer-config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$")))

(leaf diredfl
  :defer-config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(leaf dirvish
  :hook (after-init-hook . dirvish-override-dired-mode)
  :bind
  ("C-x d" . dirvish)
  (:dired-mode-map
   ("["   . dirvish-layout-switch)
   ("]"   . dirvish-layout-toggle)
   ("TAB" . dirvish-toggle-subtree-anywhere))
  :init
  (setq dirvish-attributes
        '(subtree-state collapse file-size)
        dirvish-use-mode-line t
        dirvish-use-header-line nil
        dirvish-preview-dispatchers nil)

  (defun dirvish-toggle-subtree-anywhere ()
    "Toggle current directory subtree or parent directory."
    (interactive)
    (let ((dir-p (file-directory-p (dired-get-filename))))
      (if (and (not dir-p)
               (equal 0 (dirvish-subtree--depth)))
          (dired-up-directory)
        (and (not dir-p) (dirvish-subtree-up))
        (dirvish-subtree-toggle))))

  :defer-config

  ;; setup layout
  (setq dirvish-default-layout '(0 0 0.75))
  (setq dirvish-layout-recipes
        `((1 0.11 0.55)
          ;; HACK hide preview window as small as possible to imitate full-screen dired
          (0 0 0.02)
          ,dirvish-default-layout))

  (leaf dirvish-subtree
    :init
    (setq dirvish-subtree-listing-switches "-A"
          dirvish-subtree-always-show-state t
          dirvish-subtree-state-style 'arrow
          dirvish-subtree-prefix " ")))

(provide 'init-dired)
;;; init-dired.el ends here

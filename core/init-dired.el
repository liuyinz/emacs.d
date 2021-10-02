;;; init-dired.el --- dired setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;;   ;; Suppress the warning: `ls does not support --dired'.
  ;; (setq dired-use-ls-dired nil)

  ;; WORKAROUND compatiable with `emacs-version' < 28.1
  (if emacs/>=28.1p
      (setq dired-kill-when-opening-new-dired-buffer t)
    (put 'dired-find-alternate-file 'disabled nil))

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

(leaf dired-single
  :commands dired-single-buffer dired-single-buffer-mouse dired-single-up-directory)

;; (leaf dired-aux)

(leaf dired-x
  :hook (dired-mode-hook . dired-omit-mode)
  :config
  (let ((cmd "open"))
    (setq dired-guess-shell-alist-user
          '(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.cache*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

(leaf diredfl
  :hook (after-init-hook . diredfl-global-mode))

(leaf dired-git-info
  :commands dired-git-info-mode)

;; `find-dired' alternative using `fd'
(leaf fd-dired
  :when (executable-find "fd")
  :commands fd-dired)

(provide 'init-dired)
;;; init-dired.el ends here

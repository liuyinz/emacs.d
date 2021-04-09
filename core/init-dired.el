;;; init-dired.el --- dired setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :init
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  ;;   ;; Suppress the warning: `ls does not support --dired'.
  ;; (setq dired-use-ls-dired nil)
  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)

  (when (executable-find "gls")
    ;; Use GNU ls as `gls' from `coreutils' if available.
    (setq insert-directory-program "gls")
    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first")))

;; Extra Dired functionality
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

;; Colourful dired
(leaf diredfl
  :hook (after-init-hook . diredfl-global-mode))

;; Show git info in dired
(leaf dired-git-info
  :commands dired-git-info-mode)

;; ;; `find-dired' alternative using `fd'
(leaf fd-dired
  :when (executable-find "fd")
  :commands fd-dired)

(provide 'init-dired)
;;; init-dired.el ends here

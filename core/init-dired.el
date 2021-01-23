;;; init-dired.el --- dired setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :bind (:dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode)
         ("C-c C-z f" . browse-url-of-file))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)

  ;; (defadvice dired-find-file (around dired-find-file-single-buffer activate)
  ;;   "Replace current buffer if file is a directory."
  ;;   (interactive)
  ;;   (let ((orig (current-buffer))
  ;;         (filename (dired-get-file-for-visit)))
  ;;     (when (and (file-directory-p filename) (not (eq (current-buffer) orig))) (kill-buffer orig))))

  ;; Suppress the warning: `ls does not support --dired'.
  (setq dired-use-ls-dired nil)
  (when (executable-find "gls")
    ;; Use GNU ls as `gls' from `coreutils' if available.
    (setq insert-directory-program "gls")
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)
    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first")))

;; Extra Dired functionality
(leaf dired-aux)
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
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

;; Show git info in dired
(leaf dired-git-info
  :blackout
  :after
  :bind (:dired-mode-map
         (")" . dired-git-info-mode)))

;; Allow rsync from dired buffers
(leaf dired-rsync
  :bind (:dired-mode-map
         ("C-c C-r" . dired-rsync)))

;; Colourful dired
(leaf diredfl
  :blackout
  :hook (after-init-hook . diredfl-global-mode)
  ;; :config (diredfl-global-mode 1)
  )

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (leaf fd-dired))

(provide 'init-dired)
;;; init-dired.el ends here

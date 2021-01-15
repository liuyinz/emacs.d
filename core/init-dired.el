;;; init-dired.el --- dired setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
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
    (setq dired-listing-switches "-alh --group-directories-first")
    (use-package dired-quick-sort
      :bind (:map dired-mode-map
                  ("S" . hydra-dired-quick-sort/body))))

  ;; Show git info in dired
  (use-package dired-git-info
    :straight t
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :straight t
    :bind (:map dired-mode-map
                ("C-c C-r" . dired-rsync)))

  ;; Colourful dired
  (use-package diredfl
    :straight t
    :init (diredfl-global-mode 1))

  ;; Extra Dired functionality
  (use-package dired-aux)
  (use-package dired-x
    ;; :demand
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
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired :straight t))

(provide 'init-dired)
;;; init-dired.el ends here

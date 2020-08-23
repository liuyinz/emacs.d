(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
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
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
                ("C-c C-r" . dired-rsync)))

  ;; Colourful dired
  (use-package diredfl
    :init (diredfl-global-mode 1))

  ;; Shows icons
  ; (use-package all-the-icons-dired
  ;   :diminish
  ;   :hook (dired-mode . all-the-icons-dired-mode)
  ;   :config
    ;; FIXME: Refresh after creating or renaming the files/directories.
    ;; @see https://github.com/jtbm37/all-the-icons-dired/issues/34.
  ;   (with-no-warnings
  ;     (advice-add 'dired-do-create-files :around #'all-the-icons-dired--refresh-advice)
  ;     (advice-add 'dired-create-directory :around #'all-the-icons-dired--refresh-advice))
  ;
  ;   (with-no-warnings
  ;     (defun my-all-the-icons-dired--refresh ()
  ;       "Display the icons of files in a dired buffer."
  ;       (all-the-icons-dired--remove-all-overlays)
        ;; NOTE: don't display icons it too many items
  ;       (if (<= (count-lines (point-min) (point-max)) 1000)
  ;           (save-excursion
              ;; TRICK: Use TAB to align icons
  ;             (setq-local tab-width 1)
  ;
  ;             (goto-char (point-min))
  ;             (while (not (eobp))
  ;               (when-let ((file (dired-get-filename 'verbatim t)))
  ;                 (let ((icon (if (file-directory-p file)
  ;                                 (all-the-icons-icon-for-dir
  ;                                  file
  ;                                  :face 'all-the-icons-dired-dir-face
  ;                                  :height 0.9
  ;                                  :v-adjust all-the-icons-dired-v-adjust)
  ;                               (all-the-icons-icon-for-file
  ;                                file
  ;                                :height 0.9
  ;                                :v-adjust all-the-icons-dired-v-adjust))))
  ;                   (if (member file '("." ".."))
  ;                       (all-the-icons-dired--add-overlay (point) "  \t")
  ;                     (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))
  ;               (dired-next-line 1)))
  ;         (message "Not display icons because of too many items.")))
  ;     (advice-add #'all-the-icons-dired--refresh :override #'my-all-the-icons-dired--refresh)))
  ;
  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
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
  (use-package fd-dired))

(provide 'init-dired)

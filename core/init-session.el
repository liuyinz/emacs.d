;;; init-session.el --- session settings -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(leaf desktop
  ;; :hook (after-init-hook . desktop-save-mode)
  :init
  (setq desktop-auto-save-timeout 600
        desktop-restore-frames nil)
  (setq desktop-globals-to-save
        '((comint-input-ring        . 50)
          (compile-history          . 30)
          desktop-missing-file-warning
          (dired-regexp-history     . 20)
          (extended-command-history . 30)
          (face-name-history        . 20)
          (file-name-history        . 100)
          (grep-find-history        . 30)
          (grep-history             . 30)
          (magit-revision-history   . 50)
          (minibuffer-history       . 50)
          (org-clock-history        . 50)
          (org-refile-history       . 50)
          (org-tags-history         . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          register-alist
          (search-ring              . 20)
          (shell-command-history    . 50)
          tags-file-name
          tags-table-list))

  :config
  (defun my-desktop-time-restore (orig &rest args)
    "Count the restore time in total."
    (message "Desktop: %.2fms restored in TOTAL" (time-count! (apply orig args))))
  (advice-add 'desktop-read :around 'my-desktop-time-restore)

  (defun my-desktop-time-buffer-create (orig ver filename &rest args)
    "Count the buffer restored time."
    (message "Desktop: %.2fms to restore %s"
             (time-count! (apply orig ver filename args))
             (when filename
               (abbreviate-file-name filename))))
  (advice-add 'desktop-create-buffer :around 'my-desktop-time-buffer-create))

(provide 'init-session)

;;; init-session.el ends here

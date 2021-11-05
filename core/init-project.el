;;; init-project.el --- project setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf projectile
  :hook (after-init-hook . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t
        projectile-enable-caching t
        projectile-file-exists-remote-cache-expire nil
        projectile-require-project-root nil)
  ;; FIXME too slow while getting submodule files on Windows
  (setq projectile-git-submodule-command nil)

  :defer-config
  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; ignore some dirs
  (require 'f)
  (defun my/projectile-ignore-project (project-root)
    ;; exclude `Trash' directory
    (f-descendant-of? project-root "~/.Trash/"))
  (setq projectile-ignored-project-function #'my/projectile-ignore-project)
  )

(provide 'init-project)
;;; init-project.el ends here

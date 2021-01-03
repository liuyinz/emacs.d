(declare-function yas-activate-extra-mode 'yasnippet)

(use-package magit
  :commands (magit-status magit-dispatch magit-file-popup)
  :hook ((after-init . global-auto-revert-mode)
         (git-commit-mode . (lambda ()
                              (yas-activate-extra-mode 'git-commit-mode))))
  :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
         ("\\MERGE_MSG\\'" . text-mode))
  :init
  (setq magit-no-confirm t
        magit-save-repository-buffers 'dontask
        ;; vc-handled-backends (delq 'Git vc-handled-backends)
        ;; magit-auto-revert-mode t
        magit-auto-revert-immediately t))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
         ("B" . browse-at-remote)))

;; Git related modes
(use-package gitattributes-mode)
(use-package gitconfig-mode)

(use-package gitignore-mode)
(use-package gitignore-templates
  :commands gitignore-templates-insert gitignore-templates-new-file)

(use-package projectile
  :diminish
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-indexing-method 'alien
        projectile-mode-line-prefix ""
        projectile-use-git-grep t
        projectile-sort-order 'recently-active
        projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-project-search-path '("~/Code/repo/"))
  :config
  (setq projectile-generic-command
        (let ((rg-cmd ""))
          (dolist (dir projectile-globally-ignored-directories)
            (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
          (concat "rg -0 --files --color=never --hidden" rg-cmd))))

(provide 'init-vcs)

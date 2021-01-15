;;; init-vcs.el --- version control system -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function yas-activate-extra-mode 'yasnippet)

(use-package magit
  :straight t
  :commands (magit-status magit-dispatch magit-file-popup)
  :hook ((after-init-hook . global-auto-revert-mode)
         (git-commit-mode-hook . (lambda ()
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
  :straight t
  :bind (:map vc-prefix-map
         ("B" . browse-at-remote)))

;; Git related modes
(use-package gitattributes-mode :straight t)
(use-package gitconfig-mode :straight t)
(use-package gitignore-mode :straight t)

(use-package gitignore-templates
  :straight t
  :commands gitignore-templates-insert gitignore-templates-new-file)

(provide 'init-vcs)
;;; init-vcs.el ends here

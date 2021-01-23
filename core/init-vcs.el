;;; init-vcs.el --- version control system -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf magit
  :defun (yas-activateextra-mode . yasnippet)
  :commands magit-status magit-dispatch magit-file-popup
  :hook
  (after-init-hook . global-auto-revert-mode)
  (git-commit-mode-hook . (lambda ()
                            (yas-activate-extra-mode 'git-commit-mode)))
  :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
         ("\\MERGE_MSG\\'" . text-mode))
  :init
  (setq magit-no-confirm t
        magit-save-repository-buffers 'dontask
        ;; vc-handled-backends (delq 'Git vc-handled-backends)
        ;; magit-auto-revert-mode t
        magit-auto-revert-immediately t))

;; Open github/gitlab/bitbucket page
(leaf browse-at-remote
  :bind (:vc-prefix-map
         ("B" . browse-at-remote)))

;; Git related modes
(leaf gitattributes-mode)
(leaf gitconfig-mode)
(leaf gitignore-mode)

(leaf gitignore-templates
  :commands gitignore-templates-insert gitignore-templates-new-file)

(provide 'init-vcs)
;;; init-vcs.el ends here

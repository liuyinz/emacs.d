;;; init-vcs.el --- version control system -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf magit
  :doc "deps: with-editor forge transient git-modes ghub"
  :defun (yas-activate-extra-mode . yasnippet)
  :commands (magit-status magit-dispatch magit-submodule)
  :hook (git-commit-mode-hook . (lambda () (yas-activate-extra-mode 'git-commit-mode)))
  :mode
  ("\\COMMIT_EDITMSG\\'" . text-mode)
  ("\\MERGE_MSG\\'" . text-mode)
  :init
  (setq magit-no-confirm t
        magit-save-repository-buffers 'dontask
        ;; vc-handled-backends (delq 'Git vc-handled-backends)
        ;; magit-auto-revert-mode t
        magit-auto-revert-immediately t)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

;; ;; Git related modes
;; (leaf git-modes :require t)

(leaf gitignore-templates
  :commands gitignore-templates-insert gitignore-templates-new-file
  :init (setq gitignore-templates-api 'github))

;; Open github/gitlab/bitbucket page
(leaf browse-at-remote :commands browse-at-remote)

(provide 'init-vcs)
;;; init-vcs.el ends here

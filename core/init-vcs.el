;;; init-vcs.el --- version control system -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf magit
  :doc "deps: with-editor forge transient git-modes ghub"
  :require t
  ;; :commands magit-status magit-submodule magit-dispatch magit-log
  :init
  (setq magit-no-confirm t
        magit-save-repository-buffers 'dontask
        magit-auto-revert-immediately t
        magit-submodule-remove-trash-gitdirs t)
  :defer-config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(leaf gitignore-templates
  :commands gitignore-templates-insert gitignore-templates-new-file
  :init (setq gitignore-templates-api 'github))

(leaf git-modes :require t)

;; Open github/gitlab/bitbucket page
(leaf browse-at-remote :commands browse-at-remote)

(provide 'init-vcs)
;;; init-vcs.el ends here

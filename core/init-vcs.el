;;; init-vcs.el --- version control system -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf git-modes
  :mode
  ("\\.\\(rg\\|docker\\)ignore\\'" . gitignore-mode)
  ("\\.gitconfig\\.local\\'" . gitconfig-mode))

(leaf gist
  :defer-config
  (setq gist-ask-for-description t)
  (setq gist-list-format
        '((id "Id" 7 nil identity)
          (created "Created" 15 nil "%y-%m-%d %R")
          (visibility "Visibility" 10 nil
                      (lambda (public)
                        (or (and public "public")
                            "private")))
          (description "Description" 0 nil identity)))

  (let ((info (auth-source-user-and-password "api.github.com" "liuyinz^gist")))
    (setq gh-profile-alist
          `(("github"
             :username "liuyinz"
             :password nil
             :token   ,(cadr info)
             :url "https://api.github.com"
             :remote-regexp
             ,(gh-profile-remote-regexp "github.com"))))))

(leaf vc-msg
  :init
  (setq vc-msg-show-at-line-beginning-p nil
        vc-msg-newbie-friendly-msg ""))

(leaf conventional-changelog
  :init
  ;; Integrate to `magit-tag'
  (with-eval-after-load 'magit-tag
    (transient-append-suffix 'magit-tag
      '(1 0 -1)
      '("c" "changelog" conventional-changelog-menu))))

(leaf git-commit-insert-issue
  :hook (git-commit-mode-hook . git-commit-insert-issue-mode))

(leaf gitignore-templates
  :init
  (setq gitignore-templates-api 'github)

  ;; Integrate with `magit-gitignore'
  (with-eval-after-load 'magit-gitignore
    (require 'gitignore-templates nil t)
    (transient-append-suffix 'magit-gitignore '(0)
      ["Template"
       ("n" "new file" gitignore-templates-new-file)
       ("i" "select pattern" gitignore-templates-insert)])))

(leaf magit
  :init
  (setq magit-no-confirm t
        magit-save-repository-buffers 'dontask
        magit-auto-revert-immediately t
        magit-submodule-remove-trash-gitdirs t
        ;; SEE https://magit.vc/manual/magit/Diff-Options.html
        ;; magit-diff-refine-hunk nil
        magit-diff-paint-whitespace-lines 'all
        magit-fetch-modules-jobs 7)
  :bind
  ((kbd "C-c l") . magit-dispatch)

  :defer-config
  (prependq! magit-section-initial-visibility-alist
             '((untracked . hide)
               (unpushed  . show)))

  ;; ------------------------- submodul -----------------------------

  ;; add module in `magit-status'
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-untracked-files)

  ;; HACK ignore submodules in magit-status when there is too many submodules.
  (defvar magit-status-submodule-max 20
    "Maximum number of submodules that will be not ignored in `magit-status'.")
  (defun ad/ignore-submodules-more-than-max (orig-fn &rest args)
    (let ((default-directory (magit-toplevel)))
      (if (< magit-status-submodule-max (length (magit-list-module-paths)))
          ;; SEE https://emacs.stackexchange.com/a/57594/35676
          (cl-letf (((get 'magit-status-mode 'magit-diff-default-arguments)
                     (cl-pushnew
                      "--ignore-submodules=all"
                      (get 'magit-status-mode 'magit-diff-default-arguments))))
            (apply orig-fn args))
        (apply orig-fn args))))
  (advice-add 'magit-diff--get-value :around #'ad/ignore-submodules-more-than-max)

  )

(provide 'init-vcs)
;;; init-vcs.el ends here

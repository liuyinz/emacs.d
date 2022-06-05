;;; init-vcs.el --- version control system -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf git-modes
  :mode
  ("\\.\\(rg\\|docker\\)ignore\\'" . gitignore-mode)
  ("\\.gitconfig\\'" . gitconfig-mode))

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
  :bind
  ((kbd "C-c l") . magit-dispatch)
  ((kbd "C-c f") . magit-file-dispatch)

  :defer-config
  (setq magit-slow-confirm nil)
  (setq magit-auto-revert-immediately t)
  (setq magit-save-repository-buffers 'dontask
        magit-bury-buffer-function #'magit-restore-window-configuration)

  ;; Display fullframe buffer in some magit-*-modes
  (defvar magit-modes-display-fullframe-selected
    '(magit-log-mode magit-reflog-mode magit-submodule-list-mode)
    "List of modes related to magit to display in fullframe.")
  (defun my/magit-display-buffer-fullframe-selected (buffer)
    "Display BUFFER, filling entier frame if BUFFER's major mode included in
 `magit-modes-display-fullframe-selected'.
Otherwise, behave like `magit-display-buffer-traditional'."
    (if (member (buffer-local-value 'major-mode buffer)
                magit-modes-display-fullframe-selected)
        (display-buffer buffer '(magit--display-buffer-fullframe))
      (magit-display-buffer-traditional buffer)))
  (setq magit-display-buffer-function #'my/magit-display-buffer-fullframe-selected)

  ;; -------------------------- section ------------------------------

  (prependq! magit-section-initial-visibility-alist
             '((untracked . hide)
               (unpushed  . show)))

  ;; -------------------------- commit ------------------------------

  (setq magit-commit-reword-override-date nil)

  ;; Set to meow-insert-mode automatically
  (add-hook 'git-commit-setup-hook #'meow-insert-mode)

  ;; ------------------------- submodule ----------------------------

  (setq magit-submodule-remove-trash-gitdirs t)

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

  ;; ---------------------------- log --------------------------------

  ;; Predefined log command arguments
  (with-eval-after-load 'magit-log
    (put 'magit-log-mode 'magit-log-default-arguments
         '("--graph" "-n256" "--decorate" "--color")))

  ;; --------------------------- diff -------------------------------

  ;; SEE https://magit.vc/manual/magit/Diff-Options.html
  (setq magit-diff-refine-hunk 'all
        magit-diff-refine-ignore-whitespace t
        magit-diff-paint-whitespace-lines 'all)

  ;; --------------------------- blame -------------------------------

  (add-hook 'magit-blame-mode-hook #'my/meow-motion-temporary)
  )

(provide 'init-vcs)
;;; init-vcs.el ends here

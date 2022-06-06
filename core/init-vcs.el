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

  ;; -------------------------- status ------------------------------

  (prependq! magit-section-initial-visibility-alist
             '((untracked . hide)
               (unpushed  . show)))

  ;; add module in `magit-status'
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-untracked-files)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-stashes
                          'magit-insert-untracked-files)

  ;; Predefined status command arguments
  (with-eval-after-load 'magit-status
    (put 'magit-status-mode 'magit-diff-default-arguments
         '("--no-ext-diff" "--ignore-submodules=all")))

  ;; -------------------------- commit ------------------------------

  (setq magit-commit-reword-override-date nil)

  ;; Set to meow-insert-mode automatically
  (add-hook 'git-commit-setup-hook #'meow-insert-mode)

  ;; ------------------------- submodule ----------------------------

  (setq magit-submodule-remove-trash-gitdirs t)

  ;; ---------------------------- log --------------------------------

  ;; Predefined log command arguments
  (with-eval-after-load 'magit-log
    (put 'magit-log-mode 'magit-log-default-arguments
         '("--graph" "-n256" "--decorate" "--color")))

  ;; ;; preview blob buffer in magit-log-mode
  ;; (add-hook 'magit-section-movement-hook 'magit-log-maybe-update-blob-buffer)
  ;; (keymap-set magit-log-mode-map "C-<return>" magit-blog-show-or-scroll)
  ;; (defun magit-blob-show-or-scroll (args)
  ;;   "docstring"
  ;;   (interactive)
  ;;   )

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

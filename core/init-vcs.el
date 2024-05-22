;;; init-vcs.el --- version control system -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf vc
  :init
  (setq vc-display-status 'no-backend)
  (setq vc-follow-symlinks t))

(leaf git-modes
  :mode
  ("\\.\\(rg\\|docker\\)ignore\\'" . gitignore-mode)
  ("\\.gitconfig\\'" . gitconfig-mode))

(leaf vc-msg
  :init
  (setq vc-msg-show-at-line-beginning-p nil
        vc-msg-newbie-friendly-msg ""))

(leaf git-cliff
  :init
  (setq git-cliff-extra-dir (expand-file-name "git-cliff/" my/dir-ext))
  ;; Integrate to `magit-tag'
  (with-eval-after-load 'magit-tag
    (transient-append-suffix 'magit-tag
      '(1 0 -1)
      '("c" "changelog" git-cliff-menu))))

(leaf gitignore-templates
  :init
  (setq gitignore-templates-api 'gitignore.io)
  ;; Integrate with `magit-gitignore'
  (with-eval-after-load 'magit-gitignore
    (require 'gitignore-templates nil t)
    (transient-append-suffix 'magit-gitignore '(0)
      ["Template"
       ("n" "new file" gitignore-templates-new-file)
       ("i" "insert pattern" gitignore-templates-insert)])))

(leaf git-link
  :init
  (setq git-link-use-commit t)
  (defun ad/git-link-to-system-clip (&rest _)
    "Copy to git link to system clip rather than kill-ring."
    (when (and kill-ring
               (fboundp #'simpleclip-set-contents))
      (simpleclip-set-contents
       (substring-no-properties (pop kill-ring)))))
  (advice-add 'git-link--new :after #'ad/git-link-to-system-clip)
  ;; Integrate with `magit-tool'
  )

(leaf browse-at-remote)

(leaf smerge-mode
  :hook (smerge-mode-hook . my/smerge-setup)
  :bind
  (:smerge-basic-map
   ;; move
   ("n" . smerge-vc-next-conflict)
   ("f" . smerge-first)
   ("l" . smerge-last)
   ("x" . smerge-next-file)
   ;; choose
   ("a" . smerge-keep-all)
   ("m" . smerge-keep-upper)
   ("b" . smerge-keep-base)
   ("o" . smerge-keep-lower)
   ("c" . smerge-keep-current)
   ;; diff
   ("SPC" . smerge-conflict-preview-or-scroll)
   ("<"   . smerge-diff-base-upper)
   (">"   . smerge-diff-base-lower)
   ("="   . smerge-diff-upper-lower)
   ("e"   . smerge-ediff)
   ("R"   . smerge-refine)
   ;; edit
   ("s"   . smerge-swap)
   ("C"   . smerge-combine-with-next)
   ("r"   . smerge-resolve)
   ("C-r" . smerge-resolve-all)
   ("u"   . undo)
   ("U"   . undo-redo))
  :init
  (setq smerge-change-buffer-confirm nil
        ;; smerge-command-prefix ""
        smerge-refine-ignore-whitespace nil)

  (defun my/smerge-setup ()
    ;; (my/transient-smerge)
    ;; make sure call `smerge-first' after disable `save-place-local-mode'
    ;; see `add-hook' doc about order
    (smerge-first))

  (defun smerge-first ()
    "Jump to first conflict in the buffer."
    (interactive)
    (goto-char (point-min))
    (unless (looking-at smerge-begin-re)
      (smerge-next)))

  (defun smerge-last ()
    "Jump to first conflict in the buffer."
    (interactive)
    (goto-char (point-max))
    (smerge-prev))

  (defun smerge-conflict-preview-or-scroll ()
    "Preview or scorll conflict region."
    (interactive)
    (smerge-match-conflict)
    (let* ((rev (match-beginning 0))
           (buf (get-buffer "*smerge-preview*"))
           win)
      (unless (and buf (equal rev (buffer-local-value 'orig-rev buf)))
        (copy-to-buffer "*smerge-preview*" (match-beginning 0) (match-end 0))
        ;; SEE https://emacs.stackexchange.com/a/32817
        ;; (with-current-buffer "*smerge-preview*"
        ;;   (set (make-local-variable 'orig-rev) rev))
        (setf (buffer-local-value 'orig-rev buf) rev))
      (if (setq win (get-buffer-window buf))
          (with-selected-window win
            (condition-case nil
                (scroll-up)
              (error
               (goto-char (point-min)))))
        (display-buffer buf nil))))

  (defun smerge-next-file ()
    "Jump to next conflicted file."
    (interactive)
    (vc-find-conflicted-file)
    (smerge-first)
    (smerge-refine 2)))

(leaf magit
  :defer-config
  (setq magit-slow-confirm nil)
  (setq magit-auto-revert-immediately t)
  (setq magit-save-repository-buffers 'dontask
        magit-bury-buffer-function #'magit-restore-window-configuration)
  (setq magit-show-long-lines-warning nil)

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

  
  ;;; status

  (prependq! magit-section-initial-visibility-alist
             '((untracked . hide)
               (unpushed  . show)))

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          nil t)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-stashes
                          'magit-insert-untracked-files)

  ;; Predefined status command arguments
  (with-eval-after-load 'magit-status
    (put 'magit-status-mode 'magit-diff-default-arguments
         '("--no-ext-diff" "--ignore-submodules=all")))

  ;; TODO when move to any staged/unstaged section, close other siblings files
  ;; ;; expand section when jump to it
  ;; (defun my/magit-section-ensure-visible (section)
  ;;   (when (oref section hidden)
  ;;     (magit-section-show section)))
  ;; (add-hook 'magit-section-movement-hook #'my/magit-section-ensure-visible)

  
  ;;; commit

  (setq magit-commit-reword-override-date nil
        magit-commit-show-diff nil)

  ;; Set to meow-insert-mode automatically
  (add-hook 'git-commit-setup-hook #'meow-insert-mode)

  
  ;;; submodule

  (setq magit-submodule-remove-trash-gitdirs t)

  
  ;;; tag

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

  
  ;;; diff

  ;; SEE https://magit.vc/manual/magit/Diff-Options.html
  (setq magit-diff-refine-hunk 'all
        magit-diff-refine-ignore-whitespace t
        magit-diff-paint-whitespace-lines 'all)

  
  ;;; blame

  (add-hook 'magit-blame-mode-hook #'my/meow-motion-temporary))

(provide 'init-vcs)
;;; init-vcs.el ends here

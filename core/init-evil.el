;;; init-evil.el --- evil setting  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (declare-function projectile-project-p 'projectile)

;; (defun my-search-all ()
;;   "Search with Rg in project or directory"
;;   (interactive)
;;   (if (projectile-project-p)
;;       (counsel-projectile-rg)
;;     (counsel-rg)))

(use-package general
  :straight t
  :demand
  :config
  (general-evil-setup t)

  (general-create-definer my-semicolon-leader-def
    :prefix ";"
    :states 'normal)

  (my-semicolon-leader-def
    ;; misc
    ";" 'counsel-M-x
    "c" 'counsel-counsel
    "i" 'counsel-imenu
    "f" 'counsel-find-file
    "y" 'counsel-yank-pop
    ;; yasnippet
    "ss" 'yas-visit-snippet-file
    "sn" 'yas-new-snippet
    ;; search and replace
    "sa" 'my-search-all
    "rr" 'color-rg-search-input-in-current-file
    "ra" 'color-rg-search-input-in-project
    ;; "rt" 'color-rg-search-project-with-type
    ;; "fg" 'counsel-git
    "d" 'dired-jump-other-window
    ;; buffer
    "b" 'ibuffer-other-window
    "w" 'ivy-switch-buffer
    "u" 'ivy-resume
    ;; magit
    "gg" 'magit-status
    "gl" 'magit-log-all-branches
    )

  (general-create-definer my-comma-leader-def
    :prefix ","
    :states '(normal visual))

  (my-comma-leader-def
    "c" 'comment-dwim-2
    "r" 'my-run
    "i" 'my-repl
    "f" 'my-format
    )

  ;; Define for general
  (general-create-definer my-space-leader-def
    :prefix "SPC"
    :states '(normal visual))

  (my-space-leader-def
    ;;jump
    "c" 'avy-goto-char
    "w" 'avy-goto-word-0
    "l" 'avy-goto-line
    "r" 'avy-resume
    ;; "f" 'avy-goto-paren
    ;;window
    "x" 'delete-window
    "o" 'delete-other-windows
    "h" 'split-window-horizontally
    "v" 'split-window-vertically
    "q" 'delete-frame
    ;;buffer
    "s" 'save-buffer
    "k" 'kill-this-buffer
    "a" 'evil-write-all
    ;;toggle
    "tu" 'undo-tree-visualize
    "tf" 'flycheck-list-errors
    "tq" 'toggle-keyfreq
    "td" 'toggle-debug-on-error
    ))

(use-package evil
  :straight t
  :functions (evil-define-key*
               evil-delay
               evil-set-initial-state)
  :hook (after-init-hook . evil-mode)
  ;; :init (setq evil-want-keybinding nil)
  :config
  (setq evil-want-Y-yank-to-eol t
        evil-cross-lines t
        evil-move-beyond-eol t
        evil-echo-state nil)

  (with-eval-after-load 'evil-maps
    ;; replace C-r with U for undo-tree map
    (define-key evil-normal-state-map "\C-r" nil)
    (define-key evil-normal-state-map "U" 'redo)
    ;; unbind n and N
    (define-key evil-motion-state-map "n" nil)
    (define-key evil-motion-state-map "N" nil)
    (define-key evil-motion-state-map ";" nil)
    (define-key evil-insert-state-map "\C-t" nil)
    (define-key evil-insert-state-map "\C-n" 'evil-open-below)
    (define-key evil-insert-state-map "\C-p" 'evil-open-above)
    )

  ;;evil binding
  (evil-define-key 'normal 'global
    ;; select the previously pasted text
    "gp" "`[v`]"
    ;; run the macro in the q register
    "Q" "@q"
    ;; enable jump to (
    "n" "f("
    "N" "F("
    "gs" 'dash-at-point
    )

  (evil-define-key 'normal origami-mode-map
    ;; "za" 'origami-forward-toggle-node
    "zm" 'origami-toggle-all-nodes
    ;; "zp" 'origami-previous-fold
    ;; "zn" 'origami-forward-fold
    ;; "zc" 'origami-recursively-toggle-node
    ;; "zo" 'origami-show-node
    ;; "zr" 'origami-redo
    ;; "zu" 'origami-undo
    )

  (evil-define-key 'normal flycheck-mode-map
    "]f" 'flycheck-next-error
    "[f" 'flycheck-previous-error)

  (evil-define-key 'emacs flycheck-error-list-mode-map
    "n" 'flycheck-error-list-next-error
    "p" 'flycheck-error-list-previous-error
    "c" 'flycheck-error-list-check-source
    "s" 'flycheck-error-list-set-filter
    "S" 'flycheck-error-list-reset-filter
    "x" 'flycheck-error-list-explain-error
    (kbd "RET") 'flycheck-error-list-goto-error
    "q" 'quit-window)

  (evil-define-key 'normal diff-hl-mode-map
    "[g" 'diff-hl-previous-hunk
    "]g" 'diff-hl-next-hunk
    "gh" 'diff-hl-revert-hunk)

  (evil-define-key 'emacs vterm-mode-map
    "\M-n" 'vterm-toggle-insert-cd)

  (evil-define-key '(normal emacs insert) 'global
    "\M-p" 'vterm-toggle)

  (evil-define-key 'normal smerge-mode-map
    ;; move
    "n" 'smerge-next
    "p" 'smerge-prev
    ;; keep
    "a" 'smerge-keep-all
    "b" 'smerge-keep-base
    "l" 'smerge-keep-lower
    "u" 'smerge-keep-upper
    "\C-m" 'smerge-keep-current
    "RET" 'smerge-keep-current
    ;; diff
    "<" 'smerge-diff-base-upper
    "=" 'smerge-diff-upper-lower
    ">" 'smerge-diff-base-lower
    "R" 'smerge-refine
    "E" 'smerge-ediff
    ;; other
    "C" 'smerge-combine-with-next
    "r" 'smerge-resolve
    "k" 'smerge-kill-current
    "ZZ" (lambda ()
           (interactive)
           (save-buffer)
           (bury-buffer)))

  ;;set evil initial state
  (dolist (p '((anaconda-nav-mode . emacs)
               (calendar-mode . emacs)
               (compilation-mode . emacs)
               (color-rg-mode . emacs)
               (dired-mode . emacs)
               (erc-mode . emacs)
               (eshell-mode . emacs)
               (epa-key-list-mode . emacs)
               (ffip-file-mode . emacs)
               ;; (flycheck-error-list-mode . normal)
               (grep-mode . emacs)
               (git-commit-mode . insert)
               (gud-mode . emacs)
               (help-mode . emacs)
               (ivy-occur-mode . emacs)
               (ivy-occur-grep-mode . normal)
               (Info-mode . emacs)
               (log-edit-mode . emacs)
               (js2-error-buffer-mode . emacs)
               (minibuffer-inactive-mode . emacs)
               (magit-log-edit-mode . emacs)
               (message-mode . emacs)
               (messages-buffer-mode . emacs)
               (neotree-mode . emacs)
               (profiler-report-mode . emacs)
               (quickrun-mode . emacs)
               (sr-mode . emacs)
               (shell-mode . emacs)
               (speedbar-mode . emacs)
               (special-mode . emacs)
               (sdcv-mode . emacs)
               (term-mode . emacs)
               (undo-tree-visualizer-selection-mode . emacs)
               (undo-tree-visualizer-mode . emacs)
               (vc-log-edit-mode . emacs)
               (vterm-mode . emacs)
               (w3m-mode . emacs)
               (weibo-timeline-mode . emacs)
               (weibo-post-mode . emacs)
               (woman-mode . emacs)
               (xref--xref-buffer-mode . emacs)
               ))
    (evil-set-initial-state (car p) (cdr p)))

  ;; ;;input-switch
  ;; (use-package fcitx
  ;;   :demand
  ;;   :config
  ;;   (when (executable-find "fcitx-remote")
  ;;     (fcitx-aggressive-setup)))
  )

;; (use-package evil-collection
;;   :after evil
;;   :defines evil-collection-mode-list
;;   :demand
;;   :init
;;   (setq evil-collection-mode-list '(flycheck))
;;   :config
;;   (evil-collection-init))

(use-package evil-terminal-cursor-changer
  ;; :demand
  :after evil
  ;; :unless (window-system)
  ;; :disabled
  :init
  (setq evil-motion-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-emacs-state-cursor  'box)
  :config
  (etcc-on))


;; (use-package evil-magit
;;   :after evil magit)

(use-package evil-escape
  :hook (after-init . evil-escape-mode)
  :config
  (setq-default evil-escape-delay 0.3
                evil-escape-key-sequence "kj")
  (setq evil-escape-excluded-major-modes '(dired-mode)))

;;Evil-matchit
(use-package evil-matchit
  :hook (after-init . global-evil-matchit-mode)
  :config
  ;;use "m" rather than "%"
  (setq evilmi-shortcut "m"))

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode)
  :config
  (add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup)
  (defun evil-surround-prog-mode-hook-setup ()
    (push '(?$ . ("${" . "}")) evil-surround-pairs-alist)
    (push '(?/ . ("/" . "/")) evil-surround-pairs-alist))

  (add-hook 'js-mode-hook 'evil-surround-js-mode-hook-setup)
  (defun evil-surround-js-mode-hook-setup ()
    ;; ES6
    (push '(?> . ("(e) => " . "(e)")) evil-surround-pairs-alist))

  (add-hook 'emacs-lisp-mode-hook 'evil-surround-emacs-lisp-mode-hook-setup)
  (defun evil-surround-emacs-lisp-mode-hook-setup ()
    (push '(?\( . ("( " . ")")) evil-surround-pairs-alist)
    (push '(?` . ("`" . "'")) evil-surround-pairs-alist))

  (add-hook 'org-mode-hook 'evil-surround-org-mode-hook-setup)
  (defun evil-surround-org-mode-hook-setup ()
    (push '(93 . ("[[" . "]]")) evil-surround-pairs-alist)
    (push '(?= . ("=" . "=")) evil-surround-pairs-alist)))

;; (use-package evil-nerd-commenter
;;   :after evil
;;   :init
;;   (evilnc-default-hotkeys nil t))

(provide 'init-evil)
;;; init-evil.el ends here

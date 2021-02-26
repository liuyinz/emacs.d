;;; init-evil.el --- evil setting  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; (declare-function projectile-project-p 'projectile)

;; (defun my-search-all ()
;;   "Search with Rg in project or directory"
;;   (interactive)
;;   (if (projectile-project-p)
;;       (counsel-projectile-rg)
;;     (counsel-rg)))

;;   (my-semicolon-leader-def
;;     ;; misc
;;     ";" 'counsel-M-x
;;     "c" 'counsel-counsel
;;     "i" 'counsel-imenu
;;     "f" 'counsel-find-file
;;     "y" 'counsel-yank-pop
;;     ;; yasnippet
;;     "ss" 'yas-visit-snippet-file
;;     "sn" 'yas-new-snippet
;;     ;; search and replace
;;     "sa" 'my-search-all
;;     ;; "fg" 'counsel-git
;;     "d" 'dired-jump-other-window
;;     ;; buffer
;;     "b" 'ibuffer-other-window
;;     ;; magit
;;     "gg" 'magit-status
;;     "gl" 'magit-log-all-branches
;;     )

;;   ;; Define for general
;;   (general-create-definer my-space-leader-def
;;     :prefix "SPC"
;;     :states '(normal visual))

;;   (my-space-leader-def
;;     ;;window
;;     "x" 'delete-window
;;     "o" 'delete-other-windows
;;     "h" 'split-window-horizontally
;;     "v" 'split-window-vertically
;;     "q" 'delete-frame

;;     ;;buffer
;;     "s" 'save-buffer
;;     "k" 'kill-this-buffer
;;     "a" 'evil-write-all
;;     ;;toggle
;;     "tu" 'undo-tree-visualize
;;     "tf" 'flycheck-list-errors
;;     "tq" 'toggle-keyfreq
;;     "td" 'toggle-debug-on-error
;;     ))

(leaf evil
  :doc "deps: goto-chg"
  :defun evil-define-key* evil-delay evil-set-initial-state
  :hook (after-init-hook . evil-mode)
  :init
  (setq evil-want-Y-yank-to-eol t
        evil-want-C-i-jump nil
        evil-disable-insert-state-bindings t
        evil-move-cursor-back nil
        evil-move-beyond-eol t
        evil-cross-lines t
        evil-track-eol nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-fu
        evil-echo-state nil
        evil-want-integration t
        evil-ex-substitute-global t)
  :config

  ;; set leader and localleader
  (evil-set-leader '(normal visual) ";")
  (evil-set-leader '(normal visual) "," t)

  ;; hl-line
  (add-hook 'evil-normal-state-entry-hook #'hl-line-mode)
  (add-hook 'evil-emacs-state-exit-hook #'hl-line-mode)
  (add-hook 'evil-insert-state-entry-hook (lambda () (hl-line-mode -1)))
  (add-hook 'evil-emacs-state-entry-hook (lambda () (hl-line-mode -1)))

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

;;Evil-matchit
(leaf evil-matchit
  :after evil
  :blackout
  :hook (evil-mode-hook . global-evil-matchit-mode)
  :config
  ;;use "m" rather than "%"
  (setq evilmi-shortcut "m"))

(leaf evil-surround
  :after evil
  :blackout
  :hook
  (evil-mode-hook . global-evil-surround-mode)
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

(leaf evil-nerd-commenter
  :after evil
  :commands evilnc-comment-or-uncomment-lines evilnc-copy-and-comment-lines)

(leaf anzu
  :blackout t
  :hook (evil-mode-hook . global-anzu-mode)
  :config
  (leaf evil-anzu :require t))

(leaf sis
  :doc "deps : brew install macism"
  :blackout t
  :hook (evil-mode-hook . sis-global-respect-mode))

(provide 'init-evil)
;;; init-evil.el ends here

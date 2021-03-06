;;; init-evil.el --- evil setting  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf evil
  :doc "deps: goto-chg"
  :defun evil-define-key* evil-delay evil-set-initial-state
  :hook
  (after-init-hook . evil-mode)
  ;; hl-line
  ((evil-normal-state-entry-hook
    evil-emacs-state-exit-hook) . hl-line-mode)
  ((evil-insert-state-entry-hook
    evil-emacs-state-entry-hook
    evil-visual-state-entry-hook) . (lambda () (hl-line-mode -1)))
  :init
  (setq evil-want-Y-yank-to-eol t
        evil-want-C-i-jump nil
        evil-disable-insert-state-bindings t
        evil-move-cursor-back nil
        evil-move-beyond-eol t
        evil-cross-lines t
        evil-track-eol nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo
        evil-echo-state nil
        evil-ex-substitute-global t
        ;; ;; with evil-collection
        ;; evil-want-integration t
        ;; evil-want-keybinding nil
        )
  :config

  ;; HACK integration with vundo
  (defun evil-vundo-undo ()
    "Hybrid evil and vundo."
    (interactive)
    (if (not (fboundp 'vundo))
        (evil-undo 1)
      (vundo)
      (vundo-backward 1)))

  (defun evil-vundo-redo ()
    "Hybrid evil and vundo."
    (interactive)
    (if (not (fboundp 'vundo))
        (evil-redo 1)
      (vundo)
      (vundo-forward 1)))

  (defun evil-substitute-normal ()
    "Call evil-ex-substitute in normal-state."
    (interactive)
    (if (evil-normal-state-p)
        (evil-ex "%s/")
      (evil-ex "'<,'>s/")))

  ;; (leaf evil-collection
  ;;   :doc "deps : annalist evil"
  ;;   :after evil
  ;;   :require t
  ;;   :init
  ;;   ;; (setq evil-collection-mode-list '(magit))
  ;;   :config
  ;;   (evil-collection-init 'flycheck))

  (leaf goto-chg :require t)

  ;; set leader and localleader
  (evil-set-leader '(normal visual) ";")
  (evil-set-leader '(normal visual) "," t)

  ;;set evil initial state
  (appendq! evil-buffer-regexps '(("\\`\\*Emacs Log\\*\\'" . emacs)))

  (dolist (p '((anaconda-nav-mode . emacs)
               (benchmark-init/tabulated-mode . normal)
               (benchmark-init/tree-mode . normal)
               (calendar-mode . emacs)
               (compilation-mode . emacs)
               (dired-mode . emacs)
               (erc-mode . emacs)
               (eshell-mode . emacs)
               (epa-key-list-mode . emacs)
               (ffip-file-mode . emacs)
               ;; (flycheck-error-list-mode . normal)
               (grep-mode . normal)
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
    (evil-set-initial-state (car p) (cdr p))))

;;Evil-matchit
(leaf evil-matchit
  :hook (evil-mode-hook . global-evil-matchit-mode))

(leaf evil-surround
  :hook (evil-mode-hook . global-evil-surround-mode)
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
  :commands evilnc-comment-or-uncomment-lines evilnc-copy-and-comment-lines)

;; Pinyin support
(leaf evil-find-char-pinyin
  :doc "deps: evil pinyinlib"
  :hook (evil-mode-hook . evil-find-char-pinyin-mode))

;; FIXME don't compatible with kitty, conflict with company-mode
(leaf evil-terminal-cursor-changer
  :hook (after-make-console-frame-hook . etcc-mode))

(provide 'init-evil)
;;; init-evil.el ends here

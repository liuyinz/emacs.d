;;; init-evil.el --- evil setting  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :hook ((after-init-hook . evil-mode)
         (evil-normal-state-entry-hook . hl-line-mode)
         (evil-normal-state-exit-hook . (lambda () (hl-line-mode -1))))
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
        ;; evil-ex-search-persistent-highlight nil
        evil-repeat-move-cursor nil
        ;; evil-search-module 'evil-search
        ;; ;; with evil-collection
        ;; evil-want-integration t
        ;; evil-want-keybinding nil
        )
  :config

  ;; ISSUE https://github.com/casouri/vundo/issues/6#issuecomment-886220526
  (defun ad/evil-with-vundo (fn &rest _)
    "Integretion evil and vundo."
    (if (not (fboundp 'vundo))
        (funcall fn 1)
      (vundo)
      (if (eq this-command 'evil-redo)
          (vundo-forward 1)
        (vundo-backward 1))))
  (advice-add 'evil-undo :around #'ad/evil-with-vundo)
  (advice-add 'evil-redo :around #'ad/evil-with-vundo)

  (defun evil-substitute-normal ()
    "Call evil-ex-substitute in normal-state."
    (interactive)
    (if (evil-normal-state-p)
        (evil-ex "%s/")
      (evil-ex "'<,'>s/")))

  ;; (use-package evil-collection
  ;;   :after evil
  ;;   :require t
  ;;   :init
  ;;   ;; (setq evil-collection-mode-list '(magit))
  ;;   :config
  ;;   (evil-collection-init 'flycheck)))

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

(use-package sis
  :hook (evil-mode-hook . sis-global-respect-mode))

;; SEE https://github.com/redguardtoo/evil-matchit
(use-package evil-matchit
  :hook (evil-mode-hook . global-evil-matchit-mode))

(use-package evil-surround
  :hook (evil-mode-hook . global-evil-surround-mode)
  :config

  ;; add more for prog-mode
  (add-hook 'prog-mode-hook 'evil-surround-mode-setup)
  (defun evil-surround-mode-setup ()
    (let ((extra-alist (pcase major-mode
                         ((or 'lisp-interaction-mode 'emacs-lisp-mode)
                          '((?` . ("`" . "'"))))
                         ('js-mode '((?> . ("(e) => " . "(e)"))))
                         ('org-mode '((93 . ("[[" . "]]"))
                                      (?= . ("=" . "="))))
                         (_ nil))))
      (appendq! evil-surround-pairs-alist extra-alist)))
  )

(use-package evil-nerd-commenter)

;; Pinyin support
(use-package evil-find-char-pinyin
  :hook (evil-mode-hook . evil-find-char-pinyin-mode))

(use-package evil-terminal-cursor-changer
  :hook (after-make-console-frame-hook . etcc-mode))

(provide 'init-evil)
;;; init-evil.el ends here

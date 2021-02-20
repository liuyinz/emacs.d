;;; init-key.el --- keybinding for all  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'evil

  ;; set leader
  (evil-define-key 'motion 'global ";" nil "," nil)

  ;; (with-eval-after-load 'evil-maps
  ;;   (define-key evil-insert-state-map "\C-t" nil)
  ;;   (define-key evil-insert-state-map "\C-j" 'evil-open-below)
  ;;   (define-key evil-insert-state-map "\C-k" 'evil-open-above)
  ;;   )

  ;;evil binding
  (evil-define-key nil 'global
    (kbd "M-p") 'vterm-toggle
    (kbd "C-;") 'ace-window
    (kbd "C-s") 'swiper-isearch
    (kbd "C-c C-y") 'ivy-yasnippet
    (kbd "C-l l") 'avy-goto-line
    (kbd "C-l w") 'avy-goto-word-0
    (kbd "C-l f") 'avy-goto-char
    (kbd "C-l r") 'avy-resume
    (kbd "C-l p") 'avy-goto-paren)

  (evil-define-key '(normal visual) 'global
    ;; replace redo with U
    "\C-r" nil
    "U" 'evil-redo
    ;; ? to replace
    ;; "?" 'my-replace
    "?" (lambda ()
          (interactive)
          (if (evil-normal-state-p)
              (evil-ex "%s/")
            (evil-ex "'<,'>s/")))

    ;; select the previously pasted text
    ;; "gp" "`[v`]"
    ;; run the macro in the q register
    "Q" "@q"
    "gr" 'dash-at-point
    ;; magit
    "gs" 'magit-status

    (kbd "<leader> xf") 'counsel-find-file
    (kbd "<leader> xb") 'counsel-switch-buffer
    (kbd "<leader> xs") 'save-buffer
    (kbd "<leader> xk") 'kill-buffer
    (kbd "<leader> ;") 'counsel-counsel

    ;; color-rg
    (kbd "<leader> ss") 'color-rg-search-input-in-current-file
    (kbd "<leader> sd") 'color-rg-search-input
    (kbd "<leader> sp") 'color-rg-search-input-in-project
    (kbd "<leader> st") 'color-rg-search-project-with-type
    ;; nerd-commenter
    (kbd "<leader> cc") 'evilnc-comment-or-uncomment-lines
    (kbd "<leader> ci") 'evilnc-copy-and-comment-lines
    ;; self-define
    (kbd "<leader> mf") 'my-format
    (kbd "<leader> mr") 'my-run)

  ;;ivy
  (evil-define-key nil ivy-minibuffer-map
    (kbd "<escape>") 'minibuffer-keyboard-quit
    ;; (kbd "C-j") 'ivy-next-line-and-call
    ;; (kbd "C-k") 'ivy-previous-line-and-call
    ;; (kbd "C-u") 'ivy-dispatching-call
    (kbd "C-l") 'ivy-dispatching-done)

  (evil-define-key nil swiper-isearch-map
    (kbd "M-q") 'swiper-query-replace
    (kbd "C-t") 'isearch-toggle-color-rg)

  (evil-define-key nil counsel-mode-map
    (kbd "s-<f6>") 'counsel-osx-app)

  (evil-define-key nil counsel-find-file-map
    (kbd "C-h") 'counsel-up-directory
    (kbd "<backspace>") 'counsel-up-directory)

  ;; dired
  (evil-define-key nil dired-mode-map
    (kbd "C-c C-p") 'wdired-change-to-wdired-mode
    (kbd "C-c C-z f") 'browse-url-of-file
    "[" 'dired-hide-details-mode
    ")" 'dired-git-info-mode
    "(" 'dired-omit-mode)

  ;; Company
  (evil-define-key nil company-active-map
    (kbd "M-n") nil
    (kbd "M-p") nil
    (kbd "C-n") 'company-complete-common-or-cycle
    (kbd "C-p") 'company-select-previous
    (kbd "C-/") 'company-filter-candidates
    (kbd "<escape>") 'company-abort
    (kbd "<tab>") 'company-yas)

  (evil-define-key nil company-search-map
    (kbd "C-n") 'company-select-next
    (kbd "C-p") 'company-select-previous
    (kbd "<escape>") 'company-search-abort)

  ;; yasnippet
  (evil-define-key nil yas-minor-mode-map
    (kbd "C-c C-t") 'my-yasnippet-switch)

  ;; color-rg
  (evil-define-key nil color-rg-mode-map
    "h" 'color-rg-jump-prev-file
    "l" 'color-rg-jump-next-file)

  ;; awesome-pair
  (evil-define-key nil awesome-pair-mode-map
    "(" 'awesome-pair-open-round
    "[" 'awesome-pair-open-bracket
    "{" 'awesome-pair-open-curly
    ")" 'awesome-pair-close-round
    "]" 'awesome-pair-close-bracket
    "}" 'awesome-pair-close-curly
    "=" 'awesome-pair-equal
    "%" 'awesome-pair-match-paren
    "\"" 'awesome-pair-double-quote
    "SPC" 'awesome-pair-space
    "M-o" 'awesome-pair-backward-delete
    "C-d" 'awesome-pair-forward-delete
    "C-k" 'awesome-pair-kill
    "M-\"" 'awesome-pair-wrap-double-quote
    "M-[" 'awesome-pair-wrap-bracket
    "M-{" 'awesome-pair-wrap-curly
    "M-(" 'awesome-pair-wrap-round
    "M-)" 'awesome-pair-unwrap
    "M-p" 'awesome-pair-jump-right
    "M-n" 'awesome-pair-jump-left
    "M-:" 'awesome-pair-jump-out-pair-and-newline)

  ;; emmet-mode
  (evil-define-key nil emmet-mode-keymap
    (kbd "C-j") 'emmet-expand-yas)

  (evil-define-key nil emmet-preview-keymap
    (kbd "C-j") 'my-expand-yas)

  ;; js2-mode
  (evil-define-key nil js2-mode-map
    ;; nodejs-repl
    (kbd "C-x C-e") 'nodejs-repl-send-last-expression1
    (kbd "C-c C-l") 'nodejs-repl-send-line
    (kbd "C-c C-r") 'nodejs-repl-send-region
    (kbd "C-c C-f") 'nodejs-repl-load-file)

  ;;elisp-mode
  (evil-define-key nil emacs-lisp-mode-map
    (kbd "C-c C-x") 'ielm
    (kbd "C-c C-c") 'eval-defun
    (kbd "C-c C-b") 'eval-buffer)

  ;; vc
  (evil-define-key nil vc-prefix-map
    "B" 'browse-at-remote)

  ;; origami
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

  ;; flycheck
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
           (bury-buffer))))

(provide 'init-key)
;;; init-key.el ends here

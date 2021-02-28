;;; init-key.el --- keybinding for all  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'evil

  ;; set leader
  (evil-define-key 'motion 'global
    ";" nil
    "," nil
    (kbd "C-y") nil)

  ;; enable parts of keybinding
  (evil-define-key 'insert 'global
    (kbd "C-o") 'evil-execute-in-normal-state)

  ;;evil binding
  (evil-define-key nil 'global
    ;; cancel repoeat
    (kbd "C-x <escape> <escape>") nil

    ;; window
    (kbd "C-y") nil
    (kbd "C-y C-y") 'other-window
    (kbd "C-y C-t") 'transpose-frame
    (kbd "C-y k") 'delete-window
    (kbd "C-y o") 'delete-other-windows
    (kbd "C-y h") 'split-window-horizontally
    (kbd "C-y v") 'split-window-vertically

    ;; avy
    (kbd "C-l") nil
    (kbd "C-l C-l") 'avy-goto-line
    (kbd "C-l w") 'avy-goto-word-0
    (kbd "C-l f") 'avy-goto-char
    (kbd "C-l p") 'avy-goto-paren
    (kbd "C-l r") 'avy-resume
    ;; jump between two buffer
    (kbd "C-r") (lambda ()
                  (interactive)
                  (switch-to-buffer nil))
    ;; up-directory
    (kbd "C-<backspace>") 'backward-kill-sexp
    ;; vterm-toggle
    (kbd "C-,") 'vterm-toggle

    ;; selectrum
    (kbd "C-x C-z") 'selectrum-repeat
    ;; consult
    [remap switch-to-buffer] 'consult-buffer
    [remap switch-to-buffer-other-window] 'consult-buffer-other-window
    [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame
    ;; ("C-c h" . consult-history)
    ;; ("C-c m" . consult-mode-command)
    ;; ("C-c b" . consult-bookmark)
    ;; ("C-c k" . consult-kmacro)
    ;; ;; C-x bindings (ctl-x-map)
    ;; ("C-x M-:" . consult-complex-command)
    ;; ("C-x b" . consult-buffer)
    ;; ;; Custom M-# bindings for fast register access
    ;; ("M-#" . consult-register-load)
    ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (UNRELATED!)
    ;; ("C-M-#" . consult-register)
    ;; ;; Other custom bindings
    ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
    ;; ("<help> a" . consult-apropos)            ;; orig. apropos-command
    ;; ;; M-g bindings (goto-map)
    ;; ("M-g o" . consult-outline)
    ;; ("M-g m" . consult-mark)
    ;; ("M-g k" . consult-global-mark)
    ;; ("M-g i" . consult-project-imenu) ;; Alternative: consult-imenu
    ;; ;; ("M-g e" . consult-error)
    ;; ;; M-s bindings (search-map)
    ;; ("M-s g" . consult-git-grep)              ;; alt. consult-grep, consult-ripgrep
    ;; ("M-s f" . consult-find)                  ;; alt. consult-locate, find-fd
    ;; ("M-s l" . consult-line)
    ;; ("M-s m" . consult-multi-occur)
    ;; ("M-s k" . consult-keep-lines)
    ;; ("M-s u" . consult-focus-lines)
    ;; ;; Replacement for isearch-edit-string
    ;; ("M-s e" . consult-isearch)
    ;; ;; (:isearch-mode-map
    ;; ;;  ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
    ;; ;;  ("M-s e" . consult-isearch)))
    )

  (evil-define-key '(normal visual) 'global
    ;; replace redo with U
    (kbd "C-r") nil
    "U" 'evil-redo
    ;; ? to replace
    "?" (lambda ()
          (interactive)
          (if (evil-normal-state-p)
              (evil-ex "%s/")
            (evil-ex "'<,'>s/")))

    ;; select the previously pasted text
    ;; "gp" "`[v`]"
    ;; run the macro in the q register
    "Q" "@q"
    ;; dash
    "gp" 'dash-at-point
    ;; magit
    "gs" 'magit-status
    "gz" 'magit-dispatch
    "gla" 'magit-log-all-branches

    (kbd "<leader> xf") 'find-file
    (kbd "<leader> b") 'consult-buffer
    (kbd "<leader> xs") 'save-buffer
    (kbd "<leader> xk") 'kill-buffer
    (kbd "<leader> ;") 'consult-consult

    ;; color-rg
    (kbd "<leader> ss") 'color-rg-search-input-in-current-file
    (kbd "<leader> sd") 'color-rg-search-input
    (kbd "<leader> sp") 'color-rg-search-input-in-project
    (kbd "<leader> st") 'color-rg-search-project-with-type
    ;; nerd-commenter
    (kbd "<leader> c") 'evilnc-comment-or-uncomment-lines
    ;; self-define
    (kbd "<leader> f") 'my-format
    (kbd "<leader> r") 'my-run)

  ;; selectrum
  (evil-define-key nil selectrum-minibuffer-map
    (kbd "<escape>") 'minibuffer-keyboard-quit
    ;; embark
    (kbd "C-c C-o") 'embark-export
    (kbd "C-c C-c") 'embark-act)

  ;; dired
  (evil-define-key nil dired-mode-map
    (kbd "C-c C-p") 'wdired-change-to-wdired-mode
    (kbd "C-c C-z f") 'browse-url-of-file
    "[" 'dired-hide-details-mode
    ")" 'dired-git-info-mode
    "(" 'dired-omit-mode)

  ;; company
  (evil-define-key nil company-active-map
    (kbd "<escape>") 'company-abort
    (kbd "C-n") 'company-complete-common-or-cycle
    (kbd "C-p") 'company-select-previous
    (kbd "C-/") 'company-filter-candidates
    (kbd "C-h") 'company-show-doc-buffer
    (kbd "C-d") 'company-show-location
    (kbd "<tab>") 'my-company-yasnippet)

  (evil-define-key nil company-search-map
    (kbd "<escape>") 'company-search-abort
    (kbd "C-n") 'company-select-next
    (kbd "C-p") 'company-select-previous)

  ;; yasnippet
  (evil-define-key nil yas-minor-mode-map
    (kbd "C-c C-t") 'my-yasnippet-switch
    (kbd "<tab>") 'my-company-yasnippet)

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
    (kbd "SPC") 'awesome-pair-space
    (kbd "M-o") 'awesome-pair-backward-delete
    (kbd "C-d") 'awesome-pair-forward-delete
    (kbd "C-k") 'awesome-pair-kill
    (kbd "M-\"") 'awesome-pair-wrap-double-quote
    (kbd "M-[") 'awesome-pair-wrap-bracket
    (kbd "M-{") 'awesome-pair-wrap-curly
    (kbd "M-(") 'awesome-pair-wrap-round
    (kbd "M-)") 'awesome-pair-unwrap
    (kbd "M-p") 'awesome-pair-jump-right
    (kbd "M-n") 'awesome-pair-jump-left
    (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)

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
    ;; (kbd "RET") 'flycheck-error-list-goto-error
    "q" 'quit-window)

  (evil-define-key 'normal hl-todo-mode-map
    "[h" 'hl-todo-previous
    "]h" 'hl-todo-next
    "gh" 'hl-todo-occur
    (kbd "<leader> hi") 'hl-todo-insert)

  (evil-define-key 'normal diff-hl-mode-map
    "[g" 'diff-hl-previous-hunk
    "]g" 'diff-hl-next-hunk
    "gr" 'diff-hl-revert-hunk)

  (evil-define-key 'emacs vterm-mode-map
    ;; "\M-n" 'vterm-toggle-insert-cd
    (kbd "C-c C-o") 'vterm-send-C-o
    )


  (evil-define-key 'normal smerge-mode-map
    ;; move
    "n" 'smerge-next
    "p" 'smerge-prev
    ;; keep
    "a" 'smerge-keep-all
    "b" 'smerge-keep-base
    "l" 'smerge-keep-lower
    "u" 'smerge-keep-upper
    (kbd "C-m") 'smerge-keep-current
    (kbd "<return>") 'smerge-keep-current
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

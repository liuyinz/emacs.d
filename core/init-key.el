;;; init-key.el --- keybinding for all  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; key repeat speed on macOS, @https://stackoverflow.com/a/1052296/13194984

(defun back-to-user-buffer ()
  "Switch back to text buffer,exclude `vterm-mode'."
  (interactive)
  (switch-to-buffer nil)
  (when (eq major-mode 'vterm-mode)
    (vterm-toggle)
    (switch-to-buffer nil)))

(defun save-and-kill ()
  "Save and kill current buffer."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (save-buffer)
    (kill-buffer nil)))

(with-eval-after-load 'evil
  ;; enable parts of keybinding
  (evil-define-key 'insert 'global
    (kbd "C-o") 'evil-execute-in-normal-state)

  ;; set leader
  (evil-define-key 'motion 'global
    ";" nil
    "," nil
    (kbd "C-y") nil
    (kbd "C-o") nil)

  ;;evil binding
  (evil-define-key nil 'global
    (kbd "C-s-f") 'toggle-frame-fullscreen
    (kbd "s-s") 'consult-git-grep
    (kbd "s-f") 'consult-line
    (kbd "C-x <escape> <escape>") nil
    (kbd "C-x s") nil
    (kbd "s-m") nil
    (kbd "C-/") nil
    (kbd "M-c") nil
    (kbd "C-o") nil
    (kbd "C-j") nil
    (kbd "M-j") 'emmet-expand-yas
    ;; jump between two buffer
    (kbd "C-r") 'back-to-user-buffer

    ;; window
    (kbd "C-x j") 'transpose-frame

    ;; avy
    (kbd "C-l") nil
    (kbd "C-l C-l") 'avy-goto-line
    (kbd "C-l w") 'avy-goto-word-0
    (kbd "C-l f") 'avy-goto-char
    (kbd "C-l p") 'avy-goto-paren
    (kbd "C-l r") 'avy-resume

    ;; ;; toggle
    (kbd "C-; C-;") 'vterm-toggle
    (kbd "C-; p p") 'toggle-profiler
    (kbd "C-; c l") 'clm/toggle-command-log-buffer
    ;; debug
    (kbd "C-; d e") 'toggle-debug-on-error
    (kbd "C-; d q") 'toggle-debug-on-quit
    ;; proxy
    (kbd "C-; p h") 'proxy-http-toggle
    (kbd "C-; p s") 'proxy-socks-toggle

    ;; selectrum
    (kbd "C-x C-z") 'selectrum-repeat
    ;; consult
    [remap switch-to-buffer] 'consult-buffer
    [remap switch-to-buffer-other-window] 'consult-buffer-other-window
    [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame

    ;; helpful
    [remap describe-key] 'helpful-key
    [remap describe-variable] 'helpful-variable
    [remap describe-command] 'helpful-command
    [remap describe-function] 'helpful-callable
    [remap describe-symbol] 'helpful-at-point
    )

  (evil-define-key '(normal visual) 'global
    ;; replace redo with U
    (kbd "C-r") nil
    "u" (lambda ()
          (interactive)
          (if (not (fboundp 'vundo))
              (evil-undo 1)
            (vundo)
            (vundo-backward 1)))

    ;; "U" 'evil-redo
    "U" (lambda ()
          (interactive)
          (if (not (fboundp 'vundo))
              (evil-redo 1)
            (vundo)
            (vundo-forward 1)))

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
    ;; go-translate
    "gt" 'go-translate-echo-area
    ;; webjump
    "gw" 'webjump
    ;; magit
    "gs" 'magit-status
    "gz" 'magit-dispatch
    "gla" 'magit-log-all-branches

    (kbd "<leader> xf") 'find-file
    (kbd "<leader> b") 'consult-buffer
    (kbd "<leader> xs") 'save-buffer
    (kbd "<leader> xk") 'kill-buffer
    (kbd "<leader> ;") 'consult-consult

    ;; nerd-commenter
    (kbd "<leader> c") 'evilnc-comment-or-uncomment-lines
    ;; self-define
    (kbd "<leader> f") 'my-format
    (kbd "<leader> r") 'my-run)

  ;; selectrum
  (evil-define-key nil selectrum-minibuffer-map
    (kbd "<escape>") 'minibuffer-keyboard-quit
    ;; embark
    (kbd "C-,") 'embark-act
    (kbd "C-c C-o") 'embark-export
    (kbd "M-c") 'marginalia-cycle)

  ;; embark
  (evil-define-key nil embark-general-map
    (kbd "M-c") 'marginalia-cycle)
  ;; embark-consult
  (evil-define-key nil embark-collect-mode-map
    (kbd "C-j") 'embark-consult-preview-at-point)

  ;; flyspell
  (evil-define-key nil flyspell-mode-map
    (kbd "C-;") nil
    (kbd "C-,") nil
    (kbd "C-.") nil)

  ;;transient
  (evil-define-key nil transient-map
    (kbd "<escape>") 'transient-quit-one)

  ;; dired
  (evil-define-key nil dired-mode-map
    [remap dired-find-file] 'dired-single-buffer
    [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse
    [remap dired-up-directory] 'dired-single-up-directory
    (kbd "C-c C-p") 'wdired-change-to-wdired-mode
    (kbd "C-c C-z f") 'browse-url-of-file
    "[" 'dired-omit-mode
    "]" 'dired-hide-details-mode
    "{" 'dired-git-info-mode)

  ;; company
  (evil-define-key nil company-active-map
    (kbd "<tab>") nil
    (kbd "TAB") nil
    (kbd "<escape>") 'company-abort
    (kbd "C-n") 'company-complete-common-or-cycle
    (kbd "C-p") 'company-select-previous
    (kbd "C-s") 'company-filter-candidates
    (kbd "C-h") 'company-show-doc-buffer
    (kbd "C-d") 'company-show-location
    (kbd "C-,") 'my-company-yasnippet)

  ;; yasnippet
  (evil-define-key nil yas-minor-mode-map
    (kbd "<tab>") nil
    (kbd "TAB") nil
    (kbd "C-,") 'my-company-yasnippet)

  (evil-define-key nil company-search-map
    (kbd "<escape>") 'company-search-abort
    (kbd "C-n") 'company-select-next
    (kbd "C-p") 'company-select-previous)

  ;;wgrep
  (evil-define-key 'normal wgrep-mode-map
    ;; "" 'wgrep-mark-deletion
    "ZQ" 'wgrep-abort-changes
    "ZZ" 'wgrep-finish-edit
    (kbd "<escape>") 'wgrep-exit)

  ;;elisp-mode
  (evil-define-key nil emacs-lisp-mode-map
    (kbd "C-c C-x") 'ielm
    (kbd "C-c C-c") 'eval-defun
    (kbd "C-c C-b") 'eval-buffer)

  ;; vc
  (evil-define-key nil vc-prefix-map
    "B" 'browse-at-remote)

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

  (evil-define-key nil vundo--mode-map
    "U" 'vundo-forward
    "u" 'vundo-backward
    "l" 'vundo-forward
    "h" 'vundo-backward
    "j" 'vundo-next
    "k" 'vundo-previous
    (kbd "<escape>") 'vundo-quit)

  (evil-define-key 'normal hl-todo-mode-map
    "[h" 'hl-todo-previous
    "]h" 'hl-todo-next
    "gh" 'hl-todo-occur
    (kbd "<leader> i") 'hl-todo-insert)

  (evil-define-key 'normal diff-hl-mode-map
    "[g" 'diff-hl-previous-hunk
    "]g" 'diff-hl-next-hunk
    "gr" 'diff-hl-revert-hunk)

  (evil-define-key 'emacs vterm-mode-map
    (kbd "C-c C-o") 'vterm-send-C-o)

  (evil-define-key 'emacs smerge-mode-map
    ;; move
    "n" 'smerge-next
    "N" 'smerge-prev
    "p" 'smerge-prev
    ;; keep
    "a" 'smerge-keep-all
    "b" 'smerge-keep-base
    "o" 'smerge-keep-lower
    "m" 'smerge-keep-upper
    "c" 'smerge-keep-current
    "d" 'smerge-kill-current
    ;; diff
    "<" 'smerge-diff-base-upper
    "=" 'smerge-diff-upper-lower
    ">" 'smerge-diff-base-lower
    "r" 'smerge-refine
    "e" 'smerge-ediff
    ;; other
    "u" 'undo
    "C" 'smerge-combine-with-next
    "r" 'smerge-resolve
    (kbd "C-x s") 'save-and-kill
    ))

(provide 'init-key)
;;; init-key.el ends here

;;; init-key.el --- keybinding for all  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; key repeat speed on macOS,
;; SEE https://stackoverflow.com/a/1052296/13194984
;; SEE http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html
;; SEE http://ergoemacs.org/emacs/keystroke_rep.html
;; SEE https://evil.readthedocs.io/en/latest/keymaps.html

(with-eval-after-load 'evil

  ;;; -------------------------- Global ------------------------------

  (evil-define-key nil 'global
    (kbd "C-x <escape> <escape>") nil
    (kbd "C-x s") nil
    (kbd "C-/") nil
    (kbd "M-c") nil
    (kbd "C-o") nil
    (kbd "C-l") 'embark-act
    ;; jump between two buffer
    (kbd "C-r") (lambda () (interactive) (switch-to-buffer nil))
    ;; window
    (kbd "C-x j") 'transpose-frame
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

  (evil-define-key 'motion 'global
    ";" nil
    "," nil
    (kbd "C-y") nil
    (kbd "C-o") nil
    "'" 'evil-goto-mark
    "`" 'evil-goto-mark-line)

  (evil-define-key 'insert 'global
    (kbd "C-o") 'evil-execute-in-normal-state
    (kbd "C-j") 'yas-emmet-expand)

  (evil-define-key '(normal visual) 'global
    ;; replace redo with U
    (kbd "C-r") nil
    "U" 'evil-redo
    "?" 'evil-substitute-normal
    "zt" 'recenter-top-bottom
    "zz" 'hs-toggle-all

    ;; avy
    "zl" 'avy-goto-line
    "zc" 'avy-goto-char
    "zw" 'avy-goto-word-0
    "zp" 'avy-goto-paren

    ;; run the macro in the q register
    "Q" "@q"
    ;; select the previously pasted text
    "gp" "`[v`]"

    ;; xref
    "gb" 'xref-pop-marker-stack

    ;; magit
    "gm" 'magit-dispatch
    "gt" 'magit-file-dispatch
    "gs" 'magit-status
    "gla" 'magit-log-all-branches

    (kbd "<localleader> t") 'my/transient-toggle
    (kbd "<localleader> c") 'my/transient-consult
    (kbd "<localleader> f") 'my/transient-buffer
    (kbd "<localleader> ,") 'my/transient-point
    ;; (kbd "<localleader> g") 'my/transient-smerge
    (kbd "<localleader> w") 'my/transient-window
    (kbd "<localleader> m") 'rg-menu

    (kbd "<leader> c") 'evilnc-comment-or-uncomment-lines
    (kbd "<leader> f") 'my/format
    (kbd "<leader> p") 'my/repl
    (kbd "<leader> r") 'my/run)

  ;;; ------------------------ Major-mode ----------------------------

  (evil-define-key nil minibuffer-mode-map
    (kbd "ESC") 'minibuffer-keyboard-quit
    (kbd "<escape>") 'minibuffer-keyboard-quit)

  (evil-define-key nil dired-mode-map
    (kbd "C-c C-p") 'wdired-change-to-wdired-mode
    (kbd "C-c C-z f") 'browse-url-of-file
    "[" 'dired-omit-mode
    "]" 'dired-hide-details-mode
    "{" 'dired-git-info-mode
    "?" 'my/transient-dired
    )

  (evil-define-key nil transient-map
    (kbd "ESC") 'transient-quit-one
    (kbd "<escape>") 'transient-quit-one)

  (evil-define-key nil emacs-lisp-mode-map
    (kbd "C-c C-x") 'ielm
    (kbd "C-c C-c") 'eval-defun
    (kbd "C-c C-b") 'eval-buffer)

  (evil-define-key nil rg-mode-map
    "n" 'compilation-next-error
    "p" 'compilation-previous-error
    "f" 'compilation-first-error
    "l" 'compilation-last-error
    "N" 'compilation-next-file
    "P" 'compilation-previous-file
    (kbd "SPC") 'compilation-display-error
    (kbd "<space>") 'compilation-display-error
    "R" 'rg-replace
    "?" 'rg-menu)

  ;;; ------------------------ Minor-mode ----------------------------
  ;; Defining keybindings with `minor-mode' has higher precedence than with
  ;; `minor-mode-map'

  (evil-define-key 'visual 'emmet-mode
    (kbd "C-j") 'emmet-wrap-with-markup)

  (evil-define-key 'normal 'smerge-mode
    "?" 'my/transient-smerge)

  (evil-define-key '(normal visual) 'evil-matchit-mode
    "zm" 'evilmi-jump-items)

  (evil-define-key 'normal 'hl-todo-mode
    "[h" 'hl-todo-previous
    "]h" 'hl-todo-next
    "gh" 'hl-todo-occur
    )

  (evil-define-key 'normal 'diff-hl-mode
    "[g" 'diff-hl-previous-hunk
    "]g" 'diff-hl-next-hunk
    )

  (evil-define-key 'normal 'flycheck-mode
    "]f" 'flycheck-next-error
    "[f" 'flycheck-previous-error)

  ;;; ---------------------- Non-evil minor --------------------------

  (evil-define-key nil vertico-map
    ;; vertico-repeat
    (kbd "C-c C-r") 'vertico-repeat
    ;; vertico-directory
    (kbd "RET") 'vertico-directory-enter
    (kbd "<return>") 'vertico-directory-enter
    (kbd "DEL") 'vertico-directory-delete-char
    (kbd "<delete>") 'vertico-directory-delete-char
    ;; embark
    (kbd "C-l") 'embark-act
    (kbd "C-c C-o") 'embark-export
    (kbd "C-c C-a") 'marginalia-cycle
    )

  (evil-define-key nil consult-narrow-map
    (vconcat consult-narrow-key "?") 'consult-narrow-help)

  (evil-define-key nil embark-general-map
    (kbd "C-c C-a") 'marginalia-cycle
    )

  ;; ;; embark-consult
  ;; (evil-define-key nil embark-collect-mode-map
  ;;   (kbd "C-j") 'embark-consult-preview-at-point)

  ;; company
  (evil-define-key nil company-tng-map
    ;; disable tab key in `tng-mode'
    (kbd "TAB") nil
    (kbd "<tab>") nil
    (kbd "S-TAB") nil
    (kbd "<backtab>") nil
    (kbd "<return>") 'company-complete-selection
    (kbd "RET") 'company-complete-selection
    )

  (evil-define-key nil company-active-map
    (kbd "ESC") 'company-abort
    (kbd "<escape>") 'company-abort
    (kbd "C-s") 'company-filter-candidates
    (kbd "C-t") 'company-complete-common
    (kbd "C-n") 'company-select-next
    (kbd "C-p") 'company-select-previous)

  (evil-define-key nil company-search-map
    (kbd "<escape>") 'company-search-abort
    (kbd "ESC") 'company-search-abort
    (kbd "<return>") 'company-complete-selection
    (kbd "RET") 'company-complete-selection)

  (evil-define-key nil yas-keymap
    (kbd "<tab>") 'yas-next-field
    (kbd "TAB") 'yas-next-field)

  (evil-define-key nil vundo--mode-map
    "U" 'vundo-forward
    "u" 'vundo-backward
    "l" 'vundo-forward
    "h" 'vundo-backward
    "j" 'vundo-next
    "k" 'vundo-previous
    (kbd "ESC") 'vundo-quit
    (kbd "<escape>") 'vundo-quit
    )

  ;; (evil-define-key 'normal wgrep-mode-map
  ;;   ;; "" 'wgrep-mark-deletion
  ;;   "ZQ" 'wgrep-abort-changes
  ;;   "ZZ" 'wgrep-finish-edit
  ;;   (kbd "ESC") 'wgrep-exit
  ;;   (kbd "<escape>") 'wgrep-exit)

  )

(provide 'init-key)
;;; init-key.el ends here

;;; init-key.el --- keybinding for all  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; key repeat speed on macOS, @https://stackoverflow.com/a/1052296/13194984
;; NOTE see@http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html
;; see@http://ergoemacs.org/emacs/keystroke_rep.html

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

(defun yas-emmet-switch ()
  "Call `my-company-yasnippet' or `emmet-expand-yas' when needed."
  (interactive)
  (if (bound-and-true-p emmet-mode)
      (emmet-expand-yas)
    (my-company-yasnippet)))


(with-eval-after-load 'transient
  (transient-define-prefix my/transient-buffer ()
    "Buffer Operation"
    ["Info"
     ("N" "base name" buffer-base-name)
     ("A" "absolute path" file-absolute-path)
     ;; ("R" "relative path")
     ]
    ["Edit"
     ("b" "rename buffer" rename-buffer)
     ("f" "rename file" rename-this-file)
     ("B" "rename both" rename-both)
     ("d" "delete both" delete-both)
     ]
    ["Content"
     ;; ("u" "change utf")
     ("u" "dos2unix" dos2unix)
     ("U" "unix2dos" unix2dos)
     ])

  ;; toggle-transient
  (transient-define-prefix my/transient-toggle ()
    "Toogle system setting"
    ["Toggle"
     ("v" "Vterm" vterm-toggle)
     ("f" "Fullscreen" toggle-frame-fullscreen)
     ("k" "Keylog" toggle-keylog)
     ("e" "debug-on-error" toggle-debug-on-error)
     ("t" "debug-on-quit" toggle-debug-on-quit)
     ("s" "proxy-mode" global-proxy-mode)
     ("w" "writeroom-mode" writeroom-mode)
     ("p" "Profiler" toggle-profiler)
     ("V" "Vlf-mode" vlf-mode)
     ])

  (transient-define-prefix my/transient-consult ()
    [["Jump"
      ("b" "buffer" consult-buffer)
      ("B" "buffer other window" consult-buffer-other-window)
      ("j" "bookmark" consult-bookmark)
      ("l" "go-to-line" consult-goto-line)
      ("o" "outline" consult-outline)
      ("m" "mark" consult-mark)
      ("M" "global mark" consult-global-mark)
      ("i" "imenu" consult-imenu)
      ("I" "imenu project" consult-project-imenu)
      ]
     ["Info"
      ("f" "flycheck" consult-flycheck)
      ("h" "history" consult-complex-command)
      ("c" "mode command" consult-mode-command)
      ("q" "macro" consult-kmacro)
      ("z" "man" consult-man)
      ("R" "register" consult-register)
      ("a" "apropos" consult-apropos)
      ]
     ["Search"
      ("F" "find" consult-find)
      ("r" "ripgrep" consult-ripgrep)
      ("g" "git grep" consult-git-grep)
      ]]
    )

  (transient-define-prefix my/transient-point ()
    ["Action at Point"
     ("d" "dash-at-point" dash-at-point)
     ("t" "go-translate" go-translate-echo-area)
     ("w" "webjump" webjump)
     ("r" "revert-hunk" diff-hl-revert-hunk)
     ("x" "flycheck-explain" flycheck-explain-error-at-point)
     ("h" "hl-todo-insert" hl-todo-insert)
     ])

  ;; (transient-define-prefix my/help-transient ()
  ;;   "Help commands that I use. A subset of C-h with others thrown in."
  ;;   ["Help Commands"
  ;;    ["Mode & Bindings"
  ;;     ("m" "Mode" describe-mode)
  ;;     ("b" "Major Bindings" which-key-show-full-major-mode)
  ;;     ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
  ;;     ("d" "Descbinds" counsel-descbinds)
  ;;     ("t" "Top Bindings  " which-key-show-top-level)
  ;;     ]
  ;;    ["Describe"
  ;;     ("C" "Command" helpful-command)
  ;;     ("f" "Function" helpful-callable)
  ;;     ("v" "Variable" helpful-variable)
  ;;     ("k" "Key" helpful-key)
  ;;     ("c" "Key Briefly" describe-key-briefly)
  ;;     ]
  ;;    ["Info on"
  ;;     ("C-c" "Emacs Command" Info-goto-emacs-command-node)
  ;;     ("C-f" "Function" counsel-info-lookup-symbol) ; s for symbol?
  ;;     ("C-v" "Variable" counsel-info-lookup-symbol) ; . for symbol?
  ;;     ("C-k" "Emacs Key" Info-goto-emacs-key-command-node)
  ;;     ]
  ;;    ["Goto Source"
  ;;     ("L" "Library" find-library-other-frame)
  ;;     ("F" "Function" find-function-other-frame)
  ;;     ("V" "Variable" find-variable-other-frame)
  ;;     ("K" "Key" find-function-on-key-other-frame)
  ;;     ]
  ;;    ]
  ;;   [
  ;;    ["Internals"
  ;;     ("I" "Input Method" describe-input-method)
  ;;     ("G" "Language Env" describe-language-environment)
  ;;     ("S" "Syntax" describe-syntax)
  ;;     ("O" "Coding System" describe-coding-system)
  ;;     ("C-o" "Coding Brief" describe-current-coding-system-briefly)
  ;;     ("T" "Display Table" describe-current-display-table)
  ;;     ("e" "Echo Messages" view-echo-area-messages)
  ;;     ("l" "Lossage" view-lossage)
  ;;     ]
  ;;    ["Describe"
  ;;     ("s" "Symbol" helpful-symbol)
  ;;     ("." "At Point   " helpful-at-point)
  ;;     ("C-f" "Face" counsel-describe-face)
  ;;     ("w" "Where Is" where-is)
  ;;     ("=" "Position" what-cursor-position)
  ;;     ]
  ;;    ["Info Manuals"
  ;;     ("C-i" "Info" info)
  ;;     ("C-4" "Other Window " info-other-window)
  ;;     ("C-e" "Emacs" info-emacs-manual)
  ;;     ("C-l" "Elisp" info-elisp-manual)
  ;;     ]
  ;;    ["External"
  ;;     ("W" "Dictionary" lookup-word-at-point)
  ;;     ("D" "Dash" dash-at-point)
  ;;     ]
  ;;    ]
  ;;   )
  ;; (global-set-key (kbd "C-h") 'hrm-help-transient)
  )

(with-eval-after-load 'evil
  ;; enable parts of keybinding
  (evil-define-key 'insert 'global
    (kbd "C-o") 'evil-execute-in-normal-state)

  ;; set leader
  (evil-define-key 'motion 'global
    ";" nil
    "," nil
    (kbd "C-y") nil
    (kbd "C-o") nil
    "'" 'evil-goto-mark
    "`" 'evil-goto-mark-line)

  ;;evil binding
  (evil-define-key nil 'global
    (kbd "s-s") 'consult-git-grep
    (kbd "s-f") 'consult-line
    (kbd "C-l") 'yas-emmet-switch
    (kbd "C-x <escape> <escape>") nil
    (kbd "C-x s") nil
    (kbd "s-m") nil
    (kbd "C-/") nil
    (kbd "M-c") nil
    (kbd "C-o") nil
    (kbd "C-j") nil
    (kbd "C-l") nil

    ;; jump between two buffer
    (kbd "C-r") 'back-to-user-buffer

    ;; window
    (kbd "C-x j") 'transpose-frame
    (kbd "C-c n") 'vterm-toggle

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
    "u" 'evil-vundo-undo
    "U" 'evil-vundo-redo
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
    ;; "gp" "`[v`]"

    ;; magit
    "gs" 'magit-status
    "gz" 'magit-dispatch
    "gla" 'magit-log-all-branches

    (kbd "<localleader> t") 'my/transient-toggle
    (kbd "<localleader> c") 'my/transient-consult
    (kbd "<localleader> f") 'my/transient-buffer
    (kbd "<localleader> ,") 'my/transient-point
    (kbd "<localleader> m") 'rg-menu

    (kbd "<leader> c") 'evilnc-comment-or-uncomment-lines
    (kbd "<leader> f") 'my-format
    (kbd "<leader> p") 'my-repl
    (kbd "<leader> r") 'my-run)

  ;; vertico
  (evil-define-key nil vertico-map
    (kbd "ESC") 'minibuffer-keyboard-quit
    (kbd "<escape>") 'minibuffer-keyboard-quit
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

  ;; embark
  (evil-define-key nil embark-general-map
    (kbd "C-c C-a") 'marginalia-cycle
    )
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
    (kbd "ESC") 'transient-quit-one
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
    (kbd "ESC") 'company-abort
    (kbd "<escape>") 'company-abort
    (kbd "C-n") 'company-complete-common-or-cycle
    (kbd "C-s") 'company-filter-candidates
    (kbd "<tab>") 'yas-next-field
    (kbd "TAB") 'yas-next-field
    (kbd "<backtab>") 'yas-prev-field
    (kbd "S-TAB") 'yas-prev-field)

  ;; yasnippet
  (evil-define-key nil yas-minor-mode-map
    (kbd "<tab>") nil
    (kbd "TAB") nil)

  (evil-define-key nil company-search-map
    (kbd "<escape>") 'company-search-abort
    (kbd "ESC") 'company-search-abort
    (kbd "C-n") 'company-select-next
    (kbd "C-p") 'company-select-previous)

  ;;wgrep
  (evil-define-key 'normal wgrep-mode-map
    ;; "" 'wgrep-mark-deletion
    "ZQ" 'wgrep-abort-changes
    "ZZ" 'wgrep-finish-edit
    (kbd "ESC") 'wgrep-exit
    (kbd "<escape>") 'wgrep-exit
    )

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
    (kbd "RET") 'flycheck-error-list-goto-error
    "q" 'quit-window)

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

  (evil-define-key 'normal hl-todo-mode-map
    "[h" 'hl-todo-previous
    "]h" 'hl-todo-next
    "gh" 'hl-todo-occur
    )

  (evil-define-key 'normal diff-hl-mode-map
    "[g" 'diff-hl-previous-hunk
    "]g" 'diff-hl-next-hunk
    )

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

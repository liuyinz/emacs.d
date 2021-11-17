;;; init-key.el --- keybinding for all  -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2021-11-01 00:06:28

;;; Commentary:

;; SEE https://github.com/magit/transient/wiki
;; SEE https://stackoverflow.com/a/1052296/13194984
;; SEE http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html
;; SEE http://ergoemacs.org/emacs/keystroke_rep.html
;; SEE https://evil.readthedocs.io/en/latest/keymaps.html

;;; Code:

(with-eval-after-load 'transient
  (transient-define-prefix my/transient-consult ()
    "Command related to Consult"
    [["General"
      ("b" "Buffer"     consult-buffer)
      ("d" "Dir"        consult-dir)
      ("p" "Projectile" consult-projectile)
      ("f" "Flycheck"   consult-flycheck)
      ("i" "Imenu"      consult-imenu)
      ("l" "Line"       consult-line)
      ("s" "Snippet"    consult-yasnippet)
      ("y" "Yank"       consult-yank-from-kill-ring)]
     ["Jump"
      ("j b" "Bookmark" consult-bookmark)
      ("j o" "Outline"  consult-outline)
      ("j m" "Mark"     consult-mark)
      ("j g" "Global mark" consult-global-mark)
      ("j i" "Imenu project" consult-imenu-multi)]
     ["Info"
      ("o h" "History" consult-complex-command)
      ("o c" "Mode command" consult-mode-command)
      ("o q" "Macro" consult-kmacro)
      ("o z" "Man" consult-man)
      ("o R" "Register" consult-register)
      ("o a" "Apropos" consult-apropos)
      ("o t" "Theme" consult-theme)]
     ["Search"
      ("L" "Go-to-line" consult-goto-line)
      ("F" "Find" consult-find)
      ("r" "Ripgrep" consult-ripgrep)
      ("g" "Git grep" consult-git-grep)]
     ]
    )

  (transient-define-prefix my/transient-buffer ()
    "Buffer Operation"
    [["Info"
      ("N" "Base name" buffer-base-name)
      ("A" "Absolute path" file-absolute-path)
      ;; ("R" "relative path")
      ]
     ["Edit"
      ("r" "Revert buffer" revert-this-buffer)
      ("b" "Rename buffer" rename-buffer)
      ("f" "Rename file" rename-this-file)
      ("B" "Rename both" rename-both)
      ("d" "Delete both" delete-both)
      ]
     ["Content"
      ;; ("u" "change utf")
      ("u" "Dos2unix" dos2unix)
      ("U" "Unix2dos" unix2dos)
      ]])

  ;; toggle-transient
  (transient-define-prefix my/transient-toggle ()
    "Invoke toggle command generally or related to modes"
    [["General"
      ("p" "Profiler" toggle-profiler)
      ("e" "Debug-on-error" toggle-debug-on-error)
      ("t" "Debug-on-quit" toggle-debug-on-quit)
      ("k" "Keylog" toggle-keylog)
      ("K" "Keycast" toggle-keycast)
      ("s" "Proxy" global-proxy-mode)
      ("w" "Writeroom" writeroom-mode)
      ("V" "Vlf" vlf-mode)
      ("f" "Fullscreen" toggle-frame-fullscreen)
      ("L" "Visual line" visual-line-mode)
      ("C" "Truncated line" toggle-truncate-lines)
      ("W" "Word wrap" toggle-word-wrap)
      ("d" "Highlight face" highlight-defined-mode)
      ]
     [:description "Major"
      :if-derived markdown-mode
      ("m m" "Markdown toggle markup" markdown-toggle-markup-hiding)
      ("m t" "Markdown generate toc" markdown-toc-generate-or-refresh-toc)
      ("m d" "Markdown delete toc" markdown-toc-delete-toc)
      ("m p" "Markdown preview" grip-start-preview)
      ]
     [:description "Edit Indirect"
      :if edit-indirect-buffer-indirect-p
      ("y" "Save" edit-indirect-save)
      ("z" "Commit" edit-indirect-commit)
      ("q" "Abort" edit-indirect-abort)
      ]
     ])

  (transient-define-prefix my/transient-point ()
    "Action at Point"
    [["Info"
      ("u" "Dash doc" dash-at-point)
      ("h" "Elisp demos" elisp-demos-find-demo)
      ("e" "Fanyi to" fanyi-dwim2)
      ("w" "Search web" webjump)
      ("f" "Flycheck error" flycheck-explain-error-at-point :if-non-nil flycheck-mode)
      ("g" "Github browse" browse-at-remote)
      ("m" "Version messages" vc-msg-show)
      ]
     ["Edit"
      ("p" "Paste selection" cliphist-paste-item)
      ("i" "Edit indirect" my/edit-indirect)
      ("y" "Insert yasnippet" consult-yasnippet :if-non-nil yas-minor-mode)
      ("t" "Insert todo" hl-todo-insert :if-non-nil hl-todo-mode)
      ]
     [:description "diff-hl"
      :if-non-nil diff-hl-mode
      ("dd" "Show"   diff-hl-show-hunk)
      ("ds" "Stage"  diff-hl-stage-current-hunk)
      ("dr" "Revert" diff-hl-revert-hunk)
      ("dm" "Mark"   diff-hl-mark-hunk)
      ("dj" "Set Rev"   diff-hl-set-reference-rev)
      ("dk" "Reset Rev"   diff-hl-reset-reference-rev)
      ]
     ])

  (transient-define-prefix my/transient-window ()
    "Invoke commands related to position and size of windows"
    [["Position"
      ("i" "Flip" flip-frame)
      ("o" "Flop" flop-frame)
      ("t" "Transpose" transpose-frame)
      ("r" "Rotate" rotate-frame)
      ("c" "Clockwise" rotate-frame-clockwise)
      ("a" "Anti-clockwise" rotate-frame-anticlockwise)
      ]
     ["Size"
      ("z" "Zoom window" zoom)
      ("b" "Balance window" balance-windows)
      ("j" "Enlarge window vertical" enlarge-window :transient t)
      ("k" "Shrink window vertical" shrink-window :transient t)
      ("h" "Shrink window horizontal" shrink-window-horizontally :transient t)
      ("l" "Enlarge window horizontal" enlarge-window-horizontally :transient t)
      ]
     ["Layout"
      ("u" "Winner undo" winner-undo :transient t)
      ("U" "Winner redo" winner-redo :transient t)
      ]
     ])

  ;; smerge-transient
  (transient-define-prefix my/transient-smerge ()
    "Invoke commmands related to smerge-mode"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn
    [["Move"
      ("n" "Next conflict" smerge-vc-next-conflict)
      ("p" "Prev conflict" smerge-prev)
      ("f" "First conflict" smerge-first)
      ("l" "Last conflict" smerge-last)
      ("x" "Next file" (lambda () (interactive)
                         (vc-find-conflicted-file)
                         (smerge-first)
                         (smerge-refine 2)))
      ]
     ["Choose"
      ("a" "Keep all" smerge-keep-all)
      ("m" "Keep upper" smerge-keep-upper)
      ("b" "Keep base" smerge-keep-base)
      ("o" "Keep lower" smerge-keep-lower)
      ;; ("c" "Keep current" smerge-keep-current)
      ;; ("k" "kill current" smerge-kill-current)
      ]
     ["Diff"
      ("SPC" "Preview confict" smerge-conflict-preview-or-scroll)
      ("<" "Diff base/upper" smerge-diff-base-upper)
      (">" "Diff base/lower" smerge-diff-base-lower)
      ("=" "Diff upper/lower" smerge-diff-upper-lower)
      ("e" "Invoke ediff" smerge-ediff)
      ("h" "Refine highlight" smerge-refine)
      ]
     ["Edit"
      ("u" "Undo choose" undo)
      ("U" "Redo choose" undo-redo)
      ("s" "Swap upper/lower" smerge-swap)
      ("c" "Combine next" smerge-combine-with-next)
      ("C" "Combine all" smerge-combine-with-next)
      ("r" "Auto resolve" smerge-resolve)
      ("R" "Resolve all" smerge-resolve-all)
      ]
     [:description "Action"
      :if (lambda () (featurep 'magit))
      ("w" "Commit merge" magit-commit-create :transient nil)
      ("q" "Abort merge" magit-merge-abort :transient nil)
      ]])

  (transient-define-prefix my/transient-dired ()
    "Dired commands."
    [["Action"
      ("RET" "Open file"            dired-find-file)
      ("o" "  Open in other window" dired-find-file-other-window)
      ("C-o" "Open in other window (No select)" dired-display-file)
      ("v" "  Open file (View mode)"dired-view-file)
      ("=" "  Diff"                 dired-diff)
      ("w" "  Copy filename"        dired-copy-filename-as-kill)
      ("W" "  Open in browser"      browse-url-of-dired-file)
      ("y" "  Show file type"       dired-show-file-type)]
     ["Attribute"
      ("R"   "Rename"               dired-do-rename)
      ("G"   "Group"                dired-do-chgrp)
      ("M"   "Mode"                 dired-do-chmod)
      ("O"   "Owner"                dired-do-chown)
      ("T"   "Timestamp"            dired-do-touch)]
     ["Navigation"
      ("j" "  Goto file"            dired-goto-file)
      ("+" "  Create directory"     dired-create-directory)
      ("<" "  Jump prev directory"  dired-prev-dirline)
      (">" "  Jump next directory"  dired-next-dirline)
      ("^" "  Move up directory"    dired-up-directory)]
     ["Display"
      ("g" "  Refresh buffer"       revert-buffer)
      ("l" "  Refresh file"         dired-do-redisplay)
      ("k" "  Remove line"          dired-do-kill-lines)
      ("s" "  Sort"                 dired-sort-toggle-or-edit)
      ("(" "  Toggle detail info"   dired-hide-details-mode)
      ("i" "  Insert subdir"        dired-maybe-insert-subdir)
      ("$" "  Hide subdir"          dired-hide-subdir)
      ("M-$" "Hide subdir all"      dired-hide-subdir)]
     ["Extension"
      ("e"   "Wdired"               wdired-change-to-wdired-mode)
      ("/"   "Dired-filter"         ignore)
      ("n"   "Dired-narrow"         ignore)]]
    [["Marks"
      ("m" "Marks..." my/transient-dired-marks)]])


  (transient-define-prefix my/transient-dired-marks ()
    "Sub-transient for dired."
    [["Toggles"
      ("mm"  "Mark"                 dired-mark)
      ("mM"  "Mark all"             dired-mark-subdir-files)
      ("mu"  "Unmark"               dired-unmark)
      ("mU"  "Unmark all"           dired-unmark-all-marks)
      ("mc"  "Change mark"          dired-change-marks)
      ("mt"  "Toggle mark"          dired-toggle-marks)]
     ["Type"
      ("m*"  "Executables"          dired-mark-executables)
      ("m/"  "Directories"          dired-mark-directories)
      ("m@"  "Symlinks"             dired-mark-symlinks)
      ("m&"  "Garbage files"        dired-flag-garbage-files)
      ("m#"  "Auto save files"      dired-flag-auto-save-files)
      ("m~"  "backup files"         dired-flag-backup-files)
      ("m."  "Numerical backups"    dired-clean-directory)]
     ["Search"
      ("m%"  "Regexp"               dired-mark-files-regexp)
      ("mg"  "Regexp file contents" dired-mark-files-containing-regexp)]]
    [["Act on Marked"
      ("x"   "Do action"            dired-do-flagged-delete)
      ("C"   "Copy"                 dired-do-copy)
      ("D"   "Delete"               dired-do-delete)
      ("S"   "Symlink"              dired-do-symlink)
      ("H"   "Hardlink"             dired-do-hardlink)
      ("P"   "Print"                dired-do-print)
      ("A"   "Find"                 dired-do-find-regexp)
      ("Q"   "Replace"              dired-do-find-regexp-and-replace)
      ("B"   "Elisp bytecompile"    dired-do-byte-compile)
      ("L"   "Elisp load"           dired-do-load)
      ("X"   "Shell command"        dired-do-shell-command)
      ("Z"   "Compress"             dired-do-compress)
      ("z"   "Compress to"          dired-do-compress-to)
      ("!"   "Shell command"        dired-do-shell-command)
      ("&"   "Async shell command"  dired-do-async-shell-command)]])

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

  ;; -------------------------- Global ------------------------------

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
    (kbd "C-o") 'evil-execute-in-normal-state)

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

  ;; ------------------------ Major-mode ----------------------------

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

  ;; ------------------------ Minor-mode ----------------------------
  ;; Defining keybindings with `minor-mode' has higher precedence than with
  ;; `minor-mode-map'

  (evil-define-key 'insert 'yas-minor-mode
    (kbd "C-j") 'yas-expand)

  (evil-define-key 'insert 'emmet-mode
    (kbd "C-j") 'emmet-expand-line
    (kbd "TAB") 'emmet-next-edit-point
    (kbd "<tab>")  'emmet-next-edit-point
    (kbd "S-TAB") 'emmet-prev-edit-point
    (kbd "<backtab>")  'emmet-prev-edit-point)

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

  ;; ---------------------- Non-evil minor --------------------------

  (evil-define-key nil vertico-map
    ;; vertico-repeat
    ;; (kbd "C-c C-r") 'vertico-repeat
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
    (kbd "C-h") 'company-quickhelp-manual-begin
    )

  (evil-define-key nil company-active-map
    (kbd "ESC") 'company-abort
    (kbd "<escape>") 'company-abort
    (kbd "C-s") 'company-filter-candidates
    (kbd "C-t") 'company-complete-common
    (kbd "C-n") 'company-complete-common-or-cycle
    (kbd "C-p") 'company-select-previous
    )

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

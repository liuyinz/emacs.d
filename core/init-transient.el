;;; init-transient.el --- Setup for transient keymap -*- lexical-binding: t no-byte-compile: t -*-

;; Author: 食無魚
;; Created: 2021-07-26 14:56:32

;;; Commentary:

;;; Code:

(leaf transient
  :require t
  :init
  (setq transient-highlight-mismatched-keys nil
        transient-detect-key-conflicts t))

(with-eval-after-load 'transient
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
      ("s" "Proxy" global-proxy-mode)
      ("w" "Writeroom" writeroom-mode)
      ("V" "Vlf" vlf-mode)
      ("f" "Fullscreen" toggle-frame-fullscreen)
      ("L" "Visual line" visual-line-mode)
      ("C" "Truncated line" toggle-truncate-lines)
      ("W" "Word wrap" toggle-word-wrap)
      ("d" "Highlight face" highlight-defined-mode)
      ]
     [:description "Markdown"
      :if-derived markdown-mode
      ("m" "Markdown toggle markup" markdown-toggle-markup-hiding)
      ("T" "Markdown generate toc" markdown-toc-generate-or-refresh-toc)
      ("D" "Markdown delete toc" markdown-toc-generate-or-refresh-toc)
      ("g" "Markdown preview" grip-start-preview)
      ]
     [:description "Edit Indirect"
      :if edit-indirect-buffer-indirect-p
      ("y" "Save" edit-indirect-save)
      ("z" "Commit" edit-indirect-commit)
      ("q" "Abort" edit-indirect-abort)
      ]
     ])

  (transient-define-prefix my/transient-consult ()
    "Command related to Consult"
    [["Jump"
      ("b" "Buffer" consult-buffer)
      ("B" "Buffer other window" consult-buffer-other-window)
      ("j" "Bookmark" consult-bookmark)
      ("o" "Outline" consult-outline)
      ("m" "Mark" consult-mark)
      ("M" "Global mark" consult-global-mark)
      ("i" "Imenu" consult-imenu)
      ("I" "Imenu project" consult-imenu-multi)
      ]
     ["Info"
      ("f" "Flycheck" consult-flycheck)
      ("h" "History" consult-complex-command)
      ("c" "Mode command" consult-mode-command)
      ("q" "Macro" consult-kmacro)
      ("z" "Man" consult-man)
      ("R" "Register" consult-register)
      ("a" "Apropos" consult-apropos)
      ("Y" "Yank" consult-yank-from-kill-ring)
      ("t" "Theme" consult-theme)
      ("y" "Yasnippet" consult-yasnippet)
      ]
     ["Search"
      ("L" "Go-to-line" consult-goto-line)
      ("l" "Line" consult-line)
      ("F" "Find" consult-find)
      ("r" "Ripgrep" consult-ripgrep)
      ("g" "Git grep" consult-git-grep)
      ]]
    )

  (transient-define-prefix my/transient-point ()
    "Action at Point"
    [["Info"
      ("d" "Dash doc" dash-at-point)
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
      ("r" "Revert hunk" diff-hl-revert-hunk :if-non-nil diff-hl-mode)
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

(provide 'init-transient)
;;; init-transient.el ends here

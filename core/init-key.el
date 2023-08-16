;;; init-key.el --- keybinding for all  -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2021-11-01 00:06:28

;;; Commentary:

;; SEE https://github.com/magit/transient/wiki
;; SEE https://stackoverflow.com/a/1052296/13194984
;; SEE http://xahlee.info/emacs/emacs/emacs_key_notation_return_vs_RET.html
;; SEE http://xahlee.info/emacs/emacs/keystroke_rep.html
;; SEE https://www.masteringemacs.org/article/mastering-key-bindings-emacs

;;; Code:

(require 'dash)

(with-eval-after-load 'transient

  (transient-define-prefix my/transient-transform ()
    "Invoke commands about transform."
    [["Isolate"
      :if (lambda () (featurep 'isolate))
      ("i a" "add quick"    isolate-quick-add :if region-active-p)
      ("i d" "delete quick" isolate-quick-delete)
      ("i c" "change quick" isolate-quick-change)
      ("i A" "add long"     isolate-long-add :if region-active-p)
      ("i D" "delete long"  isolate-long-delete)
      ("i C" "change long"  isolate-long-change)]
     ["Coercion"
      :if (lambda () (featurep 'coercion))
      ("c p" "PascalCase"   coercion-pascal-case)
      ("c a" "camelCase"    coercion-camel-case)
      ("c n" "snake_case"   coercion-snake-case)
      ("c g" "Giraffe_Case" coercion-giraffe-case)
      ("c m" "MACRO_CASE"   coercion-macro-case)
      ("c k" "dash-case"    coercion-dash-case)
      ("c t" "Train-Case"   coercion-train-case)
      ("c b" "COBOL-CASE"   coercion-cobol-case)
      ("c o" "dot.case"     coercion-dot-case)
      ("c f" "flatcase"     coercion-flat-case)]
     ["Powerthesaurus"
      :if (lambda () (featurep 'powerthesaurus))
      ("p s" "Synonyms"    powerthesaurus-lookup-synonyms-dwim)
      ("p a" "Antonyms"    powerthesaurus-lookup-antonyms-dwim)
      ("p r" "Related"     powerthesaurus-lookup-related-dwim)
      ("p d" "Definitions" powerthesaurus-lookup-definitions-dwim)
      ("p t" "Sentences"   powerthesaurus-lookup-sentences-dwim)]
     ["jinx"
      :if (lambda () (featurep 'jinx))
      ("j j" "Toggle"  jinx-mode)
      ("j c" "Correct" jinx-correct)
      ("j l" "Lang"    jinx-languages)]
     ])

  (transient-define-prefix my/transient-consult ()
    "Invoke commands about Consult"
    [["General"
      ("b" "Buffer"        consult-buffer)
      ("d" "Dir"           consult-dir)
      ("p" "Project"       consult-project-extra-find)
      ("f" "Flymake"       consult-flymake)
      ("t" "Hl-todo"       consult-todo)
      ("l" "Line"          consult-line)
      ("s" "Snippet"       consult-yasnippet)
      ("y" "Yank"          consult-yank-from-kill-ring)]
     ["Jump"
      ("B" "Bookmark"      consult-bookmark)
      ("O" "Outline"       consult-outline)]
     ["Info"
      ("h" "History"       consult-complex-command)
      ("c" "Mode command"  consult-mode-command)
      ("m" "Macro"         consult-kmacro)
      ("H" "Man"           consult-man)
      ("i" "Info"          consult-info)
      ]
     ["Search"
      ("L" "Go-to-line"    consult-goto-line)
      ("F" "Find"          consult-find)
      ("r" "Ripgrep"       consult-ripgrep)
      ("g" "Git grep"      consult-git-grep)]
     ])

  (transient-define-prefix my/transient-buffer ()
    "Invoke commands about buffer"
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
      ("U" "Unix2dos" unix2dos)]
     ])

  (transient-define-prefix my/transient-toggle ()
    "Invoke toggle command generally or related to modes"
    [
     ["General"
      ("p" "Profiler" toggle-profiler)
      ("e" "Debug-on-error" toggle-debug-on-error)
      ("t" "Debug-on-quit" toggle-debug-on-quit)
      ("k" "Ilog" toggle-ilog)
      ("s" "Proxy" global-proxy-mode)
      ("o" "Olivetti" olivetti-mode)
      ("f" "Focus" focus-mode)
      ("F" "Fullscreen" toggle-frame-fullscreen)
      ("L" "Visual line" visual-line-mode)
      ("c" "Hide comments" obvious-mode)
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

  (transient-define-prefix my/transient-ide ()
    "Invoke commands about IDE"
    [["ide"
      ("r" "run" my/run)
      ("f" "format" my/format)]
     ;; ("p" "repl" my/repl)
     ["test"
      ("c" "point" testrun-nearest)
      ("n" "block" testrun-namespace)
      ("b" "file" testrun-file)
      ("a" "all" testrun-all)
      ("l" "last" testrun-last)
      ]
     ;; ["jupyter"]
     ])

  (transient-define-prefix my/transient-point ()
    "Invoke commands at point"
    :transient-non-suffix 'transient--do-warn
    [["Avy"
      ("l" "line"  avy-goto-line)
      ("p" "paren" avy-goto-paren)
      ("z" "char"  avy-goto-char)]
     ["Imenu"
      ("m" "buffer" consult-imenu)
      ("M" "project" consult-imenu-multi)]
     ["info"
      ("i u" "Devdocs" devdocs-at-point)
      ("i h" "Elisp example" helpful-at-point)
      ("i w" "Web search" webjump)
      ("i g" "Github browse" browse-at-remote)
      ("i m" "Version messages" vc-msg-show)
      ("i i" "Edit indirect" my/edit-indirect)
      ("i y" "Insert yasnippet" consult-yasnippet :if-non-nil yas-minor-mode)]
     [:description "diff-hl"
      :if-non-nil diff-hl-mode
      ("d j" "next" diff-hl-next-hunk :transient t)
      ("d k" "prev" diff-hl-previous-hunk :transient t)
      ("d w" "Show"   diff-hl-show-hunk)
      ("d s" "Stage"  diff-hl-stage-current-hunk)
      ("d r" "Revert" diff-hl-revert-hunk)
      ("d m" "Mark"   diff-hl-mark-hunk)
      ("d v" "Set Rev"   diff-hl-set-reference-rev)
      ("d f" "Reset Rev"   diff-hl-reset-reference-rev)]
     [:description "flymake"
      :if-non-nil flymake-mode
      ("f j" "next"    flymake-goto-next-error :transient t)
      ("f k" "prev"    flymake-goto-prev-error :transient t)
      ("f b" "buffer"  flymake-show-buffer-diagnostics)
      ("f e" "project" flymake-show-project-diagnostics)
      ("f c" "consult" consult-flymake)]
     [:description "hl-todo"
      :if-non-nil hl-todo-mode
      ("h j" "next" hl-todo-next :transient t)
      ("h k" "prev" hl-todo-previous :transient t)
      ("h i" "insert" hl-todo-insert)
      ("h o" "occur" hl-todo-occur)]
     ]
    )

  (transient-define-prefix my/transient-window ()
    "Invoke commands related to position and size of windows"
    [
     ["Layout"
      ("b"  "Below"  split-window-below)
      ("r"  "Right"  split-window-right)
      ("d"  "Delete" delete-window)
      ("x"  "Only"   delete-other-windows)
      ("u"  "Undo"   winner-undo :transient t)
      ("U"  "Redo"   winner-redo :transient t)
      ("\"" "Toggle" toggle-one-window)
      ]
     ["Size"
      ("e" "Balance" balance-windows)
      ("j" "Down" enlarge-window :transient t)
      ("k" "Up" shrink-window :transient t)
      ("h" "Left" shrink-window-horizontally :transient t)
      ("l" "Right" enlarge-window-horizontally :transient t)]
     ["Position"
      ("o"  "Jump"   other-window :transient t)
      ("B" "Flip" flip-frame)
      ("R" "Flop" flop-frame)
      ("t" "Transpose" transpose-frame)
      ("T" "Rotate" rotate-frame :transient t)
      ("c" "Clockwise" rotate-frame-clockwise)
      ("a" "Anti-clockwise" rotate-frame-anticlockwise)]
     ["Zoom"
      ("=" "In" text-scale-increase :transient t)
      ("-" "Out" text-scale-decrease :transient t)
      ("0" "reset" (lambda () (interactive) (text-scale-increase 0)) :transient t)]
     ])

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

  ;; (transient-define-prefix my/transient-dired ()
  ;;   "Dired commands."
  ;;   [["Action"
  ;;     ("RET" "Open file"            dired-find-file)
  ;;     ("o" "  Open in other window" dired-find-file-other-window)
  ;;     ("C-o" "Open in other window (No select)" dired-display-file)
  ;;     ("v" "  Open file (View mode)"dired-view-file)
  ;;     ("=" "  Diff"                 dired-diff)
  ;;     ("w" "  Copy filename"        dired-copy-filename-as-kill)
  ;;     ("W" "  Open in browser"      browse-url-of-dired-file)
  ;;     ("y" "  Show file type"       dired-show-file-type)]
  ;;    ["Attribute"
  ;;     ("R"   "Rename"               dired-do-rename)
  ;;     ("G"   "Group"                dired-do-chgrp)
  ;;     ("M"   "Mode"                 dired-do-chmod)
  ;;     ("O"   "Owner"                dired-do-chown)
  ;;     ("T"   "Timestamp"            dired-do-touch)]
  ;;    ["Navigation"
  ;;     ("j" "  Goto file"            dired-goto-file)
  ;;     ("+" "  Create directory"     dired-create-directory)
  ;;     ("<" "  Jump prev directory"  dired-prev-dirline)
  ;;     (">" "  Jump next directory"  dired-next-dirline)
  ;;     ("^" "  Move up directory"    dired-up-directory)]
  ;;    ["Display"
  ;;     ("g" "  Refresh buffer"       revert-buffer)
  ;;     ("l" "  Refresh file"         dired-do-redisplay)
  ;;     ("k" "  Remove line"          dired-do-kill-lines)
  ;;     ("s" "  Sort"                 dired-sort-toggle-or-edit)
  ;;     ("(" "  Toggle detail info"   dired-hide-details-mode)
  ;;     ("i" "  Insert subdir"        dired-maybe-insert-subdir)
  ;;     ("$" "  Hide subdir"          dired-hide-subdir)
  ;;     ("M-$" "Hide subdir all"      dired-hide-subdir)]
  ;;    ["Extension"
  ;;     ("e"   "Wdired"               wdired-change-to-wdired-mode)
  ;;     ("/"   "Dired-filter"         ignore)
  ;;     ("n"   "Dired-narrow"         ignore)]]
  ;;   [["Marks"
  ;;     ("m" "Marks..." my/transient-dired-marks)]])


  ;; (transient-define-prefix my/transient-dired-marks ()
  ;;   "Sub-transient for dired."
  ;;   [["Toggles"
  ;;     ("mm"  "Mark"                 dired-mark)
  ;;     ("mM"  "Mark all"             dired-mark-subdir-files)
  ;;     ("mu"  "Unmark"               dired-unmark)
  ;;     ("mU"  "Unmark all"           dired-unmark-all-marks)
  ;;     ("mc"  "Change mark"          dired-change-marks)
  ;;     ("mt"  "Toggle mark"          dired-toggle-marks)]
  ;;    ["Type"
  ;;     ("m*"  "Executables"          dired-mark-executables)
  ;;     ("m/"  "Directories"          dired-mark-directories)
  ;;     ("m@"  "Symlinks"             dired-mark-symlinks)
  ;;     ("m&"  "Garbage files"        dired-flag-garbage-files)
  ;;     ("m#"  "Auto save files"      dired-flag-auto-save-files)
  ;;     ("m~"  "backup files"         dired-flag-backup-files)
  ;;     ("m."  "Numerical backups"    dired-clean-directory)]
  ;;    ["Search"
  ;;     ("m%"  "Regexp"               dired-mark-files-regexp)
  ;;     ("mg"  "Regexp file contents" dired-mark-files-containing-regexp)]]
  ;;   [["Act on Marked"
  ;;     ("x"   "Do action"            dired-do-flagged-delete)
  ;;     ("C"   "Copy"                 dired-do-copy)
  ;;     ("D"   "Delete"               dired-do-delete)
  ;;     ("S"   "Symlink"              dired-do-symlink)
  ;;     ("H"   "Hardlink"             dired-do-hardlink)
  ;;     ("P"   "Print"                dired-do-print)
  ;;     ("A"   "Find"                 dired-do-find-regexp)
  ;;     ("Q"   "Replace"              dired-do-find-regexp-and-replace)
  ;;     ("B"   "Elisp bytecompile"    dired-do-byte-compile)
  ;;     ("L"   "Elisp load"           dired-do-load)
  ;;     ("X"   "Shell command"        dired-do-shell-command)
  ;;     ("Z"   "Compress"             dired-do-compress)
  ;;     ("z"   "Compress to"          dired-do-compress-to)
  ;;     ("!"   "Shell command"        dired-do-shell-command)
  ;;     ("&"   "Async shell command"  dired-do-async-shell-command)]])

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

;;; define-keys

(defun define-keys (keymap &rest pairs)
  "Define alternating key-def PAIRS for KEYMAP."
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (define-key keymap key def))))

;;; global-set-keys

(defun global-set-keys (&rest pairs)
  "Set alternating key-def PAIRS globally."
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (global-set-key key def))))

(global-set-keys

 (kbd "C-c i") 'my/transient-ide
 (kbd "C-c w") 'my/transient-window
 (kbd "C-c p") 'my/transient-point
 (kbd "C-c t") 'my/transient-toggle
 (kbd "C-c b") 'my/transient-buffer
 (kbd "C-c j") 'my/transient-consult
 (kbd "C-l")   'embark-act

 (kbd "M-l")   'my/transient-transform
 (kbd "C-j")   'scroll-other-window
 (kbd "C-k")   'scroll-other-window-down)

(provide 'init-key)
;;; init-key.el ends here

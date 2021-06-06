;;; init-transient.el --- transient setting -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(leaf transient :require t)

(define-transient-command my/buffer-transient ()
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
(define-transient-command my/toggle-transient ()
  "Toogle system setting"
  ["Toggle"
   ("v" "Vterm" vterm-toggle)
   ("f" "Fullscreen" toggle-frame-fullscreen)
   ("l" "Command-log" clm/toggle-command-log-buffer)
   ("e" "debug-on-error" toggle-debug-on-error)
   ("q" "debug-on-quit" toggle-debug-on-quit)
   ("h" "http-proxy" proxy-http-toggle)
   ("s" "socks-proxy" proxy-socks-toggle)
   ("w" "writeroom-mode" writeroom-mode)
   ("p" "Profiler" toggle-profiler)
   ])

(define-transient-command my/point-transient ()
  ["point"
   ("d" "dash" dash-at-point)
   ("x" "flycheck-explain" flycheck-explain-error-at-point)
   ])

(define-transient-command my/bracket-left-transient ()
  ["Previous jump"
   ("f" "flycheck-error" flycheck-previous-error)
   ("g" "diff-hl" diff-hl-previous-hunk)
   ("h" "hl-todo" hl-todo-previous)
   ]
  )

(define-transient-command my/bracket-right-transient ()
  ["Next jump"
   ("f" "flycheck-error" flycheck-next-error)
   ("g" "diff-hl" diff-hl-next-hunk)
   ("h" "hl-todo" hl-todo-next)
   ]
  )

;; (define-transient-command my/help-transient ()
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

;;; init-transient.el ends here
(provide 'init-transient)

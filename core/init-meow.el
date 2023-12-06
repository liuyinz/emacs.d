;;; init-meow.el --- modal editing -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-02-01 15:30:22

;;; Commentary:

;;; Code:

(leaf meow
  :require t
  :init

  (defun meow-setup ()
    (meow-normal-define-key
     ;; argument
     '("-" . negative-argument)
     '("q" . meow-quit)

     ;; jump
     '("h" . meow-left)
     '("j" . meow-next)
     '("k" . meow-prev)
     '("l" . meow-right)
     '("b" . meow-back-word)
     '("e" . meow-next-word)
     '("B" . meow-back-symbol)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("t" . meow-till)

     ;; edit
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("c" . meow-change)
     '("C" . meow-change-save)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("u" . meow-undo)
     '("U" . meow-redo)
     ;; '("U" . meow-undo-in-selection)

     ;; kill-region (cut)
     '("x" . meow-kill)
     '("X" . meow-kill-append)
     ;; kill-ring-save (copy)
     '("y" . meow-save)
     '("Y" . meow-save-append)
     ;; yank (paste)
     '("p" . meow-yank)
     '("P" . meow-yank-pop)
     ;; replace
     '("r" . meow-replace)
     '("R" . meow-replace-save)

     ;; selection
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '(";" . meow-reverse)

     '("H" . meow-left-expand)
     '("J" . meow-next-expand)
     '("K" . meow-prev-expand)
     '("L" . meow-right-expand)
     '("v" . meow-line)
     '("V" . meow-line-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("T" . meow-join)

     '("g" . meow-cancel-selection)
     '("s" . meow-pop-selection)

     ;; grab
     '("G" . meow-grab)
     '("F" . meow-sync-grab)
     '("S" . meow-swap-grab)
     ;; '("x" . meow-pop-grab)

     ;; search
     '("n" . meow-search)
     '("N" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)

     ;; macro
     '("m" . meow-start-kmacro-or-insert-counter)
     '("M" . meow-end-or-call-kmacro)

     ;; thing
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)

     ;; self-defined
     '("Q" . insecure-lock-enter)
     '("/" . isearch-forward-regexp)
     '("<escape>" . ignore)
     '("'" . binky-binky))

    (meow-motion-overwrite-define-key
     '("C-o" . meow-temp-normal)
     '("<escape>" . ignore)
     '("'" . binky-binky))

    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)))

  (defun my/meow-motion-temporary ()
    "Switch between meow-motion-mode and meow-normal-mode automatically."
    (when (region-active-p)
      (meow--cancel-selection))
    (if meow-motion-mode
        (meow-normal-mode)
      (meow-motion-mode)))

  (defvar meow--kbd-undo-redo "C-?"
    "KBD macro for command `undo-redo'.")
  (defun meow-redo ()
    "Cancel current selection then redo."
    (interactive)
    (when (region-active-p)
      (meow--cancel-selection))
    (meow--execute-kbd-macro meow--kbd-undo-redo))

  :defer-config

  (setq meow-replace-state-name-list
        '((normal . "<N>")
          (motion . "<M>")
          (keypad . "<K>")
          (insert . "<I>")
          (beacon . "<B>")))

  ;; customize thing
  (setq meow-display-thing-help t)
  (setq meow-char-thing-table
        '((?r . round)
          (?s . square)
          (?c . curly)
          (?a . angle)
          (?g . string)
          (?e . symbol)
          (?d . defun)
          (?l . line)
          (?p . paragraph)
          (?b . buffer)))
  (meow-thing-register 'angle
                       '(pair ("<") (">"))
                       '(pair ("<") (">")))

  (meow-setup)
  (meow-global-mode 1)

  (with-eval-after-load 'consult
    (setq meow-goto-line-function #'consult-goto-line))

  )


;; REQUIRE brew tap laishulu/macism
(leaf sis
  :init
  (defun my/meow-reset-sis (&rest _)
    "Reset english input when not in meow-insert-state."
    (unless meow-insert-mode
      (sis-set-english)))

  ;; Reset when switchshi to non-insert meow-state
  (add-hook 'meow-insert-exit-hook #'sis-set-english)
  ;; Reset when refocus in frame
  (add-function :after after-focus-change-function #'my/meow-reset-sis)
  ;; Reset when change window
  (add-hook 'window-selection-change-functions #'my/meow-reset-sis))

(provide 'init-meow)
;;; init-meow.el ends here

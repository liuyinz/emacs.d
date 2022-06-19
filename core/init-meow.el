;;; init-meow.el --- modal editing -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-02-01 15:30:22

;;; Commentary:

;;; Code:

(leaf meow
  :require t
  :init
  (with-eval-after-load 'consult
    (setq meow-goto-line-function #'consult-goto-line))

  (defun meow-setup ()
    (meow-normal-define-key
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
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-start-kmacro-or-insert-counter)
     '("M" . meow-end-or-call-kmacro)
     '("n" . meow-search)
     '("N" . meow-visit)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("r" . meow-pop-selection)
     ;; '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("S" . meow-replace)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-join)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . my/transient-jump)
     '("'" . repeat)
     '("/" . isearch-forward-regexp)
     '("<escape>" . ignore))
    (meow-motion-overwrite-define-key
     ;; '("j" . meow-next)
     ;; '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     ;; '("j" . "H-j")
     ;; '("k" . "H-k")
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

  :defer-config

  (meow-setup)
  (meow-global-mode 1)

  (defun my/meow-motion-temporary ()
    "Switch between meow-motion-mode and meow-normal-mode automatically."
    (when (region-active-p)
      (meow--cancel-selection))
    (if meow-motion-mode
        (meow-normal-mode)
      (meow-motion-mode))))


;; REQUIRE brew tap laishulu/macism
(leaf sis
  :init
  (defun my/meow-reset-sis (&rest _)
    "Reset english input when not in meow-insert-state."
    (unless meow-insert-mode
      (sis-set-english)))

  ;; Reset when switch to non-insert meow-state
  (add-hook 'meow-switch-state-hook #'my/meow-reset-sis)
  ;; Reset when refocus in frame
  (add-function :after after-focus-change-function #'my/meow-reset-sis)

  )

(provide 'init-meow)
;;; init-meow.el ends here

(defun meow-setup ()
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . meow-motion-origin-command)
   '("k" . meow-motion-origin-command)
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
   '("0" . meow-digit-argument))

  (meow-normal-define-key
   '("?" . negative-argument)
    ;; Move
   '("G" . goto-line)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("h" . meow-head)
   '("l" . meow-tail)

   ;; Selection
   '(";" . meow-reverse)
   '("x" . meow-line)

   '("o" . meow-block)
   '("O" . meow-block-expand)

   '("J" . meow-next-expand)
   '("K" . meow-prev-expand)
   '("H" . meow-head-expand)
   '("L" . meow-tail-expand)

   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)

   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)

   ;; Edit
   '("u" . undo)
   '("U" . undo-redo)

   '("i" . meow-insert)
   '("a" . meow-append)
   '("I" . meow-open-above)
   '("A" . meow-open-below)

   '("d" . meow-delete)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("m" . meow-join)
   '("s" . meow-kill)

   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("t" . meow-till)
   '("T" . meow-till-expand)

   '("/" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)

   '("n" . meow-search)
   '("r" . meow-replace)
   '("N" . meow-pop-search)
   '("R" . meow-replace-save)

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

   ; '("g" . meow-keyboard-quit)
   '("M" . delete-indentation)
   '("q" . meow-quit)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   ; '("<escape>" . meow-last-buffer)
   '("<escape>" . meow-keyboard-quit)
   ))

(use-package meow
  :demand
  ; :quelpa (meow (:fetch github :repo "DogLooksGood/meow"))
  :init
  (meow-global-mode 1)
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-replace-state-name-list
    '((normal . "<N>")
      (insert . "<I>")
      (keypad . "<K>")
      (motion . "<M>")))
  (meow-setup))

(provide 'init-meow)

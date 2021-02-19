;;; init-frame.el --- setting for frame  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; @https://stackoverflow.com/a/7623566/13194984
;; (window-system) (display-graphic-p) focus on the current frame,need to select
;; the new frame with (select-frame)
;; @https://github.com/purcell/emacs.d/blob/master/lisp/init-frame-hooks.el

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")

(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(add-hook 'after-make-console-frame-hooks
          (lambda ()
            (menu-bar-mode -1)
            (tool-bar-mode -1)
            (xterm-mouse-mode 1)
            (diff-hl-mode -1)
            ;; enable mouse in cocoa terminal
            (when (featurep 'ns)
              (global-set-key (kbd "<mouse-4>") (kbd "<wheel-up>"))
              (global-set-key (kbd "<mouse-5>") (kbd "<wheel-down>")))))

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst my-initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda ()
            (when my-initial-frame
              (run-after-make-frame-hooks my-initial-frame))))

(provide 'init-frame)
;;; init-frame.el ends here

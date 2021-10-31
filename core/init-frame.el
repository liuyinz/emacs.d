;;; init-frame.el --- provide frame-hooks -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; SEE https://www.reddit.com/r/emacs/comments/lelbr5/how_to_start_emacsclient_such_that_it_respects_my/gmhbyv7?utm_source=share&utm_medium=web2x&context=3
;; https://github.com/purcell/emacs.d/blob/adf337dfa8c324983e5dc01ed055a34c3cc4a964/lisp/init-frame-hooks.el

(defvar after-make-console-frame-hook '()
  "Hooks to run after creating a new TTY frame.")

(defvar after-make-window-system-frame-hook '()
  "Hooks to run after creating a new `window-system' frame.")

(defun my/frame-setup ()
  "Setup for frame related hooks."
  (run-hooks (if (display-graphic-p)
                 'after-make-window-system-frame-hook
               'after-make-console-frame-hook)))
(add-hook 'server-after-make-frame-hook #'my/frame-setup)

(defun my/frame-no-server-setup (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (unless (daemonp)
    (with-selected-frame frame
      (my/frame-setup))))
(add-hook 'after-make-frame-functions 'my/frame-no-server-setup)

(defconst my/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")
(add-hook 'after-init-hook
          (lambda () (when my/initial-frame
                       (my/frame-no-server-setup my/initial-frame))))

(provide 'init-frame)

;;; init-frame.el ends here

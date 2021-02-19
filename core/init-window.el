;;; init-window.el --- window setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf ace-window
  :commands ace-window
  :hook (emacs-startup-hook . ace-window-display-mode)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?q ?w ?r ?t)
        aw-scope 'frame
        aw-background nil
        aw-dispatch-always t
        aw-minibuffer-flag t
        aw-dispatch-alist '((?x aw-delete-window "Delete Window")
                            (?v aw-split-window-vert "Split Vert Window")
                            (?h aw-split-window-horz "Split Horz Window")
                            (?o delete-other-windows "Delete Other Windows")
                            (?m aw-swap-window "Swap Windows")
                            (?M aw-move-window "Move Window")
                            (?c aw-copy-window "Copy Window")
                            (?j aw-switch-buffer-in-window "Select Buffer")
                            (?n aw-flip-window)
                            (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
                            (?? aw-show-dispatch-help))))


(provide 'init-window)
;;; init-window.el ends here

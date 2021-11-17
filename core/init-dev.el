;;; init-dev.el --- Devtool for Emacs Package -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2021-11-02 12:35:16

;;; Commentary:

;; SEE https://github.com/alphapapa/emacs-package-dev-handbook

;;; Code:

;; ----------------------- Demonstration ---------------------------
(leaf keycast
  :commands toggle-keycast
  :defer-config
  ;; ISSUE https://github.com/seagle0128/doom-modeline/issues/122#issuecomment-780683648
  (defun toggle-keycast()
    (interactive)
    (if (member '("" mode-line-keycast " ") global-mode-string)
        (progn (setq global-mode-string (delete '("" mode-line-keycast " ") global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast disabled"))
      (add-to-list 'global-mode-string '("" mode-line-keycast " "))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast enabled"))))

(leaf interaction-log
  :hook (ilog-log-buffer-mode-hook . (lambda ()
                                       (setq ilog-display-state 'messages)
                                       (ilog-toggle-view)))
  :init
  (setq ilog-log-max nil)
  ;; FIXME press twice should be avoided.
  (defun toggle-keylog ()
    "Toggle keybinds log."
    (interactive)
    (require 'interaction-log)
    (unless (bufferp ilog-buffer-name)
      (interaction-log-mode))
    (with-current-buffer ilog-buffer-name
      (let ((win (get-buffer-window (current-buffer))))
        (if (not (windowp win))
            (progn
              (unless interaction-log-mode
                (interaction-log-mode))
              (display-buffer (current-buffer)))
          (if interaction-log-mode
              (progn
                (interaction-log-mode -1)
                (delete-window win))
            (interaction-log-mode)))))))

;; -------------------------- Review ------------------------------

(leaf comb)

;; ---------------------------- Keg --------------------------------
;; SEE https://github.com/melpa/melpa#recipe-format
(leaf keg)

(provide 'init-dev)
;;; init-dev.el ends here

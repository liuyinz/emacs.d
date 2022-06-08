;;; init-dev.el --- Devtool for Emacs Package -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2021-11-02 12:35:16

;;; Commentary:

;; SEE https://github.com/alphapapa/emacs-package-dev-handbook

;;; Code:

;; ----------------------- Demonstration ---------------------------

(leaf keycast
  :commands toggle-keycast
  :init
  (setq keycast-mode-line-format "%10s%K%R%C")
  ;; SEE https://github.com/seagle0128/doom-modeline/issues/122#issuecomment-1133838869
  (defun toggle-keycast()
    (interactive)
    (require 'keycast)
    (let ((key '("" keycast-mode-line " ")))
      (if (member key global-mode-string)
          (progn (setq global-mode-string (delete key global-mode-string))
                 (remove-hook 'pre-command-hook 'keycast--update)
                 (message "Keycast OFF"))
        (add-to-list 'global-mode-string key)
        (add-hook 'pre-command-hook 'keycast--update t)
        (message "Keycast ON"))))
  )

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

(provide 'init-dev)
;;; init-dev.el ends here

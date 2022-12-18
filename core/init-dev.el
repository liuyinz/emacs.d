;;; init-dev.el --- Devtool for Emacs Package -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2021-11-02 12:35:16

;;; Commentary:

;; SEE https://github.com/alphapapa/emacs-package-dev-handbook

;;; Code:

;; ----------------------- Demonstration ---------------------------

;; (leaf keycast
;;   :commands toggle-keycast
;;   :init
;;   (setq keycast-mode-line-format "%10s%K%R%C")
;;   ;; SEE https://github.com/seagle0128/doom-modeline/issues/122#issuecomment-1133838869
;;   (defun toggle-keycast()
;;     (interactive)
;;     (require 'keycast)
;;     (let ((key '("" keycast-mode-line " ")))
;;       (if (member key global-mode-string)
;;           (progn (setq global-mode-string (delete key global-mode-string))
;;                  (remove-hook 'pre-command-hook 'keycast--update)
;;                  (message "Keycast OFF"))
;;         (add-to-list 'global-mode-string key)
;;         (add-hook 'pre-command-hook 'keycast--update t)
;;         (message "Keycast ON"))))
;;   )

(leaf interaction-log
  :init
  (defun toggle-ilog ()
    "Toggle interaction-log."
    (interactive)
    (require 'interaction-log)
    (let ((win (get-buffer-window ilog-buffer-name)))
      (if (and interaction-log-mode win)
          (progn
            (interaction-log-mode -1)
            (delete-window win)
            (kill-buffer ilog-buffer-name))
        (unless interaction-log-mode (interaction-log-mode +1))
        (unless win
          (run-with-timer
           (if (get-buffer ilog-buffer-name) 0 (* ilog-idle-time 2))
           nil
           (lambda ()
             (with-current-buffer ilog-buffer-name
               (remove-from-invisibility-spec 'ilog-command)
               (add-to-invisibility-spec 'ilog-load)
               (add-to-invisibility-spec 'ilog-buffer)
               (add-to-invisibility-spec 'ilog-message)
               (setq ilog-display-state 'commands))
             (display-buffer ilog-buffer-name)))))))
  )


(provide 'init-dev)
;;; init-dev.el ends here

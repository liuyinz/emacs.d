;;; init-dev.el --- Devtool for Emacs Package -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2021-11-02 12:35:16

;;; Commentary:

;; SEE https://github.com/alphapapa/emacs-package-dev-handbook

;;; Code:

;; ----------------------- Demonstration ---------------------------
(leaf keycast)

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
;; ISSUE https://github.com/conao3/keg.el/issues/42
  ;; (setq keg-archives
  ;;       '((gnu    . "http://elpa.zilongshanren.com/gnu/")
  ;;         (org    . "http://elpa.zilongshanren.com/org/")
  ;;         (melpa  . "http://elpa.zilongshanren.com/melpa/")
  ;;         (nongnu . "https://elpa.nongnu.org/nongnu/")
  ;;         (celpa  . "https://celpa.conao3.com/packages/")))
(leaf keg :require t)

(provide 'init-dev)
;;; init-dev.el ends here

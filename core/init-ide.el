;;; init-ide.el --- IDE setting for code -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Code Check
;; @https://www.flycheck.org/en/latest/
(leaf flycheck
  :hook (prog-mode-hook . flycheck-mode)
  :init
  (setq flycheck-stylelintrc "~/.stylelintrc.json"
        flycheck-tidyrc "~/.tidyrc"
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-indication-mode 'right-fringe)
  :config
  ;; Prettify fringe style
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  )


;; Code Running
;; @https://github.com/emacsorphanage/quickrun#customize
(leaf quickrun
  :defun (imp-visit-buffer . impatient-mode)
  :commands quickrun quickrun-region
  :init
  (setq quickrun-focus-p nil
        quickrun-timeout-seconds 20))

(defun my-run (&optional start end)
  "Running for whole or parts."
  (interactive "r")
  (cond
   ((member major-mode '(html-mode)) (imp-visit-buffer))
   (t (if (evil-visual-state-p)
          (quickrun-region start end)
        (quickrun)))))

(defun my-repl ()
  "Runinig for interactive."
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode) (ielm))
   ((eq major-mode 'python-mode) (run-python))
   ;; ((eq major-mode 'lua-mode) (run-lua))
   ((member major-mode '(js-mode js2-mode)) (nodejs-repl))
   (t (message "no repl for selected mode"))))

(defun quickrun-vterm ()
  "Quickrun command in vterm."
  (interactive)
  (let ((buffer buffer-file-name))
    (vterm-toggle-cd-show)
    (vterm-send-string (format "node %s" buffer))
    (vterm-send-return)))

;; Code Formating
(leaf editorconfig
  :hook (shell-mode-hook . editorconfig-mode))

(leaf format-all
  :doc "deps: inheritenv language-id"
  :hook ((prog-mode-hook markdown-mode-hook) . format-all-ensure-formatter)
  :commands format-all-buffer
  :init
  (setq format-all-debug t)
  ;; silent ensure message
  (advice-add #'format-all-ensure-formatter :around #'silent-message-advice)
  :config
  (add-to-list 'format-all-default-formatters '("HTML" prettier)))

(defun my-format ()
  "Formating files."
  (interactive)
  (whitespace-cleanup)
  (format-all-buffer))

(provide 'init-ide)
;;; init-ide.el ends here

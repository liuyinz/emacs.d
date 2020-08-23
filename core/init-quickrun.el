(declare-function imp-visit-buffer 'impatient-mode)

;; Run commands quickly
(use-package quickrun
  :commands (quickrun quickrun-region)
  :init
  (setq quickrun-focus-p nil
        quickrun-timeout-seconds 20))

(defun my-run (&optional start end)
  "running for whole or parts"
  (interactive "r")
  (cond
   ((member major-mode '(html-mode web-mode)) (imp-visit-buffer))
   (t (if (evil-visual-state-p)
          (quickrun-region start end)
        (quickrun)))))

(defun my-repl ()
  "runinig for interactive"
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode) (ielm))
   ((eq major-mode 'python-mode) (run-python))
   ((eq major-mode 'lua-mode) (run-lua))
   ((member major-mode '(js-mode js2-mode)) (nodejs-repl))
   (t (message "no repl for selected mode"))))

(defun quickrun-vterm ()
  "Quickrun command in vterm"
  (interactive)
  (let ((buffer buffer-file-name))
    (vterm-toggle-cd-show)
    (vterm-send-string (format "node %s" buffer))
    (vterm-send-return)))

(provide 'init-quickrun)

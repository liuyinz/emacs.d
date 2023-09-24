;;; init-ide.el --- IDE setting for code -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; --------------------------- VTERM -------------------------------

(leaf vterm
  :hook (vterm-mode-hook . vterm-setup)
  :bind
  ("M-u" . vterm-toggle)
  (:vterm-mode-map
   ("M-u" . nil))
  :init
  (defun vterm-setup ()
    (meow-mode -1)
    (hide-mode-line-mode -1)))

(leaf vterm-toggle)

;; -------------------------- docstr ------------------------------
;; --------------------------- Doc --------------------------------

(define-command-mixed
  devdocs-at-point
  word
  "Search devdocs.io"

  ;; SEE https://devdocs.io/help
  (browse-url (format "https://devdocs.io/#q=%s" (url-hexify-string query)))

  ;; SEE https://github.com/egoist/devdocs-desktop#using-homebrew
  ;; (shell-command (format "open devdocs://search/%s" (url-hexify-string query)))
  )

;; --------------------------- Run --------------------------------

(leaf quickrun
  :init
  (setq quickrun-focus-p nil
        quickrun-timeout-seconds 20)
  :defer-config
  (prependq! quickrun--major-mode-alist
             '((lisp-interaction-mode . "emacs")
               (typescript-ts-mode . "typescript")
               (js-ts-mode . "javascript")
               (bash-ts-mode . "shellscript"))))

(defun my/run ()
  "Running Current Buffer."
  (interactive)
  (cond
   ((member major-mode '(markdown-mode gfm-mode)) (grip-start-preview))
   ((member major-mode '(web-mode html-mode mhtml-mode)) (imp-visit-buffer))
   (t (run-general! quickrun-region quickrun))))

;; --------------------------- test -------------------------------

(leaf testrun)

;; -------------------------- jupyter ------------------------------

;; (defun my/repl ()
;;   "Runinig for interactive."
;;   (interactive)
;;   (cl-case major-mode
;;     ((emacs-lisp-mode lisp-interaction-mode)
;;      (run-general! eval-region eval-buffer))
;;     ((js-mode js2-mode jsonian-mode)
;;      (run-general! nodejs-repl-send-region nodejs-repl-send-buffer))
;;     (python-mode (run-python))
;;     (t (message "no repl for selected mode"))))

;; REQUIRE pip install notebook
;; SEE https://github.com/jupyter/jupyter/wiki/Jupyter-kernels
(leaf jupyter
  :init
  (setq jupyter-repl-prompt-margin-width 10
        jupyter-eval-use-overlays t))

(provide 'init-ide)
;;; init-ide.el ends here

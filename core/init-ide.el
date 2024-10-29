;;; init-ide.el --- IDE setting for code -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:


;; REQUIRE brew install libvterm cmake
(leaf vterm
  :hook (vterm-mode-hook . vterm-setup)
  :bind
  (:vterm-mode-map
   ("M-u" . nil)
   ("M-i" . nil))
  :init
  ;; (setq vterm-term-environment-variable "xterm-kitty")
  (setq vterm-buffer-name "*vterm*")

  (defun vterm-setup ()
    (meow-mode -1)
    (and hl-line-mode (hl-line-mode 'toggle)))

  (defun vterm--get-buffers ()
    "Return a list of vterm buffers."
    (--filter (eq (buffer-local-value 'major-mode it) 'vterm-mode)
              (buffer-list)))
  
  (defun vterm--get-windows ()
    "Return a list of windows display vterm buffers."
    (--filter (eq (buffer-local-value 'major-mode (window-buffer it))
                  'vterm-mode)
              (window-list nil 'no-minibuf)))

  (defun vterm-new (&optional other)
    "Create an new interactive Vterm buffer.
If here is a window display vterm buffer, then creat a new one in that window.
Or create a new one in other window."
    (interactive "P")
    (let ((len (length (vterm--get-buffers)))
          (wins (vterm--get-windows))
          (dir default-directory))
      (when (> len 0)
        (cl-incf len)
        (while (get-buffer (format "%s<%d>" vterm-buffer-name len))
          (cl-incf len)))
      (let ((arg (and (> len 0) len)))
        (if (not wins)
            (vterm-other-window arg)
          (let ((default-directory dir)
                (win (if (memq (selected-window) wins)
                         (car wins)
                       (car (last wins)))))
            (select-window
             (or (and other
                      (if (or (window-full-width-p win)
                              (eq 'vterm-mode
                                  (buffer-local-value
                                   'major-mode
                                   (window-buffer
                                    (window-in-direction 'left win)))))
                          (split-window-right nil win)
                        (split-window-below nil win)))
                 win))
            (vterm arg)
            ;; FIXME side windows cannot be divided, if be diveide enabled,
            ;; balance windows afterwards
            ;; (balance-windows (window-parent win))
            )))))

  ;; (defun vterm-new-other-window ()
  ;;   "Open new vterm in other window."
  ;;   (interactive)
  ;;   (vterm-new 'other))

  ;;   (defun vterm-toggle ()
  ;;     "Toggle to show or hide vterm window."
  ;;     (interactive)
  ;;     (let* ((bufs (vterm--get-buffers))
  ;;            (win (car (vterm--get-windows))))
  ;;       (cond
  ;;        ((not bufs) (vterm-other-window))
  ;;        ((not win) (switch-to-buffer-other-window (car bufs)))
  ;;        (t (delete-window win)))))
  ;;
  ;;   (defun vterm-cycle (&optional backward)
  ;;     "Cycle the vterm buffer.
  ;; If BACKWARD is non-nil, cycle vterms buffers reversely"
  ;;     (interactive "P")
  ;;     (let* ((bufs (vterm--get-buffers))
  ;;            (win (car (vterm--get-windows))))
  ;;       (cond
  ;;        ((not bufs) (vterm-other-window))
  ;;        ((not win) (switch-to-buffer-other-window (car bufs)))
  ;;        (t (save-selected-window
  ;;             (let* ((order-bufs (-sort #'string-lessp (-map #'buffer-name bufs) ))
  ;;                    (new-buf
  ;;                     (-> (if backward #'1- #'1+)
  ;;                         (funcall (-elem-index (buffer-name (window-buffer win)) order-bufs))
  ;;                         (mod (length order-bufs))
  ;;                         (nth order-bufs))))
  ;;               (select-window win)
  ;;               (switch-to-buffer new-buf)
  ;;               (message "Switch to %S" new-buf)))))))
  )


;;; repl
(defun my/repl ()
  "Runinig for interactive."
  (interactive)
  (pcase major-mode
    ((or 'emacs-lisp-mode 'lisp-interaction-mode)
     (run-general! eval-region eval-buffer))
    ((or 'js-mode 'js-ts-mode)
     (run-general! nodejs-repl-send-region nodejs-repl-send-buffer))
    ('python-mode (run-python))
    (_ (message "no repl for selected mode"))))

;; node

(leaf nodejs-repl
  :bind
  (:nodejs-repl-mode-map
   ("C-l" . comint-clear-buffer))
  :init
  (defun nodejs-repl-send-region-or-buffer ()
    "docstring"
    (interactive)
    (nodejs-repl)
    (if (use-region-p)
        (nodejs-repl-send-region (region-beginning) (region-end))
      (nodejs-repl-send-region (point-min)(point-max)))))


;; -------------------------- docstr ------------------------------
;; --------------------------- Doc --------------------------------

(defun-mixed!
 devdocs-at-point
 word
 "Search devdocs.io"

 ;; SEE https://devdocs.io/help
 (browse-url (format "https://devdocs.io/#q=%s" (url-hexify-string query)))

 ;; SEE https://github.com/egoist/devdocs-desktop#using-homebrew
 ;; (shell-command (format "open devdocs://search/%s" (url-hexify-string query)))
 )

(defvar cheatsheets-ref-lists nil)
(defun cheatsheets-open (&optional cn)
  "Browser selected reference in cheatsheets.me.
If CN is non-nil, search in zh-CN documentation."
  (interactive "P")
  (when-let* ((ref (completing-read
                   "Select quickref: "
                   (with-memoization cheatsheets-ref-lists
                     (mapcar #'file-name-sans-extension
                             (string-split
                              (shell-command-to-string
                               "gh api --jq '.[].name' /repos/Fechin/reference/contents/source/_posts"))))
                   nil t)))
    (browse-url (concat "https://cheatsheets.zip/" (if cn (concat "zh-CN/docs/" ref ".html") ref)))))


;; run
(leaf quickrun
  :hook (quickrun--mode-hook . quickrun-setup)
  :init
  (setq quickrun-focus-p nil
        quickrun-timeout-seconds 1000000)
  (defun quickrun-setup ()
    "docstring"
    (setq-local cursor-in-non-selected-windows nil))
  :defer-config
  (prependq! quickrun--major-mode-alist
             '((lisp-interaction-mode . "elisp")
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

;; ;; REQUIRE pip install notebook
;; ;; SEE https://github.com/jupyter/jupyter/wiki/Jupyter-kernels
;; (leaf jupyter
;;   :init
;;   (setq jupyter-repl-prompt-margin-width 10
;;         jupyter-eval-use-overlays t))


;; ide


(provide 'init-ide)
;;; init-ide.el ends here

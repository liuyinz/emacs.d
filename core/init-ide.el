;;; init-ide.el --- IDE setting for code -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:


;; REQUIRE brew install libvterm cmake
(leaf vterm
  :hook (vterm-mode-hook . vterm-setup)
  :bind
  ("s-u" . vterm-toggle)
  ("s-i" . vterm-cycle)
  ("s-n" . vterm-new)
  (:vterm-mode-map
   ("M-u" . nil)
   ("M-i" . nil))
  :init
  (setq vterm-term-environment-variable "xterm-kitty")
  (setq vterm-buffer-name "*vterm*")

  (defun vterm-setup ()
    (meow-mode -1)
    (and hl-line-mode (hl-line-mode 'toggle)))

  ;; 1.switch to directory when goto vterm
  ;; TODO rewrite a vterm toggle command to cd parent dir automatically
  ;; (defun my/vterm-toggle (&optional cd)
  ;;   "Toggle vterm window,if C-u"
  ;;   (interactive "P")
  ;;   (vterm-toggle)
  ;;   (when (and cd
  ;;              (eq major-mode 'vterm-mode)
  ;;              vterm-toggle--cd-cmd)
  ;;     (vterm-send-string vterm-toggle--cd-cmd t)
  ;;     (vterm-send-return)))

  )

(leaf vterm-toggle)

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
  (when-let ((ref (completing-read
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
  :init
  (setq quickrun-focus-p nil
        quickrun-timeout-seconds 20)
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

;; TODO add more command
(defun vterm-new (&optional other-window)
  "Create an interactive Vterm buffer with largest number arg.
If OTHER-WINDOW is non-nil, open in another window"
  (interactive "P")
  (let ((len (length (--filter (eq (buffer-local-value 'major-mode it) 'vterm-mode)
                               (buffer-list)))))
    (when (> len 0)
      (cl-incf len)
      (while (get-buffer (format "%s<%d>" vterm-buffer-name len))
        (cl-incf len)))
    (funcall (if other-window #'vterm-other-window #'vterm)
             (and (> len 0) len))))

(defun vterm-cycle (&optional backward)
  "Cycle the vterm buffer."
  (interactive "P")
  (if-let* ((vterm-bufs (->> (buffer-list)
                             (--filter (eq (buffer-local-value 'major-mode it)
                                           'vterm-mode))
                             (-map #'buffer-name)
                             (-sort #'string-lessp)))
            (win (--first (eq (buffer-local-value 'major-mode (window-buffer it))
                              'vterm-mode)
                          (window-list nil 'no-minibuf)))
            (new-buf (nth (mod (funcall (if backward #'1- #'1+)
                                        (-elem-index (buffer-name (window-buffer win))
                                                     vterm-bufs))
                               (length vterm-bufs))
                          vterm-bufs)))
      (progn
        (set-window-buffer win new-buf)
        (message "Switch to %S" new-buf))
    (message "can not find a window display vterm.")))

(provide 'init-ide)
;;; init-ide.el ends here

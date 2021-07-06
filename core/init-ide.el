;;; init-ide.el --- IDE setting for code -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Doc Help
(leaf dash-at-point
  :commands dash-at-point
  :config
  (appendq! dash-at-point-mode-alist
            '((js-mode . "javascript,backbone,angularjs")
              (lisp-interaction-mode . "elisp")
              (css-mode . "css,bootstrap,foundation,less,awesome,emmet"))))

;; Code Check , see@https://www.flycheck.org/en/latest/
(leaf flycheck
  :doc "deps: pkg-info dash"
  :hook
  (prog-mode-hook . flycheck-mode)
  (flycheck-mode-hook . my/flycheck-setup)
  :init
  (setq flycheck-stylelintrc "~/.stylelintrc.json"
        flycheck-tidyrc "~/.tidyrc"
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-indication-mode 'right-margin)

  (defun my/flycheck-setup ()
    "set checker for different buffer"
    (cond
     ((eq major-mode 'sh-mode) (when (and (string-match "\\.sh$" buffer-file-name)
                                          (executable-find "shellcheck"))
                                 (flycheck-select-checker 'sh-shellcheck)))
     ;; pip3 install pylint rather than brew.
     ((eq major-mode 'python-mode) (when (executable-find "pylint")
                                     (flycheck-select-checker 'python-pylint)))
     ((member major-mode '(js-mode js2-mode))
      (when (executable-find "eslint") (flycheck-select-checker 'javascript-eslint)))
     (t nil)))

  ;; ;; may needed by some command
  ;; (leaf pkg-info
  ;;   :doc "deps: epl"
  ;;   :require t)

  ;; ;; Prettify fringe style
  ;; (when (fboundp 'define-fringe-bitmap)
  ;;   (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  ;;     [16 48 112 240 112 48 16] nil nil 'center))

  )


;; Code Running, @https://github.com/emacsorphanage/quickrun#customize
(leaf quickrun
  :commands quickrun quickrun-region
  :init
  (setq quickrun-focus-p nil
        quickrun-timeout-seconds 20)
  :config
  ;; add lisp-interactive-mode to default
  (add-to-list 'quickrun--major-mode-alist '(lisp-interaction-mode . "emacs")))

(defun my-run ()
  "Running Current Buffer."
  (interactive)
  (cond
   ((member major-mode '(markdown-mode gfm-mode)) (grip-start-preview))
   (t (run-general! quickrun-region quickrun))))

(defun my-repl ()
  "Runinig for interactive."
  (interactive)
  (cond
   ((member major-mode '(emacs-lisp-mode lisp-interaction-mode))
    (run-general! eval-region eval-buffer))
   ((member major-mode '(js-mode js2-mode))
    (run-general! nodejs-repl-send-region nodejs-repl-send-buffer))
   ((eq major-mode 'python-mode) (run-python))
   (t (message "no repl for selected mode"))))

;; (defun quickrun-vterm ()
;;   "Quickrun command in vterm."
;;   (interactive)
;;   (let ((buffer buffer-file-name))
;;     (vterm-toggle-cd-show)
;;     (vterm-send-string (format "node %s" buffer))
;;     (vterm-send-return)))

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
  (appendq! format-all-default-formatters '(("HTML" prettier)
                                            ("Shell" (shfmt "-i" "2"))))
  (defun my-format ()
    "Formating files."
    (interactive)
    (whitespace-cleanup)
    (format-all-buffer))
  )

(leaf vterm
  :init
  (setenv "COLORTERM" "truecolor")
  (setq vterm-always-compile-module t
        vterm-kill-buffer-on-exit t
        vterm-clear-scrollback-when-clearing nil
        vterm-max-scrollback 10000)
  :defer-config
  (add-to-list 'vterm-keymap-exceptions "C-o"))

(leaf vterm-toggle
  :commands vterm-toggle
  :init
  (setq vterm-toggle-fullscreen-p nil))

(provide 'init-ide)

;;; init-ide.el ends here

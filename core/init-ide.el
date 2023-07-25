;;; init-ide.el --- IDE setting for code -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:


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

;; --------------------------- Lint -------------------------------

;; SEE https://www.flycheck.org/en/latest/
;; PR https://github.com/flycheck/flycheck/pull/1896
(leaf flycheck
  :hook ((prog-mode-hook yaml-mode-hook) . flycheck-mode)
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 3
        flycheck-indication-mode nil)
  ;; rc files
  (setq flycheck-stylelintrc ".stylelintrc.json"
        flycheck-tidyrc ".tidyrc")

  ;; ISSUE https://github.com/flycheck/flycheck/issues/1974#issuecomment-1343495202
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff"
              "--format=text"
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-ruff)

  (add-hook 'flycheck-mode-hook #'my/flycheck-setup)
  (defun my/flycheck-setup ()
    "set checker for different MODE"
    (cl-case major-mode
      (sh-mode
       (when (and (executable-find "shellcheck")
                  (member sh-shell '(sh bash)))
         (flycheck-select-checker 'sh-shellcheck)))

      ;; REQUIRE brew install jq
      ((json-mode js-json-mode)
       (when (executable-find "jq")
         (flycheck-select-checker 'json-jq)))

      (python-mode
       ;; REQUIRE brew install ruff
       (or (and (executable-find "ruff")
                (flycheck-select-checker 'python-ruff))
           (and (executable-find "pyright")
                (flycheck-select-checker 'python-pyright))))

      ((emacs-lisp-mode lisp-interaction-mode)
       (progn
         (if (buffer-file-name)
             (flycheck-package-setup)
           (flycheck-disable-checker 'emacs-lisp-checkdoc)
           (flycheck-disable-checker 'emacs-lisp-package))
         (flycheck-relint-setup)))

      ((mhtml-mode html-mode web-mode)
       (when (executable-find "tidy")
         (flycheck-add-mode 'html-tidy major-mode)
         (flycheck-select-checker 'html-tidy)))

      (js-mode
       ;; NOTE disable `javascript-eslint' in `mhtml-mode'
       (when (and (string= "js" (file-name-extension (buffer-name)))
                  (executable-find "eslint"))
         (flycheck-select-checker 'javascript-eslint)))

      ;; ;; BUG may run: nil
      ;; (css-mode
      ;;  ;; NOTE disable `css-stylelint' in `mhtml-mode'
      ;;  (when (and (string= "css" (file-name-extension (buffer-name)))
      ;;             (executable-find "stylelint"))
      ;;    (flycheck-select-checker 'css-stylelint)))

      (scss-mode
       (when (executable-find "stylelint")
         (flycheck-select-checker 'scss-stylelint)))

      (less-mode
       (when (executable-find "stylelint")
         (flycheck-select-checker 'less-stylelint)))

      (sass-mode
       (flycheck-select-checker 'sass))

      ((cperl-mode perl-mode)
       (when (executable-find "perlcritic")
         (flycheck-select-checker 'perl-perlcritic)))

      ;; REQUIRE pip install yamllint
      (yaml-mode
       (when (executable-find "yamllint")
         (flycheck-select-checker 'yaml-yamllint)))

      (t nil)))

  )

;; --------------------------- Run --------------------------------

(leaf quickrun
  :init
  (setq quickrun-focus-p nil
        quickrun-timeout-seconds 20)
  :defer-config
  (prependq! quickrun--major-mode-alist '((lisp-interaction-mode . "emacs"))))

(defun my/run ()
  "Running Current Buffer."
  (interactive)
  (cond
   ((member major-mode '(markdown-mode gfm-mode)) (grip-start-preview))
   ((member major-mode '(web-mode html-mode mhtml-mode)) (imp-visit-buffer))
   (t (run-general! quickrun-region quickrun))))

;; -------------------------- Format ------------------------------

(leaf editorconfig
  :hook (sh-mode-hook . editorconfig-mode))

;; ;; ISSUE https://github.com/lassik/emacs-format-all-the-code/issues/220
;; (leaf format-all
;;   ;; :require t
;;   :init
;;   (setq format-all-debug t)
;;   (advice-add 'format-all-buffer :before #'format-all-ensure-formatter)
;;
;;   (defun my/format ()
;;     "Formatting current buffer."
;;     (interactive)
;;     (cl-case major-mode
;;       (gitconfig-mode (run-general! indent-region indent-whole-buffer))
;;       (t (run-general! format-all-region format-all-buffer))))
;;   :defer-config
;;   (prependq! format-all-default-formatters
;;              '(("JSONC" prettier)
;;                ;; SEE https://google.github.io/styleguide/shellguide.html
;;                ("Shell" (shfmt "-i" "2" "-bn" "-ci"))))
;;   )

(leaf apheleia
  :require t
  :init
  (setq apheleia-hide-log-buffers t)
  (defun my/format ()
    "Formatting current buffer."
    (interactive)
    (cl-case major-mode
      ((gitconfig-mode emacs-lisp-mode lisp-interaction-mode)
       (run-general! indent-region indent-whole-buffer))
      (t (call-interactively #'apheleia-format-buffer))))
  :config
  (alist-set! apheleia-formatters
              '((shfmt . ("shfmt" "-i" "2" "-bn" "-ci"))))
  (alist-set! apheleia-mode-alist
              '((python-mode . (black isort))
                (sh-mode . shfmt))))

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

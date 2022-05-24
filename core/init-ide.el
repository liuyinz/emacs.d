;;; init-ide.el --- IDE setting for code -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; --------------------------- Doc --------------------------------

(define-command-mixed
  devdocs-at-point
  word
  "Search devdocs.io"

  ;; SEE https://devdocs.io/help
  ;; (browse-url (format "https://devdocs.io/#q=%s" (url-hexify-string query)))

  ;; SEE https://github.com/egoist/devdocs-desktop#using-homebrew
  (shell-command (format "open devdocs://search/%s" (url-hexify-string query)))
  )

;; --------------------------- Lint -------------------------------

;; SEE https://www.flycheck.org/en/latest/
;; PR https://github.com/flycheck/flycheck/pull/1896
(leaf flycheck
  :hook (prog-mode-hook . flycheck-mode)
  :init
  (setq flycheck-stylelintrc ".stylelintrc.json"
        flycheck-tidyrc ".tidyrc"
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-indication-mode 'right-margin)

  (defun my/flycheck-setup ()
    "set checker for different buffer"
    (cl-case major-mode
      (sh-mode
       (when (and (executable-find "shellcheck")
                  (member sh-shell '(sh bash)))
         (flycheck-select-checker 'sh-shellcheck)))

      ;; REQUIRE brew install jq
      ((json-mode jsonian-mode)
       (when (executable-find "jq")
         (flycheck-select-checker 'json-jq)))

      ;; REQUIRE pip3 install pylint rather than brew.
      (python-mode
       (when (executable-find "pylint")
         (flycheck-select-checker 'python-pylint)))

      (emacs-lisp-mode
       (progn
         (flycheck-package-setup)
         (flycheck-relint-setup)))

      ((mhtml-mode html-mode web-mode)
       (when (executable-find "tidy")
         (flycheck-add-mode 'html-tidy major-mode)
         (flycheck-select-checker 'html-tidy)))

      (css-mode
       ;; NOTE disable `css-stylelint' in `mhtml-mode'
       (when (and (string= "css" (file-name-extension (buffer-name)))
                  (executable-find "stylelint"))
         (flycheck-select-checker 'css-stylelint)))

      (js-mode
       ;; NOTE disable `javascript-eslint' in `mhtml-mode'
       (when (and (string= "js" (file-name-extension (buffer-name)))
                  (executable-find "eslint"))
         (flycheck-select-checker 'javascript-eslint)))

      ;; (js2-mode
      ;;  (when (executable-find "eslint")
      ;;    (flycheck-add-mode 'javascript-eslint major-mode)
      ;;    (flycheck-select-checker 'javascript-eslint)))

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
      (t nil)))

  (add-hook 'flycheck-mode-hook #'my/flycheck-setup))

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
   (t (run-general! quickrun-region quickrun))))

(defun my/repl ()
  "Runinig for interactive."
  (interactive)
  (cl-case major-mode
    ((emacs-lisp-mode lisp-interaction-mode)
     (run-general! eval-region eval-buffer))
    ((js-mode js2-mode)
     (run-general! nodejs-repl-send-region nodejs-repl-send-buffer))
    (python-mode (run-python))
    (t (message "no repl for selected mode"))))

;; -------------------------- Format ------------------------------

(leaf editorconfig
  :hook (shell-mode-hook . editorconfig-mode))

(leaf apheleia
  :defer-config
  (setq apheleia-hide-log-buffers t)
  (prependq! apheleia-mode-alist
             '((jsonian-mode . prettier)))

  (prependq! apheleia-formatters
             '((shfmt . ("shfmt" "-i" "2" "-bn" "-ci"))))
  
  )

(defun my/format ()
  "Formating current buffer."
  (interactive)
  (cl-case major-mode
    ((emacs-lisp-mode lisp-interaction-mode)
     (indent-whole-buffer))
    (t
     (call-interactively #'apheleia-format-buffer))))


(provide 'init-ide)
;;; init-ide.el ends here

;;; init-ide.el --- IDE setting for code -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------ Tree-sitter ----------------------------

(setq treesit-extra-load-path
      `(,(concat my/dir-lib "tree-sitter-module/dist")))

;; -------------------------- docstr ------------------------------
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
  :hook ((prog-mode-hook yaml-mode-hook) . flycheck-mode)
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 3
        flycheck-indication-mode nil)
  ;; rc files
  (setq flycheck-stylelintrc ".stylelintrc.json"
        flycheck-tidyrc ".tidyrc")

  (add-hook 'flycheck-mode-hook #'my/flycheck-setup)
  (defun my/flycheck-setup ()
    "set checker for different MODE"
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
       (when (executable-find "pyright")
         (flycheck-select-checker 'python-pyright)))

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

(defun my/repl ()
  "Runinig for interactive."
  (interactive)
  (cl-case major-mode
    ((emacs-lisp-mode lisp-interaction-mode)
     (run-general! eval-region eval-buffer))
    ((js-mode js2-mode jsonian-mode)
     (run-general! nodejs-repl-send-region nodejs-repl-send-buffer))
    (python-mode (run-python))
    (t (message "no repl for selected mode"))))

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
(provide 'init-ide)
;;; init-ide.el ends here

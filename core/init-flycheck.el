;;; init-flycheck.el --- config for linting -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2023-08-01 17:06:19

;;; Commentary:

;;; Code:

;; --------------------------- Lint -------------------------------

;; SEE https://www.flycheck.org/en/latest/
;; PR https://github.com/flycheck/flycheck/pull/1896
(leaf flycheck
  :hook ((prog-mode-hook yaml-ts-mode-hook) . flycheck-mode)
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
    :modes (python-mode python-ts-mode))
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
       (flycheck-select-checker 'json-python-json))

      ((python-mode python-ts-mode)
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

      ((js-ts-mode js-mode)
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
      (yaml-ts-mode
       (when (executable-find "yamllint")
         (flycheck-select-checker 'yaml-yamllint)))

      (t nil)))

  )

(provide 'init-flycheck)
;;; init-flycheck.el ends here

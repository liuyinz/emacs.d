;;; init-ide.el --- IDE setting for code -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;;; --------------------------- Doc --------------------------------

(leaf dash-at-point
  :commands dash-at-point
  :config
  (appendq! dash-at-point-mode-alist
            '((js-mode . "javascript,backbone,angularjs")
              (js2-mode . "javascript,backbone,angularjs")
              (lisp-interaction-mode . "elisp")
              (css-mode . "css,bootstrap,foundation,less,awesome,emmet")
              (jq-mode . "jq"))))

;;; --------------------------- Lint -------------------------------

;; SEE https://www.flycheck.org/en/latest/
;; PR https://github.com/flycheck/flycheck/pull/1896
(leaf flycheck
  :doc "deps: pkg-info dash"
  :hook (prog-mode-hook . flycheck-mode)
  :init
  (setq flycheck-stylelintrc "~/.stylelintrc.json"
        flycheck-tidyrc "~/.tidyrc"
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 2
        flycheck-indication-mode 'right-margin)

  (defun my/flycheck-setup ()
    "set checker for different buffer"
    (cl-case major-mode
      (sh-mode
       (when (and (executable-find "shellcheck")
                  (member sh-shell '(sh bash)))
         (flycheck-select-checker 'sh-shellcheck)))

      ;; REQUIRE pip3 install pylint rather than brew.
      (python-mode
       (when (executable-find "pylint")
         (flycheck-select-checker 'python-pylint)))

      ((emacs-lisp-mode lisp-interaction-mode)
       (leaf flycheck-relint
         :require t
         :config (flycheck-relint-setup)))

      ((js-mode js2-mode json-mode jsonc-mode)
       (when (executable-find "eslint")
         (flycheck-add-mode 'javascript-eslint major-mode)
         (flycheck-select-checker 'javascript-eslint)))

      ((mhtml-mode html-mode web-mode)
       (when (executable-find "tidy")
         (flycheck-add-mode 'html-tidy major-mode)
         (flycheck-select-checker 'html-tidy)))

      (t nil)))

  (add-hook 'flycheck-mode-hook #'my/flycheck-setup)

  (leaf pkg-info
    :doc "deps: epl"
    ;; needed by command below
    :commands pkg-info-version-info)

  )

;;; --------------------------- Run --------------------------------

(leaf quickrun
  :commands quickrun quickrun-region
  :init
  (setq quickrun-focus-p nil
        quickrun-timeout-seconds 20)
  :config
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

;;; -------------------------- Format ------------------------------

(leaf editorconfig
  :hook (shell-mode-hook . editorconfig-mode))

(leaf format-all
  :doc "deps: inheritenv language-id"
  :commands format-all-buffer format-all-region
  :init
  (setq format-all-debug t)
  (advice-add 'format-all-buffer :before #'format-all-ensure-formatter)

  ;; silent ensure message
  (advice-add #'format-all-ensure-formatter :around #'ad/silent-message)

  (defun my/format ()
    "Formating files."
    (interactive)
    (cl-case major-mode
      (gitconfig-mode (run-general! indent-region indent-whole-buffer))
      (t (run-general! format-all-region format-all-buffer))))
  :config
  ;; SEE https://google.github.io/styleguide/shellguide.html
  (prependq! format-all-default-formatters '(("HTML" prettier)
                                             ("JSONC" prettier)
                                             ("Shell" (shfmt "-i" "2" "-bn" "-ci"))))
  )

(provide 'init-ide)
;;; init-ide.el ends here

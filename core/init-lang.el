;;; init-lang.el --- language setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;;; ------------------------- Builtin ------------------------------

(leaf sh-script
  :init
  (setq sh-basic-offset 2
        sh-shell-file (executable-find "bash"))

  ;; HACK create `bash-mode-hook'
  (defvar bash-mode-hook nil "Hook run after enter sh-mode[Bash]")
  (defun run-bash-mode-hook (&rest _)
    "Run `bash-mode-hook' when enabled."
    (when (eq sh-shell 'bash)
      (run-hooks 'bash-mode-hook)))
  (add-hook 'sh-mode-hook #'run-bash-mode-hook)

  )

(leaf conf-mode :mode "\\.\\(ini\\|conf\\|.*rc\\)\\'" )

(leaf python-mode :mode "\\.pythonrc\\'")

;;; -------------------------- Plugin ------------------------------

(leaf json-mode
  :doc "deps : json-snatcher"
  :mode
  ("\\.jsonc\\'" . jsonc-mode)
  ("\\.json\\'"  . json-mode)
  ("\\.versionrc\\'" . json-mode)
  )

(leaf jq-mode
  :doc "deps : brew install jq"
  :commands jq-interactivly
  :mode "\\.jq\\'")

(leaf lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (setq lua-indent-level 2)
  (setq lua-indent-string-contents t)
  (setq lua-prefix-key nil))

(leaf csv-mode :mode "\\.[Cc][Ss][Vv]\\'")

(leaf yaml-mode :mode "\\.yaml\\'" "\\.yml\\'")

(leaf vimrc-mode :mode "\\.vim\\'" "\\vimrc\\'")

(leaf plantuml-mode :mode "\\.plantuml\\'")

(leaf logview
  :doc "deps : datetime extmap"
  :mode (("\\.log\\(?:\\.[0-9]+\\)?\\'" . logview-mode))
  :hook (logview-mode-hook . auto-revert-tail-mode))

;; (leaf mermaid-mode :require t)

;;; -------------------------- Macos -------------------------------

(leaf applescript-mode
  :mode "\\.\\(applescript\\|scpt\\)\\'"
  :interpreter "osascript"
  :init
  ;; TODO add `indent-line-function'
  (setq as-indent-offset 2))

(leaf osx-plist :mode "\\.plist\\'")

(provide 'init-lang)
;;; init-lang.el ends here

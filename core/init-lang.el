;;; init-lang.el --- language setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf js-mode
  :interpreter ("node" . js-mode)
  :init
  (setq js-indent-level 2
        js-chain-indent t
        js-jsx-indent-level 2
        ;; js-js-tmpdir
        ;; js-jsx-syntax t
        ))

(leaf nodejs-repl
  :commands (nodejs-repl
             nodejs-repl-send-line
             nodejs-repl-send-region
             nodejs-repl-send-last-expression))

(leaf typescript-mode :mode "\\.ts[x]\\'")

(leaf css-mode
  :init
  (setq css-indent-offset 2)
  (setq css-fontify-colors nil))

(leaf emmet-mode
  :hook ((css-mode-hook html-mode-hook sgml-mode-hook) . emmet-mode)
  :init
  (setq emmet-indentation 2
        emmet-insert-flash-time -1
        ;; emmet-indent-after-insert t
        ;; emmet-move-cursor-between-quotes nil
        emmet-postwrap-goto-edit-point t))
;; ;; SCSS mode
;; (leaf scss-mode
;;   :init (setq scss-compile-at-save nil))

;; (leaf less-css-mode
;;   :init (setq less-css-compile-at-save nil))

(leaf sh-script
  :init (setq sh-basic-offset 2))

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

(leaf python-mode :mode "\\.pythonrc\\'")

(leaf osx-plist :mode "\\.plist\\'")

(leaf logview
  :doc "deps : datetime extmap"
  :mode (("\\.log\\(?:\\.[0-9]+\\)?\\'" . logview-mode))
  :hook (logview-mode-hook . auto-revert-tail-mode))

;; (leaf mermaid-mode :require t)

(provide 'init-lang)
;;; init-lang.el ends here

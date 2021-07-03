;;; init-tool.el --- other tools -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(leaf rg
  :doc "deps : transient wgrep"
  :commands rg-menu
  :init
  (setq rg-ignore-case 'smart)
  :config
  (rg-enable-menu))

;; proxy wrapper
(leaf with-proxy
  :require t
  :init
  ;; TODO set embark with proxy
  (setq with-proxy-http-server my-proxy))

;; (use-package leetcode
;;   :doc "pip3 install my-cookies"
;;   :commands leetcode
;;   :init
;;   (setq leetcode-prefer-language "javascript"
;;         leetcode-prefer-sql "mysql"
;;         leetcode-save-solutions t
;;         leetcode-directory "~/Documents/repo/leetcode"))

;; google-translate
(leaf go-translate
  :commands go-traslate go-translate-kill-ring-save go-translate-echo-area
  :init
  (setq go-translate-base-url "https://translate.google.cn"
        go-translate-local-language "zh-CN"
        go-translate-token-current (cons 430675 2721866130)))

;; 简繁转换
(leaf opencc
  :commands opencc-replace-at-point opencc-print-buffer
  :doc "deps: brew install opencc")

;; IRC
(leaf erc
  :defvar erc-autojoin-channels-alist
  :init (setq erc-rename-buffers t
              erc-interpret-mirc-color t
              erc-lurker-hide-list '("JOIN" "PART" "QUIT")
              erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))

(leaf keyfreq
  :hook
  (after-init-hook . keyfreq-mode)
  (keyfreq-mode-hook . keyfreq-autosave-mode)
  :init
  (setq keyfreq-excluded-regexp
        '(;; built-in
          "\\`\\(mouse\\|scroll\\|keyboard\\|clipboard\\|minibuffer\\|package\\)-.*\\'"
          "\\`\\(backward\\|forward\\|move\\|isearch\\|describe\\|eval\\|exit\\)-.*\\'"
          "\\`\\(delete\\|kill\\|quit\\|save\\|abort\\|self\\|term\\|xterm\\)-.*\\'"
          "\\`\\(pcomplete\\|y-or-n-p\\|previous\\|next\\|right\\|left\\|zap\\)-.*\\'"
          "\\`\\(suspend\\|execute-extended\\|transient\\|indent\\electric\\)-.*\\'"
          ;; third-party
          "\\`\\(keyfreq\\|vertico\\|evil\\|company\\|vundo\\|yas\\|vterm\\)-.*\\'"
          "\\`\\(magit-section\\|helpful\\|web-mode\\|ilog\\|hungry-delete\\)-.*\\'"
          "\\`\\(markdown-insert\\|markdown-table\\|my/transient\\)-.*\\'"
          ))

  (setq keyfreq-excluded-commands
        '(undefined
          ignore
          mwheel-scroll
          hippie-expand
          newline
          newline-and-indent
          yank
          choose-completion
          comint-previous-input
          comint-send-input
          embark-act
          dired ;; nothing to optimize in dired
          dired-do-async-shell-command
          dired-find-file
          goto-line
          ispell-minor-check
          js-mode
          magit-mode-bury-buffer
          markdown-outdent-or-delete
          my-company-yasnippet
          org-beginning-of-line
          org-ctrl-c-ctrl-c
          org-cycle
          org-delete-backward-char
          org-end-of-line
          org-force-self-insert
          org-return
          org-self-insert-command
          org-todo
          orgtbl-self-insert-command
          )))

(provide 'init-tool)

;;; init-tool.el ends here

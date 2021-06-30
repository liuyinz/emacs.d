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
  (setq keyfreq-excluded-regexp '(
                                  ;; built-in
                                  "\\`mouse-.*\\'"
                                  "\\`clipboard-.*\\'"
                                  "\\`keyboard-.*\\'"
                                  "\\`scroll-.*\\'"
                                  "\\`y-or-n-p-.*\\'"
                                  "\\`minibuffer-.*\\'"
                                  "\\`backward-.*\\'"
                                  "\\`forward-.*\\'"
                                  "\\`move-.*\\'"
                                  "\\`isearch-.*\\'"
                                  "\\`describe-.*\\'"
                                  ;; third-party
                                  "\\`keyfreq-.*\\'"
                                  "\\`vertico-.*\\'"
                                  "\\`evil-.*\\'"
                                  "\\`company-.*\\'"
                                  "\\`vundo-.*\\'"
                                  "\\`yas-.*\\'"
                                  "\\`web-mode-.*\\'"
                                  ))

  (setq keyfreq-excluded-commands
        '(self-insert-command
          abort-recursive-edit
          comint-previous-input
          comint-send-input
          delete-other-windows
          delete-backward-char
          dired ;; nothing to optimize in dired
          dired-do-async-shell-command
          dired-find-file
          electric-pair-delete-pair
          erase-message-buffer
          execute-extended-command
          exit-minibuffer
          ffip
          goto-line
          hippie-expand
          hungry-delete-backward
          ignore
          indent-new-comment-line
          ispell-minor-check
          js-mode
          kill-sentence
          left-char
          magit-mode-bury-buffer
          magit-section-toggle
          magit-section-forward
          magit-section-backward
          markdown-exdent-or-delete
          markdown-outdent-or-delete
          mwheel-scroll
          my-company-yasnippet
          my-setup-develop-environment
          newline
          newline-and-indent
          next-history-element
          next-line
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
          package-menu-execute
          pcomplete
          previous-history-element
          previous-line
          push-button
          pwd
          quit-window
          right-char
          rjsx-electric-gt
          rjsx-electric-lt
          save-buffer
          save-buffers-kill-terminal
          sis-get-english
          sis-set-english
          smarter-move-beginning-of-line
          suspend-frame
          term-send-raw
          turnon-keyfreq-mode
          undefined ;; lambda function
          vterm-clear
          vterm-self-insert
          w3m-goto-url
          w3m-next-anchor
          w3m-view-this-url
          wgrep-finish-edit
          xterm-paste
          yaml-electric-backspace
          yank
          ))
  )

(provide 'init-tool)

;;; init-tool.el ends here

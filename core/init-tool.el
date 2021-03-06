;;; init-tool.el --- other tools -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; Regexp
;; NOTE see@https://www.emacswiki.org/emacs/RegularExpression
(leaf xr
  :commands xr xr-pp xr-lint
  ;; :init
  ;; (display-buffer-pop-up-window)
  )

(leaf relint
  :doc "deps: xr"
  :commands relint-file relint-directory relint-current-buffer)

;; Disable modes below when enter `vlf-mode'
(leaf vlf
  :commands vlf vlf-mode
  :hook (vlf-mode-hook . (lambda ()
                           (mode-hook-toggle vlf-mode vlf-disable-modes t)))
  :init
  (setq large-file-warning-threshold (* 1024 1024))
  (setq vlf-save-in-place t
        vlf-batch-size (* 1 1024 1024))
  (defvar vlf-disable-modes '(flycheck-mode
                              auto-revert-mode
                              diff-hl-flydiff-mode))
  :config
  (leaf vlf-setup :require t :init (setq vlf-application 'ask)))

;; Proxy
(leaf proxy-mode
  :commands global-proxy-mode proxy-mode
  :init
  (setq proxy-mode-emacs-http-proxy `(("http"     . ,(getenv "HTTP"))
                                      ("https"    . ,(getenv "HTTP"))
                                      ("ftp"      . ,(getenv "HTTP"))
                                      ("no_proxy" . "127.0.0.1")
                                      ("no_proxy" . "^.*\\(baidu\\|sina)\\.com")))

  (setq proxy-mode-emacs-socks-proxy `("Default server"
                                       ,(substring (getenv "SOCKS") 0 -5)
                                       ,(substring (getenv "SOCKS") -4 nil)
                                       5)))

;; TODO set embark with proxy
(leaf with-proxy
  :commands with-proxy with-proxy-url
  :init (setq with-proxy-http-server (getenv "HTTP")))

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

(leaf interaction-log
  :hook (ilog-log-buffer-mode-hook . (lambda ()
                                       (setq ilog-display-state 'messages)
                                       (ilog-toggle-view)))
  :init
  (setq ilog-log-max nil)
  (defun toggle-keylog ()
    "Toggle keybinds log."
    (interactive)
    (require 'interaction-log)
    (unless (bufferp ilog-buffer-name)
      (interaction-log-mode))
    (with-current-buffer ilog-buffer-name
      (let ((win (get-buffer-window (current-buffer))))
        (if (not (windowp win))
            (progn
              (unless interaction-log-mode
                (interaction-log-mode))
              (display-buffer (current-buffer)))
          (if interaction-log-mode
              (progn
                (interaction-log-mode -1)
                (delete-window win))
            (interaction-log-mode)))))))

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

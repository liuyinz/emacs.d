;;; init-tool.el --- other tools -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; --------------------------- Blog -------------------------------

;; TODO customize easy-hugo-buffer, refactor easy-hugo, tag filter
(use-package easy-hugo
  :init
  (setq easy-hugo-basedir  "~/Code/blog/"
        easy-hugo-postdir "content/posts/"
        easy-hugo-url  "https://liuyinz.github.io/"
        easy-hugo-preview-url "http://localhost:1313/"
        easy-hugo-server-flags "-D"
        easy-hugo-no-help t)
  :config
  (easy-hugo-enable-menu))

;; ------------------------ Translate -----------------------------

;; SEE https://www.emacswiki.org/emacs/RegularExpression
(use-package xr)

;; REQUIRE  brew install mpg123
(use-package fanyi
  :custom
  (fanyi-providers '(fanyi-longman-provider
                     fanyi-youdao-thesaurus-provider
                     fanyi-haici-provider))
  )

;; REQUIRE brew install opencc
(use-package opencc)

(use-package pandoc-mode)

;; ------------------------- Network ------------------------------

(use-package proxy-mode
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
(use-package with-proxy
  :demand t
  :init (setq with-proxy-http-server (getenv "HTTP")))

;; ------------------------- Keystroke -----------------------------

(use-package interaction-log
  :hook (ilog-log-buffer-mode-hook . (lambda ()
                                       (setq ilog-display-state 'messages)
                                       (ilog-toggle-view)))
  :init
  (setq ilog-log-max nil)
  ;; FIXME press twice should be avoided.
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

(use-package keyfreq
  :hook ((after-init-hook . keyfreq-mode)
         (keyfreq-mode-hook . keyfreq-autosave-mode))
  :init
  (setq keyfreq-excluded-regexp
        '(;; built-in
          "\\`\\(mouse\\|scroll\\|keyboard\\|clipboard\\|minibuffer\\|package\\)-.*\\'"
          "\\`\\(backward\\|forward\\|move\\|isearch\\|describe\\|eval\\|exit\\)-.*\\'"
          "\\`\\(delete\\|kill\\|quit\\|save\\|abort\\|self\\|term\\|xterm\\)-.*\\'"
          "\\`\\(pcomplete\\|y-or-n-p\\|previous\\|next\\|right\\|left\\|zap\\)-.*\\'"
          "\\`\\(suspend\\|execute-extended\\|transient\\|indent\\electric\\)-.*\\'"
          "\\`\\(profiler-report\\|compilation\\|smerge\\)-.*\\'"
          ;; third-party
          "\\`\\(keyfreq\\|vertico\\|evil\\|company\\|vundo\\|yas\\|vterm\\)-.*\\'"
          "\\`\\(magit-section\\|helpful\\|web-mode\\|ilog\\|hungry-delete\\)-.*\\'"
          "\\`\\(markdown-insert\\|markdown-table\\|my/transient\\|sis-set\\)-.*\\'"
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
          goto-line
          ispell-minor-check
          js-mode
          magit-mode-bury-buffer
          markdown-outdent-or-delete
          my/company-yasnippet
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
          handle-switch-frame
          )))

;; ;; REQUIRE pip3 install my-cookies
;; (use-package leetcode
;;   :init
;;   (setq leetcode-prefer-language "javascript"
;;         leetcode-prefer-sql "mysql"
;;         leetcode-save-solutions t
;;         leetcode-directory "~/Documents/repo/leetcode"))

(provide 'init-tool)
;;; init-tool.el ends here

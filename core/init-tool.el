;;; init-tool.el --- other tools -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; --------------------------- Blog -------------------------------

;; TODO customize easy-hugo-buffer, refactor easy-hugo, tag filter
(leaf easy-hugo
  :init
  (setq easy-hugo-basedir  "~/Code/blog/"
        easy-hugo-postdir "content/posts/"
        easy-hugo-url  "https://liuyinz.github.io/"
        easy-hugo-preview-url "http://localhost:1313/"
        easy-hugo-server-flags "-D"
        easy-hugo-no-help t)
  :defer-config
  (easy-hugo-enable-menu))

;; ------------------------ Translate -----------------------------

;; SEE https://www.emacswiki.org/emacs/RegularExpression
(leaf xr)

;; REQUIRE  brew install mpg123
(leaf fanyi
  :custom
  (fanyi-providers '(fanyi-longman-provider
                     fanyi-youdao-thesaurus-provider
                     fanyi-haici-provider))
  )

;; REQUIRE brew install opencc
(leaf opencc)

(leaf pinyinlib :require t)

(leaf pandoc-mode)

;; ------------------------- Network ------------------------------

(leaf proxy-mode
  :init
  (when-let ((http (getenv "HTTP")))
    (setq proxy-mode-env-http-proxy (concat "http://" http))
    (setq proxy-mode-emacs-http-proxy
          `(("http"     . ,http)
            ("https"    . ,http)
            ("ftp"      . ,http)
            ("no_proxy" . "127.0.0.1"))))

  (when-let ((sock (getenv "SOCKS")))
    (setq proxy-mode-emacs-socks-proxy
          `("Default server"
            ,(substring sock 0 -5)
            ,(substring sock -4 nil)
            5)))
  )

;; TODO set embark with proxy
(leaf with-proxy
  :require t
  :init
  (when-let ((http (getenv "HTTP")))
    (setq with-proxy-http-server http)))

;; -------------------------- record ------------------------------

(leaf keyfreq
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
          "\\`\\(keyfreq\\|vertico\\|evil\\|corfu\\|vundo\\|yas\\|vterm\\)-.*\\'"
          "\\`\\(magit\\|helpful\\|web-mode\\|ilog\\|hungry-delete\\|meow\\)-.*\\'"
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

(provide 'init-tool)
;;; init-tool.el ends here

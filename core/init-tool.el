;;; init-tool.el --- other tools -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(leaf markdown-mode
  :doc "deps : edit-indirect"
  :mode
  ("README\\.md\\'" .gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc"))

(leaf with-proxy
  :require t
  :init
  ;; TODO set embark with proxy
  (setq with-proxy-http-server my-proxy))

;; (use-package easy-hugo
;;   :custom ((easy-hugo-basedir  "~/gh/jiacai2050.github.io/")
;;        (easy-hugo-url  "https://liujiacai.net")
;;            (easy-hugo-default-ext ".org")
;;            (easy-hugo-bloglist '(((easy-hugo-basedir . "~/gh/en-blog/")
;;                                   (easy-hugo-default-ext ".org")
;;                               (easy-hugo-url . "https://en.liujiacai.net"))))))

(leaf dash-at-point)
(leaf writeroom :commands writeroom-mode)

;; (use-package leetcode
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
  (setq keyfreq-excluded-commands
        '(self-insert-command
          abort-recursive-edit
          ace-jump-done
          ace-jump-move
          ace-window
          avy-goto-line
          awesome-pair-open-round
          awesome-pair-close-round
          backward-char
          backward-delete-char-untabify
          backward-kill-word
          backward-word
          clipboard-kill-ring-save
          comint-previous-input
          comint-send-input
          company-complete-common
          company-complete-common-or-cycle
          company-complete-number
          company-complete-selection
          company-ignore
          company-search-printing-char
          company-select-next
          company-select-previous
          counsel-describe-variable
          delete-other-windows
          delete-backward-char
          describe-variable
          dired ;; nothing to optimize in dired
          dired-do-async-shell-command
          dired-find-file
          diredp-next-line
          diredp-previous-line
          electric-pair-delete-pair
          erase-message-buffer
          execute-extended-command
          exit-minibuffer
          evil-a-WORD
          evil-append
          evil-backward-char
          evil-backward-word-begin
          evil-change
          evil-change-line
          evil-complete-next
          evil-complete-previous
          evil-delete
          evil-delete-backward-char-and-join
          evil-delete-char
          evil-delete-line
          evil-emacs-state
          evil-end-of-line
          evil-escape-emacs-state
          evil-escape-insert-state
          evil-escape-isearch
          evil-escape-minibuffer
          evil-escape-motion-state
          evil-escape-visual-state
          evil-ex
          evil-ex-command
          evil-ex-completion
          evil-ex-delete-backward-char
          evil-exit-emacs-state
          evil-exit-visual-state
          evil-filepath-inner-text-object
          evil-filepath-outer-text-object
          evil-find-char
          evil-find-char-to
          evil-first-non-blank
          evil-force-normal-state
          evil-forward-char
          evil-forward-word-begin
          evil-forward-word-end
          evil-goto-definition
          evil-goto-first-line
          evil-goto-line
          evil-goto-mark-line
          evil-indent
          evil-inner-WORD
          evil-inner-double-quote
          evil-inner-single-quote
          evil-inner-word
          evil-insert
          evil-join
          evil-jump-backward
          evil-jump-forward
          evil-mc-make-and-goto-next-match
          evil-mouse-drag-region
          evil-next-line
          evil-next-visual-line
          evil-normal-state
          evil-open-below
          evil-paste-after
          evil-paste-before
          evil-previous-line
          evil-previous-visual-line
          evil-record-macro
          evil-redo
          evil-repeat
          evil-replace
          evil-ret
          evil-scroll-page-down
          evil-scroll-page-up
          evil-search-forward
          evil-search-next
          evil-search-word-forward
          evil-send-leader
          evil-send-localleader
          evil-set-marker
          evil-substitute
          evil-undo
          evil-visual-block
          evil-visual-char
          evil-visual-line
          evil-visual-screen-line
          evil-yank
          ffip
          forward-char
          forward-word
          gnus
          gnus-summary-exit
          gnus-summary-next-page
          gnus-summary-scroll-up
          gnus-topic-select-group
          goto-line
          hippie-expand
          hungry-delete-backward
          ido-complete
          ido-delete-backward-updir
          ido-exit-minibuffer
          ido-switch-buffer
          ignore
          indent-new-comment-line
          isearch-abort
          isearch-backward-regexp
          isearch-cancel
          isearch-delete-char
          isearch-exit
          isearch-forward-regexp
          isearch-other-control-char
          isearch-other-meta-char
          isearch-printing-char
          isearch-repeat-forward
          isearch-ring-retreat
          ispell-minor-check
          ivy-backward-delete-char
          ivy-backward-kill-word
          ivy-done
          ivy-next-line
          ivy-occur
          ivy-occur-next-line
          ivy-occur-press-and-switch
          ivy-occur-previous-line
          ivy-previous-line
          ivy-wgrep-change-to-wgrep-mode
          js-mode
          js2-line-break
          keyboard-escape-quit
          keyboard-quit
          keyfreq-mode
          keyfreq-save-now
          keyfreq-show
          kill-sentence
          left-char
          magit-mode-bury-buffer
          magit-section-toggle
          magit-section-forward
          magit-section-backward
          markdown-exdent-or-delete
          markdown-outdent-or-delete
          minibuffer-complete
          minibuffer-complete-and-exit
          minibuffer-keyboard-quit
          mouse-set-point
          mouse-set-region
          mouse-drag-vertical-line
          mouse-drag-header-line
          mouse-drag-region
          move-beginning-of-line
          move-end-of-line
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
          paredit-backward-delete
          paredit-backward-kill-word
          paredit-close-round
          paredit-doublequote
          paredit-newline
          paredit-open-round
          paredit-semicolon
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
          scroll-down-command
          scroll-up-command
          select-window-0
          select-window-1
          select-window-2
          select-window-3
          select-window-4
          select-window-5
          select-window-6
          select-window-7
          select-window-8
          select-window-9
          selectrum-next-candidate
          selectrum-previous-candidate
          selectrum-select-current-candidate
          self-insert-command
          sis-get-english
          smarter-move-beginning-of-line
          suspend-frame
          term-send-raw
          turnon-keyfreq-mode
          undefined ;; lambda function
          undo-tree-redo
          undo-tree-undo
          vterm-clear
          vterm-self-insert
          w3m-goto-url
          w3m-next-anchor
          w3m-view-this-url
          web-mode
          web-mode-complete
          web-mode-jshint
          web-mode-navigate
          web-mode-part-beginning
          web-mode-reload
          web-mode-reveal
          web-mode-surround
          web-mode-tag-beginning
          web-mode-test
          wgrep-finish-edit
          xterm-paste
          yaml-electric-backspace
          yank
          yas-compile-directory
          yas-expand
          yas-next-field-or-maybe-expand
          y-or-n-p-insert-y
          y-or-n-p-insert-n
          y-or-n-p-insert-other
          )))

(provide 'init-tool)
;;; init-tool.el ends here

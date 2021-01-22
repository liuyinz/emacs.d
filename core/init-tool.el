;;; init-tool.el --- other tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package easy-hugo
;;   :custom ((easy-hugo-basedir  "~/gh/jiacai2050.github.io/")
;;        (easy-hugo-url  "https://liujiacai.net")
;;            (easy-hugo-default-ext ".org")
;;            (easy-hugo-bloglist '(((easy-hugo-basedir . "~/gh/en-blog/")
;;                                   (easy-hugo-default-ext ".org")
;;                               (easy-hugo-url . "https://en.liujiacai.net"))))))

(leaf dash-at-point)

;; (use-package leetcode
;;   :commands leetcode
;;   :init
;;   (setq leetcode-prefer-language "javascript"
;;         leetcode-prefer-sql "mysql"
;;         leetcode-save-solutions t
;;         leetcode-directory "~/Documents/repo/leetcode"))

;; IRC
(leaf erc
  :defvar erc-autojoin-channels-alist
  :init (setq erc-rename-buffers t
              erc-interpret-mirc-color t
              erc-lurker-hide-list '("JOIN" "PART" "QUIT")
              erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))

;; (leaf keyfreq
;;   :blackout keyfreq-mode keyfreq-autosave-mode
;;   :hook (after-init-hook . (lambda ()
;;                                (keyfreq-mode)
;;                                (keyfreq-autosave-mode)))
;;   (defun toggle-keyfreq ()
;;     (interactive)
;;     (keyfreq-mode)
;;     (keyfreq-autosave-mode))
;;
;;   (setq keyfreq-excluded-commands
;;         '(self-insert-command
;;           abort-recursive-edit
;;           ace-jump-done
;;           ace-jump-move
;;           ace-window
;;           avy-goto-line
;;           backward-char
;;           backward-delete-char-untabify
;;           backward-kill-word
;;           backward-word
;;           clipboard-kill-ring-save
;;           comint-previous-input
;;           comint-send-input
;;           company-complete-common
;;           company-complete-number
;;           company-complete-selection
;;           company-ignore
;;           company-search-printing-char
;;           delete-other-windows
;;           delete-backward-char
;;           describe-variable
;;           dired ;; nothing to optimize in dired
;;           dired-do-async-shell-command
;;           dired-find-file
;;           diredp-next-line
;;           diredp-previous-line
;;           electric-pair-delete-pair
;;           erase-message-buffer
;;           exit-minibuffer
;;           ffip
;;           forward-char
;;           forward-word
;;           gnus
;;           gnus-summary-exit
;;           gnus-summary-next-page
;;           gnus-summary-scroll-up
;;           gnus-topic-select-group
;;           goto-line
;;           hippie-expand
;;           ido-complete
;;           ido-delete-backward-updir
;;           ido-exit-minibuffer
;;           ido-switch-buffer
;;           indent-new-comment-line
;;           isearch-abort
;;           isearch-backward-regexp
;;           isearch-cancel
;;           isearch-delete-char
;;           isearch-exit
;;           isearch-forward-regexp
;;           isearch-other-control-char
;;           isearch-other-meta-char
;;           isearch-printing-char
;;           isearch-repeat-forward
;;           isearch-ring-retreat
;;           ispell-minor-check
;;           ivy-backward-delete-char
;;           ivy-backward-kill-word
;;           ivy-done
;;           ivy-next-line
;;           ivy-occur
;;           ivy-occur-next-line
;;           ivy-occur-press-and-switch
;;           ivy-occur-previous-line
;;           ivy-previous-line
;;           ivy-wgrep-change-to-wgrep-mode
;;           js-mode
;;           js2-line-break
;;           keyboard-escape-quit
;;           keyboard-quit
;;           keyfreq-mode
;;           keyfreq-save-now
;;           keyfreq-show
;;           kill-sentence
;;           left-char
;;           magit-mode-bury-buffer
;;           magit-section-toggle
;;           magit-section-forward
;;           magit-section-backward
;;           markdown-exdent-or-delete
;;           markdown-outdent-or-delete
;;           minibuffer-complete
;;           minibuffer-complete-and-exit
;;           minibuffer-keyboard-quit
;;           mouse-set-point
;;           mouse-set-region
;;           mouse-drag-vertical-line
;;           mouse-drag-header-line
;;           mouse-drag-region
;;           move-beginning-of-line
;;           move-end-of-line
;;           mwheel-scroll
;;           my-setup-develop-environment
;;           newline
;;           newline-and-indent
;;           next-history-element
;;           next-line
;;           org-beginning-of-line
;;           org-ctrl-c-ctrl-c
;;           org-cycle
;;           org-delete-backward-char
;;           org-end-of-line
;;           org-force-self-insert
;;           org-return
;;           org-self-insert-command
;;           org-todo
;;           orgtbl-self-insert-command
;;           package-menu-execute
;;           paredit-backward-delete
;;           paredit-backward-kill-word
;;           paredit-close-round
;;           paredit-doublequote
;;           paredit-newline
;;           paredit-open-round
;;           paredit-semicolon
;;           pcomplete
;;           previous-history-element
;;           previous-line
;;           push-button
;;           pwd
;;           quit-window
;;           right-char
;;           rjsx-electric-gt
;;           rjsx-electric-lt
;;           save-buffer
;;           save-buffers-kill-terminal
;;           scroll-down-command
;;           scroll-up-command
;;           select-window-0
;;           select-window-1
;;           select-window-2
;;           select-window-3
;;           select-window-4
;;           select-window-5
;;           select-window-6
;;           select-window-7
;;           select-window-8
;;           select-window-9
;;           self-insert-command
;;           smarter-move-beginning-of-line
;;           suspend-frame
;;           term-send-raw
;;           turnon-keyfreq-mode
;;           undefined ;; lambda function
;;           undo-tree-redo
;;           undo-tree-undo
;;           w3m-goto-url
;;           w3m-next-anchor
;;           w3m-view-this-url
;;           web-mode
;;           web-mode-complete
;;           web-mode-jshint
;;           web-mode-navigate
;;           web-mode-part-beginning
;;           web-mode-reload
;;           web-mode-reveal
;;           web-mode-surround
;;           web-mode-tag-beginning
;;           web-mode-test
;;           wgrep-finish-edit
;;           xterm-paste
;;           yaml-electric-backspace
;;           yank
;;           yas-compile-directory
;;           yas-expand
;;           yas-next-field-or-maybe-expand
;;           )))

(provide 'init-tool)
;;; init-tool.el ends here

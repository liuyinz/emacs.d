;;; init-default.el --- setting for builtin package  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package startup
;;   :init
;;   (defun display-startup-echo-area-message ())
;;   (setq inhibit-startup-screen t
;;         inhibit-startup-echo-area-message (user-login-name)
;;         initial-scratch-message nil
;;         inhibit-default-init t
;;         initial-major-mode 'fundamental-mode))

;; startup
(defun display-startup-echo-area-message ())
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name)
      initial-scratch-message nil
      inhibit-default-init t
      initial-major-mode 'fundamental-mode)

;; Start server
(use-package server
  :hook (after-init-hook . (lambda ()
                             (require 'server)
                             (unless (server-running-p)
                               (server-start))))
  :init
  (setq server-socket-dir (format "/tmp/emacs-%d-%s-%d"
                                  (user-uid)
                                  (format-time-string "%Y%m%d-%H%M%S")
                                  (emacs-pid))))

;; display line number
;; (use-package display-line-numbers
;;   :hook (prog-mode-hook . display-line-numbers-mode)
;;   :init (setq-default display-line-numbers-type 'relative))

(use-package saveplace
  :hook (after-init-hook . save-place-mode))

;; recentf
(use-package recentf
  :hook (after-init-hook . recentf-mode)
  :init
  (setq recentf-max-menu-items 20
        recentf-max-saved-items 1000
        recentf-auto-cleanup 60
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)

  ;; silent message
  (mapc (lambda (cmd)
          (advice-add cmd :around #'silent-message-advice))
        '(recentf-load-list
          recentf-save-list
          recentf-cleanup)))

;; Savehist
(use-package savehist
  :hook (after-init-hook . savehist-mode)
  :init
  (setq enable-recursive-minibuffers t
        history-length 1000
        savehist-save-minibuffer-history 1
        history-delete-duplicates t
        savehist-autosave-interval 300
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)))

(use-package simple
  :hook (after-init-hook . (lambda ()
                             (size-indication-mode)
                             (transient-mark-mode)
                             (line-number-mode)
                             (column-number-mode)))
  :init (setq line-move-visual t
              track-eol t
              set-mark-command-repeat-pop t))

(use-package files
  :init
  (setq make-backup-files nil
        enable-local-variables :all
        create-lockfiles nil
        auto-save-default nil
        auto-save-list-file-prefix nil
        save-silently t
        confirm-kill-processes nil
        find-file-suppress-same-file-warnings t))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"))

;; Delete selection if you insert
(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :hook (after-init-hook . global-auto-revert-mode)
  :init
  (setq auto-revert-interval 0.01
        auto-revert-use-notify nil
        auto-revert-verbose nil))

;; Automatic parenthesis pairing
(use-package elec-pair
  :hook (after-init-hook . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :hook ((prog-mode-hook . subword-mode)
         (minibuffer-setup-hook . subword-mode)))

;; Pass a URL to a WWW browser
(use-package browse-url
  :defines dired-mode-map
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :hook ((text-mode-hook . goto-address-mode)
         ;; An all-in-one comment command to rule them all
         (prog-mode-hook . goto-address-prog-mode)))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  ;; :commands outline-show-all
  :hook(;; show org ediffs unfolded
        ;; (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit-hook . winner-undo))
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Resolve diff3 conflicts
(use-package smerge-mode
  :hook ((find-file-hook . (lambda ()
                             (save-excursion
                               (goto-char (point-min))
                               (when (re-search-forward "^<<<<<<< " nil t)
                                 (smerge-mode 1)
                                 ))))))

;; Whitespace-mode
(use-package whitespace
  :commands whitespace-cleanup
  :init
  (setq whitespace-style '(face empty trailing))
  (face-spec-set 'whitespace-empty
                 '((((background light))
                    :background "#FF6C6B")
                   (t
                    :background "#FF6C6B"))))

(use-package mwheel
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))

(use-package tooltip
  :config
  (tooltip-mode 1)
  (setq tooltip-resize-echo-area t))

;; adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
;; adaptive-fill-first-line-regexp "^* *$"
;; sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
;; sentence-end-double-space nil)

(setq vc-follow-symlinks t)

;; c source
(setq use-file-dialog nil
      use-dialog-box nil
      load-prefer-newer t
      echo-keystrokes 0.1
      ad-redefinition-action 'accept
      delete-by-moving-to-trash t
      inhibit-compacting-font-caches t
      window-resize-pixelwise t
      frame-resize-pixelwise t
      ring-bell-function 'ignore)

(setq-default fill-column 80
              tab-width 4
              fringes-outside-margins t
              fringe-indicator-alist nil
              indent-tabs-mode nil
              left-margin-width 0
              right-margin-width 0
              default-directory "~")


;; ;; Mouse & Smooth Scroll
;; (setq scroll-step 0
;;       scroll-margin 0
;;       scroll-conservatively 100000)

;; Misc
(defalias 'yes-or-no-p 'y-or-n-p)
;; Alias the UTF-8
(define-coding-system-alias 'UTF-8 'utf-8)

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setenv "LC_ALL" "en_CN.UTF-8")
;; make vterm colorful
(setenv "COLORTERM" "truecolor")
;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(if (get-buffer "*scratch*")
    (setq default-directory "~/"))

;;unbind keys
(dolist (key '("\M-j" "\M-k" "\M-u" "\M-l"))
  (global-unset-key key))

(provide 'init-default)
;;; init-default.el ends here

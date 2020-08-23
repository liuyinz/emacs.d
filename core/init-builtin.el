;; Start server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;;display line number
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :init (setq-default display-line-numbers-type 'relative))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;;recentf
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-menu-items 20
        recentf-max-saved-items 500
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

;; Savehist
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
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
  :ensure nil
  :hook (after-init . (lambda ()
                        (size-indication-mode)
                        (transient-mark-mode)
                        (line-number-mode)
                        (column-number-mode)))
  :init (setq line-move-visual nil
              track-eol t
              set-mark-command-repeat-pop t))

;; No backup file
(use-package files
  :ensure nil
  :init
  (setq make-backup-files nil
        create-lockfiles nil
        auto-save-default nil
        auto-save-list-file-prefix nil
        save-silently t
        confirm-kill-processes nil
        find-file-suppress-same-file-warnings t))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"))

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :init
  (setq auto-revert-interval 0.01
        auto-revert-use-notify nil
        auto-revert-verbose nil))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; ;; Hideshow
;; (use-package hideshow
;;   :ensure nil
;;   :diminish hs-minor-mode
;;   :bind (:map hs-minor-mode-map
;;          ("C-`" . hs-toggle-hiding)))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         ;; An all-in-one comment command to rule them all
         (prog-mode . goto-address-prog-mode)))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  ;; :commands outline-show-all
  :ensure nil
  :hook(;; show org ediffs unfolded
        ;; (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)
                            ))))))
;; )

;; Whitespace-mode
(use-package whitespace
  :ensure nil
  :commands whitespace-cleanup
  :init
  (setq whitespace-style '(face empty trailing))
  (face-spec-set 'whitespace-empty
                 '((((background light))
                    :background "#FF6C6B")
                   (t
                    :background "#FF6C6B"))))

(use-package mwheel
  :ensure nil
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))

(setq load-prefer-newer t)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message "ray"
      initial-scratch-message nil
      ;; initial-major-mode 'lisp-interaction-mode
      window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Mouse & Smooth Scroll
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

(setq gc-cons-threshold 100000000)

(setq use-file-dialog nil
      enable-local-variables :all
      use-dialog-box nil
      ad-redefinition-action 'accept
      delete-by-moving-to-trash t
      inhibit-startup-echo-area-message (user-login-name)
      inhibit-compacting-font-caches t
      ring-bell-function 'ignore
      visible-bell t
      inhibit-compacting-font-caches t
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

(setq-default fill-column 80
              tab-width 4
              tab-always-indent 'complete
              indent-tabs-mode nil
              ;; cursor-type 'bar
              ;; Set fringe style
              fringes-outside-margins t
              fringe-indicator-alist nil
              ;; hide margin
              left-margin-width 0
              right-margin-width 0
              default-directory "~"
              )

;; Misc
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer-other-window)
;; Alias the UTF-8
(define-coding-system-alias 'UTF-8 'utf-8)
(defun display-startup-echo-area-message ())

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

;; (setq profiler-report-cpu-line-format   ;让 profiler-report 第一列宽一点
;;       '((100 left)
;;         (24 right ((19 right)
;;                    (5 right)))))
;; (setq profiler-report-memory-line-format
;;       '((100 left)
;;         (19 right ((14 right profiler-format-number)
;;                    (5 right)))))

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; GC automatically while unfocusing the frame
            (add-function :after after-focus-change-function
              (lambda ()
                (unless (frame-focus-state)
                  (garbage-collect))))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun max-gc-limit ()
              (setq gc-cons-threshold most-positive-fixnum))

            (defun reset-gc-limit ()
              (setq gc-cons-threshold 800000))

            (add-hook 'minibuffer-setup-hook #'max-gc-limit)
            (add-hook 'minibuffer-exit-hook #'reset-gc-limit)))

;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(if (get-buffer "*scratch*")
    (setq default-directory "~/Documents/"))

;;unbind keys
(dolist (key '("\M-j" "\M-k" "\M-u" "\M-l"))
  (global-unset-key key))

(provide 'init-builtin)

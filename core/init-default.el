;;; init-default.el --- setting for builtin package  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf startup
  :init
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message (user-login-name)
        initial-scratch-message nil
        inhibit-default-init t
        initial-major-mode 'fundamental-mode)
  ;; Get rid of "For information about GNU Emacs..." message at startup
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Start server
(leaf server
  :init
  (setq server-client-instructions nil)
  :hook (after-init-hook . (lambda ()
                             (require 'server)
                             (unless (server-running-p)
                               (server-start)))))

(leaf saveplace
  :hook (after-init-hook . save-place-mode))

;; recentf
(leaf recentf
  :hook (after-init-hook . (lambda ()
                             (recentf-mode)
                             (recentf-cleanup)))
  :init
  (setq recentf-max-menu-items 20
        recentf-max-saved-items 1000
        recentf-auto-cleanup 30
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" "/share/emacs/.+$" ; "^/ssh:"
          (lambda (file) (file-in-directory-p file my-dir-lib))))
  :config
  ;; silent message
  (mapc (lambda (cmd)
          (advice-add cmd :around #'silent-message-advice))
        '(recentf-load-list
          recentf-save-list
          recentf-cleanup)))

;; Savehist
(leaf savehist
  :hook (after-init-hook . savehist-mode)
  :init
  (setq
   savehist-autosave-interval 300
   savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history)))

(leaf simple
  :hook (after-init-hook . (lambda ()
                             (global-visual-line-mode)
                             (line-number-mode)
                             (column-number-mode)
                             (size-indication-mode))))

(leaf webjump
  :init
  (setq webjump-sites '(
                        ;; Internet search engines.
                        ("Google" .
                         [simple-query "www.google.com"
                                       "www.google.com/search?q=" ""])
                        ("Github" .
                         [simple-query "www.github.com"
                                       "www.github.com/search?q=" ""])
                        ("Melpa" .
                         [simple-query "melpa.org"
                                       "melpa.org/#/?q=" ""])
                        ("Baidu" .
                         [simple-query "www.baidu.com"
                                       "www.baidu.com/s?wd=" ""])
                        ("Zhihu" .
                         [simple-query "www.zhihu.com"
                                       "www.zhihu.com/search?type=content&q=" ""])
                        ("V2ex" .
                         [simple-query "www.sov2ex.com"
                                       "www.sov2ex.com/?q=" ""])
                        ("Wikipedia" .
                         [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])))
  :config
  ;; HACK support visual selection texts in webjump()
  (defun webjump-visual-patch (prompt)
    "Patch for visual selection avaible"
    (let* ((region-text (if (region-active-p)
                            (buffer-substring-no-properties (region-beginning) (region-end)) nil))
           (input (read-string (concat prompt ": ") region-text)))
      (if (webjump-null-or-blank-string-p input) nil input)))
  (advice-add 'webjump-read-string :override #'webjump-visual-patch)
  )

(leaf files
  :init
  (setq make-backup-files nil
        enable-local-variables :all
        create-lockfiles nil
        auto-save-default nil
        auto-save-list-file-prefix nil
        auto-mode-case-fold nil
        save-silently t
        large-file-warning-threshold nil
        confirm-kill-processes nil
        find-file-suppress-same-file-warnings t))

(leaf uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"))

;; Delete selection if you insert
(leaf delsel
  :hook (after-init-hook . delete-selection-mode))

;; Automatically reload files was modified by external program
(leaf autorevert
  :hook (after-init-hook . global-auto-revert-mode)
  :init
  (setq auto-revert-interval 0.01
        auto-revert-use-notify nil
        auto-revert-verbose nil))

;; Automatic parenthesis pairing
(leaf elec-pair
  :hook (after-init-hook . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Handling capitalized subwords in a nomenclature
(leaf subword
  :hook
  (prog-mode-hook . subword-mode)
  (minibuffer-setup-hook . subword-mode))

;; Click to browse URL or to send to e-mail address
(leaf goto-addr
  :hook
  (text-mode-hook . goto-address-mode)
  ;; An all-in-one comment command to rule them all
  (prog-mode-hook . goto-address-prog-mode))

(leaf eldoc)

;; A comprehensive visual interface to diff & patch
(leaf ediff
  ;; :commands outline-show-all
  :hook (;; show org ediffs unfolded
         ;; (ediff-prepare-buffer . outline-show-all)
         ;; restore window layout when done
         (ediff-quit-hook . winner-undo))
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Resolve diff3 conflicts
(leaf smerge-mode
  :hook (find-file-hook . (lambda ()
                            (save-excursion
                              (goto-char (point-min))
                              (when (re-search-forward "^<<<<<<< " nil t)
                                (smerge-mode 1)
                                (evil-emacs-state 1)
                                )))))

;; Whitespace-mode
(leaf whitespace
  :commands whitespace-cleanup
  :init
  (setq whitespace-style '(face empty trailing))
  (face-spec-set 'whitespace-empty
                 '((((background light))
                    :background "#FF6C6B")
                   (t
                    :background "#FF6C6B"))))

(leaf mwheel
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))

;; adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
;; adaptive-fill-first-line-regexp "^* *$"
;; sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
;; sentence-end-double-space nil)

(leaf profiler
  :init
  (setq profiler-report-leaf-mark  ">")

  ;; HACK , reformat memory size
  (defun profiler-bytes-h (str)
    "reformat with human-readeable size"
    (let ((s (cl-count ?, str)))
      (cond
       ((= s 1) (concat (substring str 0 -4) " K"))
       ((= s 2) (concat (substring str 0 -8) " M"))
       ((>= s 3) (concat (substring str 0 -12) " G"))
       (t str))))
  (advice-add 'profiler-format-number :filter-return #'profiler-bytes-h))

(setq isearch-lazy-count t)
(setq vc-follow-symlinks t)

;; c source
;; Misc
(setq use-short-answers t
      use-file-dialog nil
      use-dialog-box nil
      echo-keystrokes 0.1
      ad-redefinition-action 'accept
      delete-by-moving-to-trash t
      inhibit-compacting-font-caches t
      window-resize-pixelwise t
      frame-resize-pixelwise t
      ring-bell-function 'ignore
      history-length 1000
      history-delete-duplicates t)

(setq-default fill-column 80
              tab-width 4
              fringes-outside-margins t
              fringe-indicator-alist nil
              indent-tabs-mode nil
              left-margin-width 0
              right-margin-width 0
              default-directory "~")

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but we inhibit it there anyway. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caces t)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help with performance while scrolling.
(setq redisplay-skip-fontification-on-input t)

;; Remove command line options that aren't relevant to our current OS
(setq command-line-x-option-alist nil)

;; This file stores usernames, passwords, and other such treasures for the aspiring malicious third party.
(setq auth-sources '(macos-keychain-internet))

;; Encoding
;; Alias the UTF-8
(define-coding-system-alias 'UTF-8 'utf-8)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setenv "LC_ALL" "en_US.UTF-8")
;; make vterm colorful
(setenv "COLORTERM" "truecolor")

;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(if (get-buffer "*scratch*")
    (setq default-directory "~/"))

;; cursor
(setq blink-cursor-blinks 0)
;; HACK move cursor to corner when minibuffer is set up
(require 'avoid)
(add-hook 'minibuffer-setup-hook #'mouse-avoidance-banish)

;; minibuffer
;; Grow and shrink minibuffer
;; (setq resize-mini-windows t)
;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; HACK Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Add prompt indicator to `completing-read-multiple'.
(defun crm-indicator (args)
  "Set indicater ARGS for multiple read."
  (cons (concat "[*] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(provide 'init-default)
;;; init-default.el ends here

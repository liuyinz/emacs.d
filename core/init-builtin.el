;;; init-builtin.el --- setting for builtin package  -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;; Code:

;; ------------------------- Startup ------------------------------

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name)
      initial-scratch-message nil
      inhibit-default-init t
      auto-save-list-file-prefix nil)

;; don't load sitestart.el
(setq site-run-file nil)
(setq user-full-name "liuyinz")
(setq user-mail-address "liuyinz95@gmail.com")

(advice-add #'display-startup-echo-area-message :override #'ignore)

(leaf server
  :init (setq server-client-instructions nil)
  :hook (after-init-hook . (lambda ()
                             (require 'server)
                             (unless (or (daemonp) (server-running-p))
                               (server-start)))))

;; --------------------------- Tui --------------------------------

(leaf frame
  :init
  (setq blink-cursor-blinks 0)
  ;; menu-bar-mode would called forced before gui-frame in Macos
  (add-hook 'after-make-window-system-frame-hook (lambda () (menu-bar-mode -1)))

  ;; set vertical split bar to "│"
  ;; SEE https://www.reddit.com/r/emacs/comments/5tm9zy/vertical_split_bar/ddnw72f?utm_source=share&utm_medium=web2x&context=3
  (set-display-table-slot standard-display-table 'vertical-border ?│)

  ;; SEE https://apple.stackexchange.com/a/36947
  (defun ad/enable-tui-fullscreen (fn)
    "Toggle fullscreen with TUI support."
    (if (and (not (display-graphic-p))
             (eq system-type 'darwin))
        (call-process "osascript" nil t nil
                      (expand-file-name "kitty-toggle.scpt" my/dir-ext))
      (funcall fn)))
  (advice-add 'toggle-frame-fullscreen :around #'ad/enable-tui-fullscreen))

(leaf xt-mouse
  :init
  (defun my/mouse-setup ()
    "enable mouse and keybindings in eamcs -nw"
    (xterm-mouse-mode)
    (global-set-key [mouse-4] (lambda ()
                                (interactive)
                                (scroll-down 1)))
    (global-set-key [mouse-5] (lambda ()
                                (interactive)
                                (scroll-up 1))))
  (add-hook 'after-make-console-frame-hook #'my/mouse-setup))

(leaf mwheel
  :defer-config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))

;; ------------------------ Apperance -----------------------------

;; TODO multiple desktop settings,see
;; https://www.emacswiki.org/emacs/DesktopMultipleSaveFiles
;; https://stackoverflow.com/a/849180/13194984
(leaf desktop
  ;; :hook (after-init-hook . desktop-save-mode)
  :init
  (setq desktop-auto-save-timeout 600
        desktop-restore-frames nil)
  (setq desktop-globals-to-save
        '((comint-input-ring        . 50)
          (compile-history          . 30)
          desktop-missing-file-warning
          (dired-regexp-history     . 20)
          (extended-command-history . 30)
          (face-name-history        . 20)
          (file-name-history        . 100)
          (grep-find-history        . 30)
          (grep-history             . 30)
          (magit-revision-history   . 50)
          (minibuffer-history       . 50)
          (org-clock-history        . 50)
          (org-refile-history       . 50)
          (org-tags-history         . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          register-alist
          (search-ring              . 20)
          (shell-command-history    . 50)
          tags-file-name
          tags-table-list))

  :defer-config
  (defun ad/desktop-time-restore (orig &rest args)
    "Count the restore time in total."
    (message "Desktop: %.2fms restored in TOTAL" (time-count! (apply orig args))))
  (advice-add 'desktop-read :around 'ad/desktop-time-restore)

  (defun ad/desktop-time-buffer-create (orig ver filename &rest args)
    "Count the buffer restored time."
    (message "Desktop: %.2fms to restore %s"
             (time-count! (apply orig ver filename args))
             (when filename
               (abbreviate-file-name filename))))
  (advice-add 'desktop-create-buffer :around 'ad/desktop-time-buffer-create))

(leaf simple
  :init
  (setq next-error-highlight t
        next-error-highlight-no-select t
        next-error-message-highlight t
        read-extended-command-predicate #'command-completion-default-include-p
        ;; next-error-recenter '(4)
        )
  :defer-config
  (line-number-mode)
  (column-number-mode)
  (size-indication-mode))

(leaf uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"))

;; TODO add commands to jump folds with toggle automaticlly
(leaf hideshow
  :hook (prog-mode-hook . hs-minor-mode)
  :init
  (setq hs-isearch-open t
        hs-hide-comments-when-hiding-all nil)

  ;; display more information
  (defun display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (propertize
                    (format "...%d folding..."
                            (- (count-lines (overlay-start ov) (overlay-end ov)) 1))
                    'face 'vertico-current))))
  (setq hs-set-up-overlay #'display-code-line-counts)

  :defer-config

  ;; FIXME add command `hs-toggle-all'
  (defvar-local hs-all-hide-p nil)
  (defun hs-toggle-all ()
    "Toggle all folds at once."
    (interactive)
    (hs-life-goes-on
     (if (bound-and-true-p hs-all-hide-p)
         (hs-show-all)
       (hs-hide-all))))
  (advice-add 'hs-show-all :after (lambda () (setq hs-all-hide-p nil)))
  (advice-add 'hs-hide-all :after (lambda () (setq hs-all-hide-p t)))
  )

;; -------------------------- Buffer ------------------------------

(leaf files
  :init
  (setq make-backup-files nil
        enable-local-variables :all
        create-lockfiles nil
        auto-save-default nil
        auto-mode-case-fold nil
        save-silently t
        ;; large-file-warning-threshold nil
        confirm-kill-processes nil
        find-file-suppress-same-file-warnings t)

  ;; ISSUE https://github.com/emacsorphanage/osx-trash/issues/5#issuecomment-882759527
  (setq delete-by-moving-to-trash t)
  (when (eq system-type 'darwin)
    (setq trash-directory "~/.Trash"))
  )

(leaf saveplace
  :init (setq save-place-limit nil)
  :hook (after-init-hook . save-place-mode))

(leaf recentf
  :hook (after-init-hook . recentf-mode)
  :init
  (setq recentf-max-saved-items nil
        recentf-auto-cleanup 15
        recentf-exclude
        '("\\.?cache" "-autoloads\\.el\\'" ".cask" "url" "COMMIT_EDITMSG\\'"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/" "/.Trash/"
          "^/tmp/" "^/var/folders/.+$" "/share/emacs/.+$" "\\.git/.+$"
          "bookmarks"))

  :defer-config
  ;; auto-cleanup in save/load
  (advice-add 'recentf-save-list :before #'recentf-cleanup)
  (advice-add 'recentf-load-list :after #'recentf-cleanup)

  ;; silent message
  (mapc (lambda (cmd)
          (advice-add cmd :around #'ad/silent-message))
        '(recentf-load-list
          recentf-save-list
          recentf-cleanup)))

(leaf savehist
  :hook (after-init-hook . savehist-mode)
  :init
  (setq savehist-autosave-interval 300
        savehist-additional-variables
        '(mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history)))

(leaf so-long
  :hook (after-init-hook . global-so-long-mode))

(leaf minibuffer
  :init
  (setq enable-recursive-minibuffers t)
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; --------------------------- Edit -------------------------------

(leaf elec-pair
  :hook (after-init-hook . electric-pair-mode)
  :defer-config
  ;; SEE https://emacs-china.org/t/html-electric-pair-mode-js/13904/11?u=cheunghsu
  (defvar electric-pair-extra-inhibit-mode-chars-alist
    '((t . nil))
    "A list of major-mode and inhibit chars.
Each element is in the form of (MODE . (CHAR/CHAR-STRING/CHAR-FUNCTION ...)).
MODE
    A mode, or t for all modes.
CHAR
    A character to match the input. for example:
        ?\{
CHAR-STRING
    A pair of character and string, the character to match the input,
    the string for ‘looking-back’. for example:

        (?\{ . \":{\")
CHAR-FUNCTION
    A pair of character and function, the character to match the input,
    the function accept the input character as parameter. for example:
        (?\{ . (lambda (_c)
                 (eq ?: (char-before (1- (point))))))")
  (defun electric-pair-extra-inhibit (c)
    (let ((alist
           (append
            (assoc-default major-mode electric-pair-extra-inhibit-mode-chars-alist)
            (assoc-default t          electric-pair-extra-inhibit-mode-chars-alist))))
      (or (cl-member c
                     alist
                     :test
                     (lambda (c it)
                       (cond
                        ((characterp it) (equal c it))
                        ((and (consp it) (equal c (car it)))
                         (cond ((stringp   (cdr it)) (looking-back (cdr it) 1))
                               ((functionp (cdr it)) (funcall (cdr it) c)))))))
          (electric-pair-default-inhibit c))))
  (setq electric-pair-inhibit-predicate #'electric-pair-extra-inhibit)

  (appendq! electric-pair-extra-inhibit-mode-chars-alist
            '((js-mode . (?<))
              (js2-mode . (?<))
              (org-mode . (?<))))
  )

(leaf subword
  :hook ((prog-mode-hook . subword-mode)
         (minibuffer-setup-hook . subword-mode)))

(leaf delsel
  :hook (after-init-hook . delete-selection-mode))

(leaf whitespace
  :init
  (setq whitespace-style '(face empty trailing))
  (face-spec-set 'whitespace-empty
                 '((((background light))
                    :background "#FF6C6B")
                   (t
                    :background "#FF6C6B"))))

(leaf newcomment
  :init
  (setq comment-auto-fill-only-comments t)
  :bind
  ([remap comment-dwim] . #'newcomment-toggle)
  :defer-config
  (defun newcomment-toggle (n)
    "Toggle the comments as evil-nerd-comment."
    (interactive "*p")
    (if (or (use-region-p)
            (save-excursion
              (beginning-of-line)
              (looking-at "\\s-*$")))
        (call-interactively 'comment-dwim)
      (let ((range
             (list (line-beginning-position)
                   (goto-char (line-end-position n)))))
        (comment-or-uncomment-region
         (apply #'min range)
         (apply #'max range))))))

(leaf copyright
  :init (setq copyright-year-ranges t))

(leaf diff-mode
  :init
  ;; disable smerge-refine with set `diff-refine' to nil
  (setq diff-refine 'navigation))

(leaf smerge-mode
  :hook (smerge-mode-hook . my/smerge-setup)
  :init
  (setq smerge-command-prefix ""
        smerge-change-buffer-confirm nil
        smerge-refine-ignore-whitespace nil)

  (defvar smerge-toggle-modes
    '((diff-hl-mode . nil)
      (hl-line-mode . nil)
      (save-place-local-mode . nil)
      (highlight-parentheses-mode . nil))
    "Modes should be toggled when call smerge-mode-hook")

  (defun my/smerge-setup ()
    "Setting when `smerge-mode' is enable."
    (mode-hook-toggle smerge-mode smerge-toggle-modes)
    (my/transient-smerge)
    ;; make sure call `smerge-first' after disable `save-place-local-mode'
    ;; see `add-hook' doc about order
    (smerge-first))

  (defun smerge-first ()
    "Jump to first conflict in the buffer."
    (interactive)
    (goto-char (point-min))
    (unless (looking-at smerge-begin-re)
      (smerge-next)))

  (defun smerge-last ()
    "Jump to first conflict in the buffer."
    (interactive)
    (goto-char (point-max))
    (smerge-prev))

  (defun smerge-conflict-preview-or-scroll ()
    "Preview or scorll conflict region."
    (interactive)
    (smerge-match-conflict)
    (let* ((rev (match-beginning 0))
           (buf (get-buffer "*smerge-preview*"))
           win)

      (unless (and buf (equal rev (buffer-local-value 'orig-rev buf)))
        (copy-to-buffer "*smerge-preview*" (match-beginning 0) (match-end 0))

        ;; SEE https://emacs.stackexchange.com/a/32817
        ;; (with-current-buffer "*smerge-preview*"
        ;;   (set (make-local-variable 'orig-rev) rev))
        (setf (buffer-local-value 'orig-rev buf) rev)
        )

      (if (setq win (get-buffer-window buf))
          (with-selected-window win
            (condition-case nil
                (scroll-up)
              (error
               (goto-char (point-min)))))
        (display-buffer buf nil))))

  ;; (find-file-hook . my/smerge-setup)
  ;; (defun  my/smerge-setup ()
  ;;   "If conflicts detected enable `smerge-mode'"
  ;;   (save-excursion
  ;;     (goto-char (point-min))
  ;;     (when (re-search-forward "^<<<<<<< " nil t)
  ;;       (smerge-mode 1))))

  )

;; ;; On-the-fly spell checker
;; (leaf flyspell
;;   :if (executable-find "aspell")
;;   :hook (((text-mode-hook outline-mode-hook) . flyspell-mode)
;;          (prog-mode-hook . flyspell-prog-mode)
;;          )
;;   :init (setq flyspell-issue-message-flag nil
;;               flyspell-doublon-as-error-flag nil
;;               ispell-program-name "aspell"
;;               ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

;; (leaf eldoc)

;; ;; A comprehensive visual interface to diff & patch
;; (leaf ediff
;;   :hook (;; show org ediffs unfolded
;;          ;; (ediff-prepare-buffer . outline-show-all)
;;          ;; restore window layout when done
;;          (ediff-quit-hook . winner-undo))
;;   :init
;;   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;   (setq ediff-split-window-function 'split-window-horizontally)
;;   (setq ediff-merge-split-window-function 'split-window-horizontally))

;; adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
;; adaptive-fill-first-line-regexp "^* *$"
;; sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
;; sentence-end-double-space nil)

(setq isearch-lazy-count t)
(setq vc-follow-symlinks t)

;; Add prompt indicator to `completing-read-multiple'.
(defun ad/crm-indicator (args)
  "Set indicater ARGS for multiple read."
  (cons (concat "[*] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'ad/crm-indicator)

;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(if (get-buffer "*scratch*")
    (setq default-directory "~/"))

;; (leaf autorevert
;;   :hook (after-init-hook . global-auto-revert-mode)
;;   :init
;;   (setq auto-revert-interval 0.01
;;         auto-revert-use-notify nil
;;         auto-revert-verbose nil))

;; --------------------------- Jump -------------------------------

(leaf xref
  :init
  (when emacs/>=28.1p
    (setq xref-search-program #'ripgrep
          xref-show-xrefs-function 'xref-show-definitions-completing-read
          xref-show-definitions-function 'xref-show-definitions-completing-read)))

(leaf winner
  :hook (after-init-hook . winner-mode))

(leaf goto-addr
  :hook ((after-init-hook . global-goto-address-mode)
         (prog-mode-hook . goto-address-prog-mode)))

(leaf webjump
  :init
  (setq webjump-sites
        '(;; Internet search engines.
          ("StackOverFlow" .
           [simple-query "stackoverflow.com"
                         "stackoverflow.com/search?q=" ""])
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
           [simple-query "wikipedia.org"
                         "wikipedia.org/wiki/" ""])))
  :defer-config
  ;; HACK support visual selection texts in webjump()
  (defun ad/webjump-read-string-enable-visual (prompt)
    "Patch for visual selection avaible"
    (let* ((region-text (if (use-region-p)
                            (buffer-substring-no-properties
                             (region-beginning)
                             (region-end)) nil))
           (input (read-string (concat prompt ": ") region-text)))
      (if (webjump-null-or-blank-string-p input) nil input)))
  (advice-add 'webjump-read-string :override #'ad/webjump-read-string-enable-visual))

(leaf compile
  :defer-config
  (defun compilation-first-error ()
    "Move point to the first error in the compilation buffer."
    (interactive)
    (compilation-next-error 1 nil (point-min)))

  (defun compilation-last-error ()
    "Move point to the last error in the compilation buffer."
    (interactive)
    (compilation-next-error (- 1) nil (point-max)))

  (defun ad/compilation-previous-file (n)
    "Move point at first error when jumping to previous files."
    (interactive "p")
    (compilation-next-file (- n))
    (condition-case nil
        (progn
          (compilation-next-file (- 1))
          (compilation-next-file 1))
      (error
       (compilation-first-error))))
  (advice-add 'compilation-previous-file :override #'ad/compilation-previous-file)

  )

;; --------------------------- Tool -------------------------------

(leaf auth-source
  :commands auth-source-user-and-password
  :init
  (setq auth-sources '("~/.authinfo")))

(leaf profiler
  :init
  (setq profiler-report-leaf-mark  ">")

  (defun ad/profiler-bytes-h (str)
    "reformat with human-readeable size"
    (let ((s (cl-count ?, str)))
      (cond
       ((= s 1) (concat (substring str 0 -4) " K"))
       ((= s 2) (concat (substring str 0 -8) " M"))
       ((>= s 3) (concat (substring str 0 -12) " G"))
       (t str))))
  (advice-add 'profiler-format-number :filter-return #'ad/profiler-bytes-h))

(leaf transient
  :require t
  :init
  (setq transient-highlight-mismatched-keys nil
        transient-detect-key-conflicts t))

;; -------------------------- Encode ------------------------------

(define-coding-system-alias 'UTF-8 'utf-8)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; ------------------------- C source -----------------------------

(setq use-short-answers t
      use-file-dialog nil
      use-dialog-box nil
      echo-keystrokes 0.02
      ad-redefinition-action 'accept
      inhibit-compacting-font-caches t
      window-resize-pixelwise t
      frame-resize-pixelwise t
      ring-bell-function 'ignore
      history-length 1000
      history-delete-duplicates t
      )

(setq-default fill-column 80
              tab-width 4
              fringes-outside-margins t
              fringe-indicator-alist nil
              indent-tabs-mode nil
              left-margin-width 1
              right-margin-width 1
              default-directory "~")

;; SEE https://emacs.stackexchange.com/a/2555/35676;9u
(setq-default major-mode
              (lambda () (if buffer-file-name
                             (fundamental-mode)
                           (let ((buffer-file-name (buffer-name)))
                             (set-auto-mode)))))

;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000)  ; 64kb

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(provide 'init-builtin)
;;; init-builtin.el ends here

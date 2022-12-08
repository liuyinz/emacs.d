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

  ;; HACK menu-bar-mode would called forced before gui-frame in Macos
  (add-hook 'after-make-window-system-frame-hook (lambda () (menu-bar-mode -1)))

  ;; set vertical split bar to "│"
  ;; SEE https://www.reddit.com/r/emacs/comments/5tm9zy/vertical_split_bar/ddnw72f?utm_source=share&utm_medium=web2x&context=3
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
  ;; use space for wrap line
  ;; SEE https://www.emacswiki.org/emacs/LineWrap
  (set-display-table-slot standard-display-table 'wrap ?\ )
  (set-display-table-slot standard-display-table 'truncation ?\ )

  ;; SEE https://apple.stackexchange.com/a/36947
  (defun ad/enable-tui-fullscreen (fn)
    "Toggle fullscreen with TUI support."
    (if (and (not (display-graphic-p))
             (eq system-type 'darwin))
        (call-process "osascript"
                      nil t nil
                      (expand-file-name "kitty-toggle.scpt" my/dir-ext))
      (funcall fn)))
  (advice-add 'toggle-frame-fullscreen :around #'ad/enable-tui-fullscreen))

(leaf mouse
  :init
  (setq mouse-yank-at-point t))

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

(leaf hl-line
  :hook (after-init-hook . global-hl-line-mode))

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
        read-extended-command-predicate #'command-completion-default-include-p)

  (setq-default indent-tabs-mode nil)

  (setq undo-no-redo t)
  ;; jump after inserted text after undo-redo
  (advice-add #'primitive-undo :override #'my/primitive-undo)
  (defun my/primitive-undo (n list)
    "Undo N records from the front of the list LIST.
Return what remains of the list."

    ;; This is a good feature, but would make undo-start
    ;; unable to do what is expected.
    ;;(when (null (car (list)))
    ;;  ;; If the head of the list is a boundary, it is the boundary
    ;;  ;; preceding this command.  Get rid of it and don't count it.
    ;;  (setq list (cdr list))))

    (let ((arg n)
          ;; In a writable buffer, enable undoing read-only text that is
          ;; so because of text properties.
          (inhibit-read-only t)
          ;; We use oldlist only to check for EQ.  ++kfs
          (oldlist buffer-undo-list)
          (did-apply nil)
          (next nil))
      (while (> arg 0)
        (while (setq next (pop list))     ;Exit inner loop at undo boundary.
          ;; Handle an integer by setting point to that value.
          (pcase next
            ((pred integerp) (goto-char next))
            ;; Element (t . TIME) records previous modtime.
            ;; Preserve any flag of NONEXISTENT_MODTIME_NSECS or
            ;; UNKNOWN_MODTIME_NSECS.
            (`(t . ,time)
             ;; If this records an obsolete save
             ;; (not matching the actual disk file)
             ;; then don't mark unmodified.
             (let ((visited-file-time (visited-file-modtime)))
               ;; Indirect buffers don't have a visited file, so their
               ;; file-modtime can be bogus.  In that case, use the
               ;; modtime of the base buffer instead.
               (if (and (numberp visited-file-time)
                        (= visited-file-time 0)
                        (buffer-base-buffer))
                   (setq visited-file-time
                         (with-current-buffer (buffer-base-buffer)
                           (visited-file-modtime))))
	           (when (time-equal-p time visited-file-time)
                 (unlock-buffer)
                 (set-buffer-modified-p nil))))
            ;; Element (nil PROP VAL BEG . END) is property change.
            (`(nil . ,(or `(,prop ,val ,beg . ,end) pcase--dontcare))
             (when (or (> (point-min) beg) (< (point-max) end))
               (error "Changes to be undone are outside visible portion of buffer"))
             (put-text-property beg end prop val))
            ;; Element (BEG . END) means range was inserted.
            (`(,(and beg (pred integerp)) . ,(and end (pred integerp)))
             ;; (and `(,beg . ,end) `(,(pred integerp) . ,(pred integerp)))
             ;; Ideally: `(,(pred integerp beg) . ,(pred integerp end))
             (when (or (> (point-min) beg) (< (point-max) end))
               (error "Changes to be undone are outside visible portion of buffer"))
             ;; Set point first thing, so that undoing this undo
             ;; does not send point back to where it is now.
             (goto-char beg)
             (delete-region beg end))
            ;; Element (apply FUN . ARGS) means call FUN to undo.
            (`(apply . ,fun-args)
             (let ((currbuff (current-buffer)))
               (if (integerp (car fun-args))
                   ;; Long format: (apply DELTA START END FUN . ARGS).
                   (pcase-let* ((`(,delta ,start ,end ,fun . ,args) fun-args)
                                (start-mark (copy-marker start nil))
                                (end-mark (copy-marker end t)))
                     (when (or (> (point-min) start) (< (point-max) end))
                       (error "Changes to be undone are outside visible portion of buffer"))
                     (apply fun args) ;; Use `save-current-buffer'?
                     ;; Check that the function did what the entry
                     ;; said it would do.
                     (unless (and (= start start-mark)
                                  (= (+ delta end) end-mark))
                       (error "Changes to be undone by function different from announced"))
                     (set-marker start-mark nil)
                     (set-marker end-mark nil))
                 (apply fun-args))
               (unless (eq currbuff (current-buffer))
                 (error "Undo function switched buffer"))
               (setq did-apply t)))
            ;; Element (STRING . POS) means STRING was deleted.
            (`(,(and string (pred stringp)) . ,(and pos (pred integerp)))
             (let ((valid-marker-adjustments nil)
                   (apos (abs pos)))
               (when (or (< apos (point-min)) (> apos (point-max)))
                 (error "Changes to be undone are outside visible portion of buffer"))
               ;; Check that marker adjustments which were recorded
               ;; with the (STRING . POS) record are still valid, ie
               ;; the markers haven't moved.  We check their validity
               ;; before reinserting the string so as we don't need to
               ;; mind marker insertion-type.
               (while (and (markerp (car-safe (car list)))
                           (integerp (cdr-safe (car list))))
                 (let* ((marker-adj (pop list))
                        (m (car marker-adj)))
                   (and (eq (marker-buffer m) (current-buffer))
                        (= apos m)
                        (push marker-adj valid-marker-adjustments))))
               ;; Insert string and adjust point
               (if (< pos 0)
                   (progn
                     (goto-char (- pos))
                     (insert string))
                 (goto-char pos)
                 (insert string)
                 ;; HACK jump after inserted text
                 (goto-char (+ pos (length string))))
               ;; Adjust the valid marker adjustments
               (dolist (adj valid-marker-adjustments)
                 ;; Insert might have invalidated some of the markers
                 ;; via modification hooks.  Update only the currently
                 ;; valid ones (bug#25599).
                 (if (marker-buffer (car adj))
                     (set-marker (car adj)
                                 (- (car adj) (cdr adj)))))))
            ;; (MARKER . OFFSET) means a marker MARKER was adjusted by OFFSET.
            (`(,(and marker (pred markerp)) . ,(and offset (pred integerp)))
             (warn "Encountered %S entry in undo list with no matching (TEXT . POS) entry"
                   next)
             ;; Even though these elements are not expected in the undo
             ;; list, adjust them to be conservative for the 24.4
             ;; release.  (Bug#16818)
             (when (marker-buffer marker)
               (set-marker marker
                           (- marker offset)
                           (marker-buffer marker))))
            (_ (error "Unrecognized entry in undo list %S" next))))
        (setq arg (1- arg)))
      ;; Make sure an apply entry produces at least one undo entry,
      ;; so the test in `undo' for continuing an undo series
      ;; will work right.
      (if (and did-apply
               (eq oldlist buffer-undo-list))
          (setq buffer-undo-list
                (cons (list 'apply 'cdr nil) buffer-undo-list))))
    list)

  :defer-config
  (line-number-mode)
  (column-number-mode)
  (size-indication-mode))

;; disable show-paren-mode by default
(leaf paren
  :hook (after-init-hook . (lambda () (show-paren-mode -1)))
  :init
  (setq show-paren-style 'parenthesis
        show-paren-context-when-offscreen 'overlay))

(leaf uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"))

(leaf outline
  :hook (prog-mode-hook . outline-minor-mode)
  :init
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) "...")))))

(leaf hideshow
  :hook (prog-mode-hook . hs-minor-mode)
  :init
  (setq hs-isearch-open t
        hs-hide-comments-when-hiding-all t)

  ;; display more information
  (defun display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (propertize
                    (format "...%d"
                            (- (count-lines (overlay-start ov) (overlay-end ov)) 1))
                    'face 'shadow))))
  (setq hs-set-up-overlay #'display-code-line-counts)

  ;; ;; define command `hs-toggle-all'
  ;; (add-hook 'hs-minor-mode-hook (lambda () (defvar-local hs-all-hide-p nil)))
  ;; (defun hs-toggle-all ()
  ;;   "Toggle all folds at once."
  ;;   (interactive)
  ;;   (hs-life-goes-on
  ;;    (if (bound-and-true-p hs-all-hide-p)
  ;;        (progn (hs-show-all)
  ;;               (setq-local hs-all-hide-p nil))
  ;;      (hs-hide-all)
  ;;      (setq-local hs-all-hide-p t))))

  )

;; (leaf display-fill-column-indicator
;;   :hook (after-init-hook . global-display-fill-column-indicator-mode)
;;   :init
;;   ;; use white space character
;;   (setq-default display-fill-column-indicator-character ?\u0020))

;; -------------------------- Buffer ------------------------------

(leaf files
  :init
  (setq auto-mode-case-fold nil
        enable-local-variables :all
        save-silently t
        ;; large-file-warning-threshold nil
        confirm-kill-processes nil
        find-file-suppress-same-file-warnings t
        find-file-visit-truename t)

  ;; ISSUE https://github.com/emacsorphanage/osx-trash/issues/5#issuecomment-882759527
  (setq delete-by-moving-to-trash t)
  (when sys/macp
    (setq trash-directory "~/.Trash"))

  ;; auto-save
  (add-hook 'after-init-hook #'auto-save-visited-mode)
  (setq make-backup-files nil
        create-lockfiles nil
        auto-save-default t
        auto-save-visited-interval 10)
  (setq auto-save-visited-predicate
        (lambda () (and (buffer-modified-p))))
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
          "^/tmp/" "^/private/tmp/" "^/var/folders/.+$" "/share/emacs/.+$" "\\.git/.+$"
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
  :init (setq comment-empty-lines t)
  :bind ([remap comment-dwim] . #'newcomment-toggle)
  :defer-config
  (defun newcomment-toggle (n)
    "Toggle the comments."
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
  (setq smerge-command-prefix "\e"
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

(leaf project
  :require t
  :init
  (setq project-vc-merge-submodules nil))

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

(leaf executable
  :init
  (setq executable-prefix-env t))

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
  :bind
  (:transient-map
   ((kbd "ESC") . transient-quit-one)
   ((kbd "<escape>") . transient-quit-one))
  :init
  (setq transient-highlight-mismatched-keys nil
        transient-detect-key-conflicts t))

(leaf xwidget
  :hook (xwidget-webkit-mode-hook . xwidget-setup)
  :init
  (setq xwidget-webkit-buffer-name-format "*xwidge: %10T*")
  (defun xwidget-setup ()
    "docstring"
    (goto-address-mode -1)))

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
      word-wrap-by-category t)

(setq-default fill-column 80
              tab-width 4
              fringes-outside-margins t
              fringe-indicator-alist nil
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


;; ---------------------- disabled command -------------------------

;; disable all commands
;; (setq disabled-command-functions nil)

(defvar disabled-command-list '(set-goal-column help-fns-edit-variable))
(mapatoms (lambda (sym)
            (when (and (commandp sym)
                       (get sym 'disabled)
                       (not (member sym disabled-command-list)))
              (put sym 'disabled nil))))

;; -------------------- library protection ------------------------

(defun lisp-directory-read-only ()
  "Set all built-in library read-only."
  (when (file-in-directory-p buffer-file-name lisp-directory)
    (read-only-mode)))

(add-hook #'find-file-hook #'lisp-directory-read-only)

(provide 'init-builtin)
;;; init-builtin.el ends here

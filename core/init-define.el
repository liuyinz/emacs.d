;;; init-define.el --- Definition by user -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ------------------------- Variable -----------------------------

(defconst my/dir-core
  (expand-file-name "core/" user-emacs-directory)
  "User dir for Emacs configs.")

(defconst my/dir-ext
  (expand-file-name "ext/" user-emacs-directory)
  "User dir for external tools.")

(defconst my/dir-lib
  (expand-file-name "lib/" user-emacs-directory)
  "User dir for submodules.")

(defconst my/dir-cache
  (expand-file-name ".cache/" user-emacs-directory)
  "User dir for recentf,places and so on.")

(defconst my/config-debug
  (expand-file-name "init-debug.el" my/dir-core)
  "Mininmal config for debug.")

(defconst user-home-page
  "https://github.com/liuyinz"
  "The Github Page of mine.")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25)
           (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

(defconst emacs/>=25.3p
  (or emacs/>=26p
      (and (= emacs-major-version 25)
           (>= emacs-minor-version 3)))
  "Emacs is 25.3 or above.")

(defconst emacs/>=28.1p
  (or emacs/>=29p
      (and (= emacs-major-version 28)
           (>= emacs-minor-version 1)))
  "Emacs is 28.1 or above.")

;; -------------------------- Macro -------------------------------
(require 'cl-lib)

(defmacro message! (arg)
  "Echo `ARG' info."
  `(message (concat "[" (symbol-name ',arg) "] >> TYPE: %s , VALUE: %s ")
            (type-of (symbol-value ',arg)) ,arg))

;; time
(defmacro time-count! (&rest body)
  "Measure the time (ms) it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (* 1000.0 (float-time (time-since time)))))

;; Mutation
(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

(defmacro run-general! (fn-r fn)
  "Expand as one command to rule all.
FN-R : region function, FN: default function"
  `(if (use-region-p)
       (funcall ',fn-r (region-beginning) (region-end))
     (funcall ',fn)))

(defmacro define-command-mixed (func-name thing-type prompt body)
  ""
  `(defun ,func-name ()
     (interactive)
     (let* ((sym-here (thing-at-point ',thing-type t))
            (default-val
              (if sym-here
                  (format
                   (propertize "(default %s)" 'face 'font-lock-doc-face)
                   (propertize sym-here 'face 'font-lock-variable-name-face))
                ""))
            (query (if (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))
                     (read-string
                      (format "%s %s: " ,prompt default-val)
                      nil nil sym-here))))
       ,body)))


;; ------------------------- Function -----------------------------

(defun debug-start-p ()
  "Return t if mininal debug start."
  (/= (file-attribute-size (file-attributes my/config-debug)) 551))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-carrage-returns ()
  "Delete `^M' characters in the buffer.
Same as `replace-string C-q C-m RET RET'."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun delete-both ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-both (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun rename-this-file (new-name)
  "Renames current file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (when (file-exists-p filename)
      (rename-file filename new-name 1))))

(defun file-absolute-path ()
  "Return visited file absolute-path."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (message "File Path: '%s' " filename)
    (kill-new filename)))

(defun buffer-base-name ()
  "Return buffer name."
  (interactive)
  (let ((name (buffer-name)))
    (message "Buffer name: '%s' " name)
    (kill-new name)))

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;; FIXME https://github.com/seagle0128/.emacs.d/blob/0d96e2c1acfd5f34110b567cc2f3ecfead8b96f5/lisp/init-funcs.el#L129
(defun webkit-browse-url (url &optional pop-buffer new-session)
  "Browse URL with webkit and switch or pop to the buffer.
POP-BUFFER specifies whether to pop to the buffer.
NEW-SESSION specifies whether to create a new xwidget-webkit session."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (when (and (featurep 'xwidget-internal)
             (fboundp 'xwidget-buffer)
             (fboundp 'xwidget-webkit-current-session))
    (xwidget-webkit-browse-url url new-session)
    (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
      (when (buffer-live-p buf)
        (and (eq buf (current-buffer)) (quit-window))
        (if pop-buffer
            (pop-to-buffer buf)
          (switch-to-buffer buf))))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (if-let ((filename (if (equal major-mode 'dired-mode)
                         default-directory
                       (buffer-file-name))))
      (progn
        (kill-new filename)
        (message "Copied '%s'" filename))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))

(defun browse-homepage ()
  "Browse the Github page of Centaur Emacs."
  (interactive)
  (browse-url user-home-page))

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun capitalize-first (s)
  "Capitalize the first char of string S."
  (if (> (length s) 0)
      (concat (upcase (substring s 0 1)) (downcase (substring s 1)))
    nil))

(defun save-and-kill ()
  "Save and kill current buffer."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (save-buffer)
    (kill-buffer nil)))

(defun kill-buffer-silently ()
  "Kill current buffer silently."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer nil)))

(defun ad/silent-message (original &rest args)
  "Silent function `ORIGINAL' message."
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply original args)))

(defun indent-whole-buffer ()
  "Indent whole buffer."
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (indent-region begin end nil)
    (whitespace-cleanup-region begin end)))

(defun indent-dir-files (dir ext)
  "Formatting files with EXT in DIR."
  (interactive (list (read-directory-name "Directory: ")
                     (read-string "File extension: " ".")))
  (let ((bufs (buffer-list))
        (cur (current-buffer)))
    (dolist (file (directory-files-recursively dir ext))
      (find-file file)
      (indent-region (point-min) (point-max))
      (save-buffer)
      (unless (member (get-file-buffer file) bufs)
        (kill-buffer nil)))
    (switch-to-buffer cur)))

(defun toggle-profiler ()
  "Start,stop or report in one command."
  (interactive)
  (if (not (or (profiler-cpu-running-p) (profiler-memory-running-p)))
      (profiler-start 'cpu+mem)
    (profiler-stop)
    (profiler-report)))

(defun cjk-font-setting (font scale)
  "Set cjk `FONT' with `SCALE'."
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family font))
    (setq face-font-rescale-alist '((font . scale)))))

(defun open-in-other-window (&rest _)
  "Open in other window."
  (switch-to-buffer-other-window (current-buffer)))

(defun mode-hook-toggle (hook alist)
  "When `HOOK' is called, toggle modes listed in `ALIST'.
`ALIST' is consisted of '(MODE . ENABLE)'."
  (mapc (lambda (s) (let ((mode (car s))
                          (enable (cdr s)))
                      (when (fboundp mode)
                        (funcall mode (when (xor hook enable) -1)))))
        alist))

(defun tty-frame-list ()
  "Return a list of all tty frames, except the daemon <frame F1>."
  (seq-filter (lambda (f) (frame-parameter f 'tty))
              (frame-list)))

(defun gui-frame-list ()
  "Return a list of all gui frames."
  (seq-filter (lambda (f) (frame-parameter f 'display))
              (frame-list)))

(defun shell-command-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a cons."
  (with-temp-buffer
    (cons (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

;; --------------------------- Hook -------------------------------

;; SEE https://www.reddit.com/r/emacs/comments/lelbr5/how_to_start_emacsclient_such_that_it_respects_my/gmhbyv7?utm_source=share&utm_medium=web2x&context=3
;; https://github.com/purcell/emacs.d/blob/adf337dfa8c324983e5dc01ed055a34c3cc4a964/lisp/init-frame-hooks.el

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(defvar after-make-console-frame-hook '()
  "Hooks to run after creating a new TTY frame.")

(defvar after-make-window-system-frame-hook '()
  "Hooks to run after creating a new window-system frame.")

(defun my/frame-setup ()
  "Setup for frame related hooks."
  (run-hooks (if (display-graphic-p)
                 'after-make-window-system-frame-hook
               'after-make-console-frame-hook)))
(add-hook 'server-after-make-frame-hook #'my/frame-setup)

(defun my/frame-no-server-setup (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (unless (daemonp)
    (with-selected-frame frame
      (my/frame-setup))))
(add-hook 'after-make-frame-functions 'my/frame-no-server-setup)

(defconst my/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")
(add-hook 'after-init-hook
          (lambda () (when my/initial-frame
                       (my/frame-no-server-setup my/initial-frame))))

(provide 'init-define)
;;; init-define.el ends here

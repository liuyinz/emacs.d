;;; init-lib.el --- Macro and Function defined by user -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;;; Code:

(require 'cl-lib)

(require 'init-const)

(defvar socks-noproxy)
(defvar socks-server)

;;; -------------------------- Macro ----------------------------------
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
  `(if (region-active-p)
       (funcall ',fn-r (region-beginning) (region-end))
     (funcall ',fn)))

;;; -------------------------- Functions ----------------------------------
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

;; TRICK @https://github.com/seagle0128/.emacs.d/blob/0d96e2c1acfd5f34110b567cc2f3ecfead8b96f5/lisp/init-funcs.el#L129
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

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))

;; Browse the homepage
(defun browse-homepage ()
  "Browse the Github page of Centaur Emacs."
  (interactive)
  (browse-url my-homepage))

;; Misc
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

;; silently message
(defun silent-message-advice (original &rest args)
  "Silent function `ORIGINAL' message."
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply original args)))

(defun elisp-format ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

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

(defun mode-hook-toggle (mode list &optional reverse)
  "Toggle modes in `LIST' according to `MODE' value.
with `REVERSE' is t, diable modes instead."
  (mapc (lambda (s) (funcall s (unless (xor mode reverse) -1))) list))

;; ;; @https://emacs.stackexchange.com/a/36253
;; (defun consult-consult ()
;;   "call command related to consult"
;;   (interactive)
;;   (setq unread-command-events (nconc
;;                                (listify-key-sequence "consult- ")
;;                                unread-command-events))
;;   (call-interactively #'execute-extended-command))

;; UI
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(provide 'init-lib)
;;; init-lib.el ends here

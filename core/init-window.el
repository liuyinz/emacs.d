;;; init-window.el --- window setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf window
  :bind
  :init
  (setq window-min-height 1
        fit-window-to-buffer-horizontally t
        delete-window-choose-selected 'pos))

(leaf transpose-frame)

(leaf shackle
  :hook (after-init-hook . shackle-mode)
  :init
  (setq shackle-default-rule nil)
  (setq shackle-rules
        ;; SEE https://github.com/seagle0128/.emacs.d/blob/320ae719a1acb84c047e94bb6ee3f33e426f7b47/lisp/init-window.el#L204
        '(
          ;; builtin
          (("*Warnings*" "*Messages*") :size 0.3 :align 'below)
          ("\\*[Wo]*Man.*\\*" :regexp t :popup t :select t :size 0.5 :align 'below)
          ;; third-party

          ;; BUG other,no-select
          ;; ("elisp-demos.org" :select nil :other t)

          ("*format-all-errors*" :size 0.3 :align 'below)
          (helpful-mode :other t :select nil)
          (" *Flycheck checkers*" :select t :size 0.3 :align 'below)
          ((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
           :select t :size 0.25 :align 'below)

          ("*rg*" :select t)
          ("\\`\\*edit-indirect .+\\*\\'" :regexp t :popup t :select t :size 0.4 :align 'below)
          ;; ("*Emacs Log*" :size 0.3 :align 'right)
          )))


(add-to-list 'display-buffer-alist
             '("^\\*Emacs Log\\*"
               (display-buffer-reuse-window
	            display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . right)
               (window-width    . 0.25)))


(leaf popper
  :hook (after-init-hook . popper-mode)
  :init
  (setq popper-display-control t)
  (setq popper-reference-buffers
        '(
          ;; "\\*Messages\\*$"
          "\\*Async Shell Command\\*"
          "\\*Pp Eval Output\\*"
          "\\*Pp Macroexpand Output\\*"
          ;; "\\*quickrun\\*"
          ;; compilation-mode
          vterm-mode quickrun--mode nodejs-repl-mode
          inferior-emacs-lisp-mode
          inferior-python-mode
          shell-mode eshell-mode))

  ;; TODO
  ;; 2. applied even when first pop buffer arise
  (setq popper-display-function #'my/popper-display-switch)

  (defvar my/popper-switches
    '((bottom . 0.4) (right . 0.5) (left . 0.99)))

  (defun my/popper-display-switch (buffer &optional alist)
    (unless (get 'my/popper-switches 'current-index)
      (put 'my/popper-switches 'current-index 0))
    (let* ((switch (nth (get 'my/popper-switches 'current-index)
                        my/popper-switches))
           (win-conf `((side . ,(car switch))
                       (slot . 1)
                       (window-width . ,(cdr switch))
                       (window-height . ,(cdr switch)))))
      (select-window (display-buffer-in-side-window buffer (append alist win-conf)))))

  (defun popper-switch ()
    (interactive)
    (when-let ((window (caar popper-open-popup-alist)))
      (let* ((idx (get 'my/popper-switches 'current-index))
             (next-idx (% (1+ idx) (length my/popper-switches)))
             (args (or (and (memq (car (nth idx my/popper-switches)) '(right left))
                            '(2 . 0))
                       '(3 . 1)))
             (ratio (/ (float (- (nth (car args) (window-edges window))
                                 (nth (cdr args) (window-edges window))))
                       (nth (car args) (window-edges (frame-root-window))))))
        (popper-close-latest)
        (put 'my/popper-switches 'current-index next-idx)
        (setcdr (nth idx my/popper-switches) ratio)))
    (popper-open-latest))
  )

(keymap-global-set "s-s" #'popper-switch)
(keymap-global-set "s-i" #'popper-cycle)
(keymap-global-set "s-t" #'popper-toggle)
(keymap-global-set "s-u" #'vterm-new)


;;; window management

(defvar my/winconf-list nil)

;; TODO refactor and update automatically
(defun my/winconf-switch (&optional new)
  (interactive "P")
  (if new
      (let ((len (length my/winconf-list)))
        (cl-pushnew (current-window-configuration)
                    my/winconf-list
                    :test #'window-configuration-equal-p)
        (when (= (- (length my/winconf-list) len) 1)
          (message "Add new layout !")))
    (if my/winconf-list
        (let* ((current (current-window-configuration))
               (len (length my/winconf-list))
               (head (car my/winconf-list))
               (head-p (window-configuration-equal-p current head)))
          (if (not head-p)
              (set-window-configuration head)
            (if (= len 1)
                (message "Already on the only window layout yet.")
              (set-window-configuration (cadr my/winconf-list))
              (setq my/winconf-list (nconc (cdr my/winconf-list) (list head))))))
      (message "No window layout saved yet."))))

(defun toggle-one-window ()
  "Toggle between window layout between multi and one."
  (interactive)
  (if (length= (cl-remove-if #'window-dedicated-p (window-list)) 1)
      (if-let ((saved (get 'toggle-one-window 'saved)))
          (progn
            (set-window-configuration saved)
            (put 'toggle-one-window 'saved nil))
        (message "No saved window configuration."))
    (put 'toggle-one-window 'saved (current-window-configuration))
    (delete-other-windows)))

(provide 'init-window)
;;; init-window.el ends here

;;; init-window.el --- window setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf window
  :bind
  ("C-x 7" . transpose-windows)
  :init
  (setq window-min-height 1
        fit-window-to-buffer-horizontally t
        delete-window-choose-selected 'pos)
  (defun transpose-windows ()
    "Transpose two windows.  If more or less than two windows are visible, error."
    (interactive)
    (unless (= 2 (count-windows))
      (error "There are not 2 windows."))
    (let* ((windows (window-list))
           (w1 (car windows))
           (w2 (nth 1 windows))
           (w1b (window-buffer w1))
           (w2b (window-buffer w2)))
      (set-window-buffer w1 w2b)
      (set-window-buffer w2 w1b)
      (other-window 1))))

(leaf shackle
  :hook (after-init-hook . shackle-mode)
  :init
  (setq shackle-default-rule nil)
  (setq shackle-rules
        ;; SEE https://github.com/seagle0128/.emacs.d/blob/320ae719a1acb84c047e94bb6ee3f33e426f7b47/lisp/init-window.el#L204
        '(
          ;; builtin
          (("*Warnings*" "*Messages*") :size 0.3 :align 'below)
          (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 :align 'below)
          ("\\*[Wo]*Man.*\\*" :regexp t :popup t :select t :size 0.5 :align 'below)
          ("*Pp Eval Output*" :popup t :size 0.5 :align 'below :select nil)
          ("*Pp Macroexpand Output*" :popup t :size 0.5 :align 'below :select nil)
          ;; third-party
          ;; ("*evil-marks*" :align 'below :size 0.4)

          ("*vterm*" :align 'below :size 0.4)
          ;; ("*quickrun*" :select t :size 0.4 :align 'below)
          ("*Python*" :select t :size 0.4 :align 'below)
          ("*nodejs*" :select t :size 0.4 :align 'below)

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
             '("^\\*quickrun"
               (display-buffer-reuse-window
	            display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.3)))

(add-to-list 'display-buffer-alist
             '("^\\*Emacs Log*"
               (display-buffer-reuse-window
	            display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . right)
               (window-width    . 0.25)))

;; ------------------------- commands -----------------------------

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

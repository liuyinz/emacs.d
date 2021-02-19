;;; init-gcmh.el --- garbadge collection magic hack  -*- lexical-binding: t no-byte-compile: t *-
;;; Commentary:
;;; Code:

;; Speed up startup
;; If you experience freezing,decrease. If you experience stuttering, increase.
(defvar gc-cons-default `(,(* 16 1024 1024) 0.1)
  "The default value for `gc-cons-threshold' and `gc-cons-percentage'.")

(defvar gc-cons-max `(,(* 512 1024 1024) 0.6)
  "The temporary value for `gc-cons-threshold' and `gc-cons-percentage'.")

(defvar gc-timer-default
  (run-with-idle-timer 30 t #'garbage-collect)
  "Run garbarge collection when idle 30s.")

(defvar file-name-handler-alist-default file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold (car gc-cons-max)
      gc-cons-percentage (cadr gc-cons-max))

(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist file-name-handler-alist-default
                  gc-cons-threshold (car gc-cons-default)
                  gc-cons-percentage (cadr gc-cons-default))

            ;; GC automatically while unfocusing the frame
            (add-function :after after-focus-change-function
                          (lambda ()
                            (unless (frame-focus-state)
                              (garbage-collect))))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (add-hook 'minibuffer-setup-hook
                      (lambda () (setq gc-cons-threshold (car gc-cons-max))))
            (add-hook 'minibuffer-exit-hook
                      (lambda () (setq gc-cons-threshold (car gc-cons-default))))))

(provide 'init-gcmh)
;;; init-gcmh.el ends here

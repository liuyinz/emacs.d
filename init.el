;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
; (setq debug-on-error t)
; (debug-on-entry 'package-initialize)

(when (version< emacs-version "27")
  (error "please upgrade your emacs-version above 27 !"))

;; avoid cl depreciated warning
(setq byte-compile-warnings '(cl-function))

;; Speed up startup
;; If you experience freezing,decrease. If you experience stuttering, increase.
(defvar gc-cons-default `(,(* 16 1024 1024) 0.1)
  "The default value for `gc-cons-threshold' and `gc-cons-percentage'.")

(defvar gc-cons-max `(,(* 512 1024 1024) 0.6)
  "The temporary value for `gc-cons-threshold' and `gc-cons-percentage'.")

(defvar gc-timer-default
  (run-with-idle-timer 15 t
                       (lambda ()
                          (message "Garbage Collector has run for %.03fs"
                                   (time-count (garbage-collect)))))
  "Run garbarge collection when idle 15s.")

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
            (defun my-minibuffer-gc-max ()
              (setq gc-cons-threshold (car gc-cons-max)))

            (defun my-minibuffer-gc-default ()
              (setq gc-cons-threshold (car gc-cons-default)))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-gc-max)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-gc-default)))

;; load core/init files
(with-temp-message ""
  ;; benchmark
  (require 'init-benchmark)

  (require 'init-const)
  (require 'init-funcs)
  (require 'init-package)
  (require 'init-default)

  (require 'init-ivy)
  (require 'init-company)

  ;; test
  ;; (require 'init-test)

  ;; ui
  (require 'init-ui)
  (require 'init-highlight)
  ;; (require 'init-frame)
  (require 'init-window)
  (require 'init-ibuffer)
  (require 'init-dired)
  (require 'init-org)
  (require 'init-edit)
  (require 'init-shell)
  (require 'init-tool)
  (require 'init-rg)

  ;; programing
  (require 'init-vcs)
  (require 'init-project)
  (require 'init-flycheck)
  (require 'init-format)
  (require 'init-quickrun)
  (require 'init-code)
  (require 'init-lsp)

  ;; language
  (require 'init-prog)
  (require 'init-elisp)
  (require 'init-web)
  (require 'init-js)

  ;; (require 'init-evil)
  (require 'init-meow))

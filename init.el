;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
; (setq debug-on-error t)
; (debug-on-entry 'package-initialize)

(when (version< emacs-version "27")
  (error "please upgrade your emacs-version above 27 !"))

;; avoid cl depreciated warning
(setq byte-compile-warnings '(cl-function))

;; Speed up startup
(defvar gc-cons-threshold-default (* 16 1024 1024)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar gc-cons-threshold-max (* 400 1024 1024)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar gc-timer-idle (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar file-name-handler-alist-default file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold gc-cons-threshold-max
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist file-name-handler-alist-default
                  gc-cons-threshold gc-cons-threshold-default
                  gc-cons-percentage 0.1)

            ;; GC automatically while unfocusing the frame
            (add-function :after after-focus-change-function
              (lambda ()
                (unless (frame-focus-state)
                  (garbage-collect))))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun gc-max ()
              (setq gc-cons-threshold gc-cons-threshold-max))

            (defun gc-default ()
              (setq gc-cons-threshold gc-cons-threshold-default))

            (add-hook 'minibuffer-setup-hook #'gc-max)
            (add-hook 'minibuffer-exit-hook #'gc-default)))

;; load core/init files
(with-temp-message ""
  ;; benchmark
  (require 'init-benchmark)

  (require 'init-const)
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
  (require 'init-meow)
  (require 'init-funcs))

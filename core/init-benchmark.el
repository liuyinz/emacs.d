;;; init-benchmark.el --- Measure startup time -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; ISSUE https://github.com/dholm/benchmark-init-el/issues/15#issuecomment-766083560
(define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (docstring (if ll (pop ll) nil)))
    (list obsolete-name current-name when docstring)))

(require 'benchmark-init-modes)
(require 'benchmark-init)
(benchmark-init/activate)
(add-hook 'after-init-hook #'benchmark-init/deactivate)

(defun benchmark-show-init-time ()
  "Show startup time."
  (message "init completed in %.2fms"
           (* 1000.0 (float-time (time-subtract after-init-time before-init-time)))))
(add-hook 'after-init-hook #'benchmark-show-init-time)

(provide 'init-benchmark)
;;; init-benchmark.el ends here

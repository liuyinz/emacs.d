;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

(when (version< emacs-version "27")
  (error "please upgrade your emacs-version above 27 !"))

(setq byte-compile-warnings '(cl-function))

(load-file (expand-file-name "core/init-const.el" user-emacs-directory))

(let (;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

  ;; Load path
  ;; Optimize: Force "elpa" and "core" at the head to reduce the startup time.
  (defun update-load-path (&rest _)
    "Update `load-path'."
    (dolist (dir (mapcar #'symbol-value '(my-dir-core my-dir-elpa my-dir-github)))
      (push dir load-path)))

  (defun add-subdirs-to-load-path (&rest _)
    "Add subdirectories to `load-path'."
    (dolist (dir (mapcar #'symbol-value '(my-dir-elpa my-dir-github)))
      (let ((default-directory dir))
        (normal-top-level-add-subdirs-to-load-path))))

  (advice-add #'package-initialize :after #'update-load-path)
  (advice-add #'package-initialize :after #'add-subdirs-to-load-path)

  (update-load-path)

  ;; load core/init files
  (with-temp-message ""
    ;; benchmark
    (require 'init-benchmark)

    (require 'init-const)
    (require 'init-funcs)

    (require 'init-package)
    (require 'init-system)
    (require 'init-builtin)

    (require 'init-ivy)
    (require 'init-company)

    ;; test
    ;; (require 'init-test)

    ;; ui
    (require 'init-ui)
    (require 'init-highlight)
    (require 'init-frame)
    (require 'init-window)
    (require 'init-ibuffer)
    (require 'init-dired)
    (require 'init-org)
    (require 'init-edit)
    (require 'init-shell)
    (require 'init-tool)
    ; (require 'init-rg)

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

    (require 'init-evil)
    ))

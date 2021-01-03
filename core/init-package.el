;; <package start>
(require 'package)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  ; (package-initialize)
  )

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t
        use-package-minimum-reported-time 0.01
        use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))
(use-package blackout)
(use-package bind-key)
(use-package gnu-elpa-keyring-update :demand)

;; Auto update packages
(use-package auto-package-update
  :commands auto-package-update-now
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results nil)
  (defalias 'upgrade-packages #'auto-package-update-now))

(use-package quelpa
  :demand
  :init (setq quelpa-self-upgrade-p nil
              quelpa-checkout-melpa-p nil
              quelpa-update-melpa-p nil
              quelpa-upgrade-p t
              quelpa-dir (expand-file-name "quelpa" my-dir-cache))
  :config
  (use-package quelpa-use-package
  :demand
  ;;TO-DO
  :init (setq quelpa-use-package-inhibit-loading-quelpa t)))
;; <package end>

;; Environment
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand
  :defines exec-path-from-shell-check-startup-files
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
        exec-path-from-shell-arguments '("-l"))
  :config
  ;; Cache $PATH once for all, refer @https://github.com/manateelazycat/cache-path-from-shell/blob/master/cache-path-from-shell.el
  (defvar cache-path-from-shell-loaded-p nil)
  (defadvice exec-path-from-shell-initialize (around cache-path-from-shell-advice activate)
    (if cache-path-from-shell-loaded-p
        (message "All shell environment variables has loaded in Emacs, yow!")
      (setq cache-path-from-shell-loaded-p t)
      ad-do-it))
  (exec-path-from-shell-initialize))

;; keep ~/.emacs.d clean
(use-package no-littering
  :demand
  :defines recentf-exclude
  :functions (no-littering-expand-etc-file-name no-littering-expand-var-file-name)
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" my-dir-cache)
        no-littering-var-directory (expand-file-name "var/" my-dir-cache))
  :config
  ;; exclude these in recentf
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  ;; save auto-save file if needed
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; save custom.el here
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;; (use-package with-proxy
;;   :init
;;   (setq with-proxy-http-server "127.0.0.1:7890"))

(provide 'init-package)

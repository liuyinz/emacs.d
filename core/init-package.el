;; <package start>
(require 'package)

;; package.el
(setq package-user-dir my-dir-elpa
      package-archives elpa-tsinghua)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

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

;; (setq package-check-signature nil)
(use-package gnu-elpa-keyring-update :demand)

(use-package quelpa
  :commands quelpa-read-cache
  :init (setq quelpa-self-upgrade-p nil
              quelpa-checkout-melpa-p nil
              quelpa-update-melpa-p nil
              quelpa-upgrade-p t
              quelpa-dir (expand-file-name "quelpa" my-dir-cache)))

(use-package quelpa-use-package
  :demand
  ;;TO-DO
  :init (setq quelpa-use-package-inhibit-loading-quelpa t))

;; Auto update packages
(use-package auto-package-update
  :commands auto-package-update-now
  :hook (auto-package-update-after . (lambda ()
                                       (quelpa-upgrade-all)
                                       (package-quickstart-refresh)))
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results nil)
  (defalias 'upgrade-packages #'auto-package-update-now))
;; <package end>

(provide 'init-package)

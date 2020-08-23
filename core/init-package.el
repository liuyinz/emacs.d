(require 'init-const)

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

(setq package-user-dir my-dir-elpa)
(setq package-archives
      '(
        ;; Tsinghua
        ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")

        ;; Tencent cloud
        ; ("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
        ; ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ; ("org" . "http://mirrors.cloud.tencent.com/elpa/org/")
        ; ("melpa-stable" . "http://mirrors.cloud.tencent.com/elpa/melpa-stable/")

        ;; ;; Emacs-china
        ;; ("gnu"   . "http://elpa.emacs-china.org/gnu/")
        ;; ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ;; ("org" . "http://elpa.emacs-china.org/org/")
        ;; ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")

        ))

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
  (require 'use-package)
  (setq use-package-compute-statistics t))

;; (use-package benchmark-init
;;   :demand
;;   :config (benchmark-init/activate)
;;   :hook (after-init . benchmark-init/deactivate))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)
;; (use-package use-package-ensure-system-package)

(setq package-check-signature nil)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update
  :demand)

;; Auto update packages
(use-package auto-package-update
  :commands auto-package-update-now
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results nil)
  (defalias 'upgrade-packages #'auto-package-update-now))

(provide 'init-package)

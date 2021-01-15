;;; init-selectrum.el --- Config for selectrum  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package selectrum
  :blackout (selectrum-mode)
  :hook (after-init . selectrum-mode)
  :init
  (setq selectrum-extend-current-candidate-highlight t
        selectrum-count-style 'current/matches)
  :bind (
         ("C-x C-z" . selectrum-repeat)
         ))

(use-package selectrum-prescient
  ;; :disabled
  :blackout (selectrum-prescient-mode prescient-persist-mode)
  :hook (selectrum-mode . (lambda()
                            (selectrum-prescient-mode)
                            (prescient-persist-mode)))
  :init
  (setq prescient-history-length 300))


(use-package embark
  :bind(
        :map selectrum-minibuffer-map
        ("C-c C-o" . embark-export)
        ("C-c C-c" . embark-act)))

(use-package consult
  :bind(
        ( [remap switch-to-buffer] . consult-buffer)
        ( [remap switch-to-buffer-other-window] . consult-buffer-other-window)
        ( [remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
        ))
;; (when (executable-find "rg")
;;   (global-set-key (kbd "M-?") 'consult-ripgrep))

(use-package embark-consult)

(use-package consult-flycheck)

(use-package marginalia
  :hook (after-init . (lambda () (marginalia-mode 1)))
  :config
  (setq-default marginalia-annotators '(marginalia-annotators-heavy)))


(provide 'init-selectrum)
;;; init-selectrum.el ends here

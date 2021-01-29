;;; init-selectrum.el --- Config for selectrum  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf orderless
  :require t
  :init (setq completion-styles '(orderless)))

(leaf selectrum
  :blackout t
  :hook (after-init-hook . selectrum-mode)
  :bind
  ("C-x C-z" . selectrum-repeat)
  (:selectrum-minibuffer-map
   ([escape] . minibuffer-keyboard-quit))
  :init
  (setq selectrum-max-window-height 15
        selectrum-extend-current-candidate-highlight t
        selectrum-count-style 'current/matches
        selectrum-complete-in-buffer nil)
  :config
  (with-eval-after-load 'orderless
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)))

(leaf selectrum-prescient
  :require t
  :blackout (selectrum-prescient-mode prescient-persist-mode)
  :hook (selectrum-mode-hook . (lambda()
                                 (selectrum-prescient-mode)
                                 (prescient-persist-mode)))
  :init (setq prescient-history-length 300))

(leaf consult
  :bind
  ( [remap switch-to-buffer] . consult-buffer)
  ( [remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ( [remap switch-to-buffer-other-frame] . consult-buffer-other-frame))
;; (when (executable-find "rg")
;;   (global-set-key (kbd "M-?") 'consult-ripgrep))

(leaf embark
  :require t
  :after selectrum
  :bind(:selectrum-minibuffer-map
        ("C-c C-o" . embark-export)
        ("C-c C-c" . embark-act))
  :config
  (leaf embark-consult
    :blackout t
    :hook (embark-collect-mode-hook . embarkconsult-preview-minor-mode)))

;; (leaf embark-consult
;;   :blackout t
;;   :after (embark consult)
;;   :hook (embark-collect-mode-hook . embarkconsult-preview-minor-mode))

;; (leaf consult-flycheck)

(leaf marginalia
  :hook (selectrum-mode-hook . marginalia-mode)
  :bind
  (:minibuffer-local-map
   ("C-M-a" . marginalia-cycle))
  (:embark-general-map
   ("A" . marginalia-cycle))
  :config
  (setq-default marginalia-annotators '(marginalia-annotators-heavy))
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))


(provide 'init-selectrum)
;;; init-selectrum.el ends here

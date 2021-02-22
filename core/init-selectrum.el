;;; init-selectrum.el --- Config for selectrum  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf selectrum
  :blackout t
  :hook (after-init-hook . selectrum-mode)
  :init
  (setq selectrum-max-window-height 15
        selectrum-fix-vertical-window-height t
        selectrum-right-margin-padding 0
        selectrum-extend-current-candidate-highlight t
        selectrum-count-style 'current/matches)
  :config
  ;; sorting
  (leaf selectrum-prescient
    :require t
    :blackout t
    :init
    (setq prescient-history-length 300)
    :config
    (prescient-persist-mode)
    (selectrum-prescient-mode))

  ;; filtering
  (leaf orderless
    :after selectrum-prescient
    :require t
    :config
    (setq completion-styles '(orderless))
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))
  )

(leaf consult
  :require t
  :init
  (setq consult-async-min-input 1)
  ;; (setq consult-project-root-function 'project-roots)
  :config
  ;; @https://emacs.stackexchange.com/a/36253
  (defun consult-consult ()
    "call command related to consult"
    (interactive)
    (setq unread-command-events (nconc
                                 (listify-key-sequence "consult- ")
                                 unread-command-events))
    (call-interactively #'execute-extended-command)))

(leaf embark
  :require t
  :after selectrum
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
  ;; :bind
  ;; (:minibuffer-local-map
  ;;  ("C-M-a" . marginalia-cycle))
  ;; (:embark-general-map
  ;;  ("A" . marginalia-cycle))
  :config
  (setq-default marginalia-annotators '(marginalia-annotators-heavy nil))
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))


(provide 'init-selectrum)
;;; init-selectrum.el ends here

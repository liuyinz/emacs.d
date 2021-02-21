;;; init-selectrum.el --- Config for selectrum  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf prescient
  :blackout prescient-persisit-mode
  :hook (after-init-hook . prescient-persist-mode)
  :init (setq prescient-history-length 300))

(leaf orderless
  :require t
  ;; :commands orderless-filter orderless-highlight-matches
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
        selectrum-fix-vertical-window-height t
        selectrum-right-margin-padding 0
        selectrum-extend-current-candidate-highlight t
        selectrum-count-style 'current/matches
        ;; selectrum-complete-in-buffer nil
        )
  :config
  (with-eval-after-load 'orderless
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)))

(leaf selectrum-prescient
  :blackout selectrum-prescient-mode
  :hook (selectrum-mode-hook . selectrum-prescient-mode))

(leaf consult
  :init
  (setq consult-async-min-input 1)
  :bind
  ;; ("C-c h" . consult-history)
  ("C-c m" . consult-mode-command)
  ;; ("C-c b" . consult-bookmark)
  ("C-c k" . consult-kmacro)
  ;; C-x bindings (ctl-x-map)
  ("C-x M-:" . consult-complex-command)
  ("C-x b" . consult-buffer)
  ;; Custom M-# bindings for fast register access
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (UNRELATED!)
  ("C-M-#" . consult-register)
  ;; Other custom bindings
  ("M-y" . consult-yank-pop)                ;; orig. yank-pop
  ("<help> a" . consult-apropos)            ;; orig. apropos-command
  ;; M-g bindings (goto-map)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-project-imenu) ;; Alternative: consult-imenu
  ;; ("M-g e" . consult-error)
  ;; M-s bindings (search-map)
  ("M-s g" . consult-git-grep)              ;; alt. consult-grep, consult-ripgrep
  ("M-s f" . consult-find)                  ;; alt. consult-locate, find-fd
  ("M-s l" . consult-line)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Replacement for isearch-edit-string
  ("M-s e" . consult-isearch)
  (:isearch-mode-map
   ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch)))

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

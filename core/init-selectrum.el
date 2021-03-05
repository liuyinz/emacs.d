;;; init-selectrum.el --- selectrum suits -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(leaf selectrum
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
    :init
    (setq prescient-history-length 300
          prescient-aggressive-file-save t
          prescient-sort-length-enable nil
          prescient-sort-full-matches-first t)
    :config
    (prescient-persist-mode)
    (selectrum-prescient-mode))

  ;; filtering
  (leaf orderless
    :after selectrum-prescient
    :require t
    :config

    ;; @https://github.com/oantolin/orderless/blob/master/README.org#style-dispatchers
    ;; dispatchers
    (defun without-if-bang (pattern _index _total)
      "!pattern : exclude pattern."
      (when (string-prefix-p "!" pattern)
        `(orderless-without-literal . ,(substring pattern 1))))

    (defun initialism-if-at (pattern _index _total)
      "@pattern : first letter of word in order."
      (when (string-prefix-p "@" pattern)
        `(orderless-initialism . ,(substring pattern 1))))

    (setq orderless-style-dispatchers '(initialism-if-at without-if-bang))
    (setq orderless-component-separator #'orderless-escapable-split-on-space)

    ;; selectrum setting
    ;; @https://github.com/oantolin/orderless/blob/master/README.org#selectrum
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)))

(leaf marginalia
  :hook (selectrum-mode-hook . marginalia-mode)
  :config
  (setq-default marginalia-annotators '(marginalia-annotators-heavy nil))
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

(leaf consult
  :require t
  :after selectrum
  :init
  (setq consult-async-min-input 1)
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-find-command "fd --color=never --full-path ARG OPTS")
  :config
  ;; @https://emacs.stackexchange.com/a/36253
  (defun consult-consult ()
    "call command related to consult"
    (interactive)
    (setq unread-command-events (nconc
                                 (listify-key-sequence "consult- ")
                                 unread-command-events))
    (call-interactively #'execute-extended-command))

  (leaf consult-flycheck :commands consult-flycheck))

(leaf embark
  :require t
  :after consult
  ;; :init (setq embark-prompter 'embark-completing-read-prompter)
  :config
  (leaf embark-consult :require t))
(provide 'init-selectrum)
;;; init-selectrum.el ends here

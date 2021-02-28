;;; init-selectrum.el --- selectrum suits -*- lexical-binding: t no-byte-compile: t -*-

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
    (setq prescient-history-length 300
          prescient-aggressive-file-save t
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
  ;; :bind
  ;; (:minibuffer-local-map
  ;;  ("C-M-a" . marginalia-cycle))
  ;; (:embark-general-map
  ;;  ("A" . marginalia-cycle))
  :config
  (setq-default marginalia-annotators '(marginalia-annotators-heavy nil))
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))
(leaf consult
  ;; :require t
  :commands consult-buffer
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


  ;; add org source, @https://github.com/minad/consult/wiki#org-buffers
  (autoload 'org-buffer-list "org")
  (defvar org-buffer-source
    `(:name     "Org"
                :narrow   ?o
                :category buffer
                :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
  (add-to-list 'consult-buffer-sources 'org-buffer-source 'append)

  (leaf consult-flycheck
    :commands consult-flycheck))

(leaf embark
  :require t
  :after selectrum
  :init
  ;; (setq embark-prompter 'embark-completing-read-prompter)
  :config
  ;; HACK @https://github.com/oantolin/embark/issues/74#issuecomment-753233512
  ;; set virtual-buffer with file actions correctly
  (defun virtual-buffer-dispatch (pair)
    (pcase pair
      (`(virtual-buffer . ,cand)
       (cons (pcase (- (elt cand 0) #x100000)
               ((or ?b ?p) 'buffer)
               ((or ?f ?q) 'file)
               (?m 'bookmark)
               (_ 'general))
             (substring cand 1)))))

  (advice-add 'embark-target-top-minibuffer-completion
              :filter-return 'virtual-buffer-dispatch)

  (leaf embark-consult
    :blackout t
    :after embark consult
    :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode)))

(provide 'init-selectrum)
;;; init-selectrum.el ends here

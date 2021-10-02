;;; init-minibuffer.el --- minibuffer sets -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; SEE https://github.com/purcell/emacs.d/blob/master/lisp/init-minibuffer.el

(leaf vertico
  :hook (after-init-hook . vertico-mode)
  :init
  (setq vertico-cycle t
        vertico-count 15)
  :config

  ;; SEE https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (defun ad/vertico-customize-candidate (orig cand prefix suffix index _start)
    (setq cand (funcall orig cand prefix suffix index _start))
    (concat (if (= vertico--index index)
                (propertize "> " 'face 'hl-todo)
              "  ")
            cand))
  (advice-add #'vertico--format-candidate :around #'ad/vertico-customize-candidate)

  ;;; -------------------------- Eextra ------------------------------

  (leaf vertico-repeat :commands vertico-repeat)
  (leaf vertico-directory
    :commands vertico-directory-delete-char vertico-directory-enter)
  )

(leaf marginalia
  :hook (vertico-mode-hook . marginalia-mode)
  :config
  (setq-default marginalia-annotators '(marginalia-annotators-heavy nil)))

(leaf orderless
  :require t
  :after vertico
  :init
  (setq orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-strict-initialism))

  (setq completion-styles '(orderless))

  :config

  ;; SEE https://github.com/cute-jumper/pinyinlib.el#pinyinlib-build-regexp-string
  (defun ad/orderless-regexp-pinyin (str)
    "Patch `orderless-regexp' with pinyin surpport"
    (setf (car str) (pinyinlib-build-regexp-string (car str)))
    str)
  (advice-add 'orderless-regexp :filter-args #'ad/orderless-regexp-pinyin)

  ;; SEE https://github.com/oantolin/orderless/blob/master/README.org#style-dispatchers
  (defun without-if-bang (pattern _index _total)
    "!pattern : exclude pattern."
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (defun initialism-if-at (pattern _index _total)
    "@pattern : first letter of word in order."
    (cond
     ((equal "@" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "@" pattern)
      `(orderless-initialism . ,(substring pattern 1)))))

  (setq orderless-component-separator #'orderless-escapable-split-on-space
        orderless-style-dispatchers '(initialism-if-at without-if-bang))

  ;; SEE https://github.com/oantolin/orderless#company
  (defun ad/just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))
  (advice-add 'company-capf--candidates :around #'ad/just-one-face)

  )

;; TODO consult-browser-bookmark consult-browser-history
(leaf consult
  :require t
  :after vertico
  :init
  (setq consult-async-min-input 1)
  (setq consult-async-split-style 'semicolon)
  (setq consult-line-start-from-top t)
  (setq consult-find-config "fd --color=never --full-path ARG OPTS")
  (setq consult-project-root-function #'projectile-project-root)

  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<")

  :config

  ;;; -------------------------- Source ------------------------------

  ;; extend hidden source
  (appendq! consult-buffer-filter '("\\`\\*.*\\*\\'"
                                    "\\`.*\\.el\\.gz\\'"
                                    "\\`magit[:-].*\\'"
                                    "\\`COMMIT_EDITMSG\\'"))

  ;; enable hidden buffer preview
  (consult-customize consult--source-hidden-buffer :state #'consult--buffer-state)

  ;; filter `consult--source-buffer'
  (consult-customize consult--source-buffer
                     :items
                     (lambda ()
                       (consult--buffer-query
                        :sort 'visibility
                        :as #'buffer-name
                        :predicate
                        (lambda (buffer)
                          (with-current-buffer buffer
                            (not (or (eq major-mode 'dired-mode)
                                     (string-match "\\`\\*gist-.+\\*/.+\\'"
                                                   (buffer-name)))))))))
  ;; Dired-source
  (defvar consult--source-dired
    `(:name     "Dired"
      :narrow   ?d
      :hidden   t
      :category buffer
      :face     dired-header
      :state    ,#'consult--buffer-state
      :items
      ,(lambda ()
         (consult--buffer-query :mode 'dired-mode
                                :sort 'visibility
                                :as #'buffer-name)))
    "Dired buffer candidate source for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-dired)

  ;; Gist-source
  (defvar consult--source-gist
    `(:name     "Gist"
      :narrow   ?g
      :hidden   t
      :category buffer
      :face     transient-pink
      :state    ,#'consult--buffer-state
      :items
      ,(lambda ()
         (consult--buffer-query :sort 'visibility
                                :as #'buffer-name
                                :predicate
                                (lambda (buffer)
                                  (with-current-buffer buffer
                                    (string-match "\\`\\*gist-.+\\*/.+\\'"
                                                  (buffer-name)))))))
    "Gist buffer candidate source for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-gist)

  ;; Org-source
  (autoload 'org-buffer-list "org")
  (defvar consult--source-org
    `(:name     "Org"
      :narrow   ?o
      :hidden   t
      :category buffer
      :face     org-headline-todo
      :state    ,#'consult--buffer-state
      :items
      ,(lambda ()
         (consult--buffer-query :mode 'org-mode
                                :sort 'visibility
                                :as #'buffer-name)))
    "Org buffer candidate source for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-org)

  ;;; ------------------------- Preview ------------------------------

  (prependq! consult-preview-excluded-hooks
             '(undohist-recover-safe
               save-place-find-file-hook
               recentf-track-opened-file
               auto-revert-find-file-function))

  ;; disable preview
  (consult-customize
   consult-recent-file consult-bookmark consult--source-file
   consult--source-project-file consult--source-bookmark
   consult-ripgrep consult-git-grep consult-grep
   :preview-key nil)

  ;;; -------------------------- Extra -------------------------------

  (unless emacs/>=28.1p
    (require 'consult-xref)
    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  (require 'consult-register)
  ;; Optionally configure the register formatting.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  (with-eval-after-load 'flycheck
    (leaf consult-flycheck :require t))

  (require 'consult-imenu)
  ;; Integrate with `leaf'
  (setq consult-imenu-config
        '((emacs-lisp-mode
           :toplevel "Functions"
           :types
           ((?f "Functions" font-lock-function-name-face)
            (?m "Macros"    font-lock-function-name-face)
            (?p "Leaf"      font-lock-constant-face)
            (?t "Types"     font-lock-type-face)
            (?v "Variables" font-lock-variable-name-face)))
          (sh-mode
           :types
           ((?f "Functions" font-lock-function-name-face)
            (?v "Variables" font-lock-variable-name-face)))
          ))

  ;; (require 'consult-compile)

  ;;; ------------------------- Function -----------------------------

  (setq completion-in-region-function (lambda (&rest args)
                                        (apply (if vertico-mode
                                                   #'consult-completion-in-region
                                                 #'completion--in-region)
                                               args)))
  )

(leaf embark
  :commands embark-act embark-become
  ;; :init (setq embark-prompter 'embark-completing-read-prompter)
  :config
  (leaf embark-consult :require t)

  ;; HACK Open source code of `symbol' in other window
  (advice-add 'embark-find-definition :before #'open-in-other-window))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here

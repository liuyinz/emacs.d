;;; init-minibuffer.el --- minibuffer sets -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; SEE https://github.com/purcell/emacs.d/blob/master/lisp/init-minibuffer.el

(use-package vertico
  :hook (after-init-hook . vertico-mode)
  :init
  (setq vertico-cycle t
        vertico-count 15
        resize-mini-windows t)
  :config
  ;; HACK inspired by vertico-reverse
  (defun ad/vertico--display-prompt-bottom (lines)
    "Set prompt line to bottom in `vertico-mode'."
    (move-overlay vertico--candidates-ov (point-min) (point-min))
    (unless (eq vertico-resize t)
      (setq lines (nconc (make-list (max 0 (- vertico-count (length lines))) "\n") lines)))
    (let ((string (apply #'concat lines)))
      (add-face-text-property 0 (length string) 'default 'append string)
      (overlay-put vertico--candidates-ov 'before-string string))
    (vertico--resize-window (length lines)))
  (advice-add 'vertico--display-candidates :override #'ad/vertico--display-prompt-bottom)

  ;; SEE https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (defun ad/vertico-customize-candidate (orig cand prefix suffix index _start)
    (setq cand (funcall orig cand prefix suffix index _start))
    (concat (if (= vertico--index index)
                (propertize "> " 'face 'hl-todo)
              "  ")
            cand))
  (advice-add #'vertico--format-candidate :around #'ad/vertico-customize-candidate)

  ;; -------------------------- Extra ------------------------------
  )

(use-package marginalia
  :hook (vertico-mode-hook . marginalia-mode)
  :config
  (setq-default marginalia-annotators '(marginalia-annotators-heavy nil)))

(use-package orderless
  :after vertico
  :init
  (setq orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-strict-initialism))

  (setq completion-styles '(orderless))

  :config

  ;; SEE https://github.com/oantolin/orderless#company
  (defun ad/just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))
  (advice-add 'company-capf--candidates :around #'ad/just-one-face)

  ;; SEE https://github.com/cute-jumper/pinyinlib.el#pinyinlib-build-regexp-string
  (defun ad/orderless-regexp-pinyin (args)
    "Patch `orderless-regexp' with pinyin surpport"
    (setf (car args) (pinyinlib-build-regexp-string (car args)))
    args)
  (advice-add 'orderless-regexp :filter-args #'ad/orderless-regexp-pinyin)

  ;; SEE https://github.com/minad/consult/wiki#orderless-style-dispatchers-ensure-that-the--regexp-works-with-consult-buffer
  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun my/orderless-dispatch (pattern index _total)
    (cond
     ;; Treat first component as prefix. This is useful for Corfu completion-in-region.
     ((and completion-in-region-mode (= index 0))
      `(orderless-regexp . ,(concat "^" (regexp-quote pattern))))
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

  (setq orderless-component-separator #'orderless-escapable-split-on-space
        orderless-style-dispatchers '(my/orderless-dispatch))
  )

;; TODO consult-browser-bookmark consult-browser-history
(use-package consult
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

  ;; Dired-recentf-source
  (defvar consult--source-recentf-dired
    `(:name     "Dired Recentf"
      :narrow   ?d
      :hidden   t
      :category file
      :face     consult-file
      :state    ,#'consult--file-state
      :enabled  ,(lambda () recentf-mode)
      :items
      ,(lambda ()
         (let* ((ht (consult--string-hash (consult--buffer-query
                                           :mode 'dired-mode
                                           :as (lambda (buffer)
                                                 (with-current-buffer buffer
                                                   dired-directory))))))
           (seq-remove
            (lambda (x) (gethash x ht))
            (delete-dups (mapcar (lambda (file)
                                   (abbreviate-file-name (file-name-directory file)))
                                 recentf-list))))))
    "Dired recentf candidate source for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-recentf-dired 'append)

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

  ;; Integrate with `use-package'
  (setq consult-imenu-config
        '((emacs-lisp-mode
           :toplevel "Functions"
           :types
           ((?f "Functions" font-lock-function-name-face)
            (?m "Macros"    font-lock-function-name-face)
            (?p "Packages"  font-lock-constant-face)
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

(use-package embark
  ;; :init (setq embark-prompter 'embark-completing-read-prompter)
  :config
  ;; HACK Open source code of `symbol' in other window
  (advice-add 'embark-find-definition :before #'open-in-other-window))

(use-package embark-consult
  :after embark consult)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here

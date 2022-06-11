;;; init-minibuffer.el --- minibuffer sets -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; SEE https://github.com/purcell/emacs.d/blob/master/lisp/init-minibuffer.el

(leaf vertico
  :hook (after-init-hook . vertico-mode)
  :init
  (setq vertico-cycle t
        vertico-count 15
        resize-mini-windows t)
  :bind
  ((kbd "C-c C-r") . vertico-repeat)
  (:vertico-map
   ((kbd "RET") . vertico-directory-enter)
   ((kbd "DEL") . vertico-directory-delete-char))

  :defer-config

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
                (propertize "> " 'face 'font-lock-warning-face)
              "  ")
            cand))
  (advice-add #'vertico--format-candidate :around #'ad/vertico-customize-candidate)

  )

(leaf marginalia
  :hook (vertico-mode-hook . marginalia-mode)
  :defer-config
  (setq marginalia-align 'right
        marginalia-align-offset -1))

(leaf orderless
  :after vertico
  :require t
  :defer-config

  ;; SEE https://github.com/minad/consult/wiki#minads-orderless-configuration
  (setq completion-styles '(orderless basic)
        orderless-component-separator #'orderless-escapable-split-on-space)

  ;; Apply orderless style with initialism in category
  (orderless-define-completion-style my/orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-category-defaults nil
        completion-category-overrides
        '((file     (styles partial-completion))
          (command  (styles my/orderless-with-initialism))
          (variable (styles my/orderless-with-initialism))
          (symbol   (styles my/orderless-with-initialism))))

  ;; define dispatch
  (defvar my/orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?` . orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun my/orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) my/orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) my/orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))
  (setq orderless-style-dispatchers '(my/orderless-dispatch))

  ;; SEE https://github.com/minad/consult/wiki#use-orderless-as-pattern-compiler-for-consult-grepripgrepfind
  (with-eval-after-load 'consult
    (defun consult--orderless-regexp-compiler (input type &rest _config)
      (setq input (orderless-pattern-compiler input))
      (cons
       (mapcar (lambda (r) (consult--convert-regexp r type)) input)
       (lambda (str) (orderless--highlight input str))))
    (setq consult--regexp-compiler #'consult--orderless-regexp-compiler))

  ;; SEE https://github.com/cute-jumper/pinyinlib.el#pinyinlib-build-regexp-string
  (with-eval-after-load 'pinyinlib
    (defun ad/orderless-regexp-pinyin (args)
      "Patch `orderless-regexp' with pinyin surpport"
      (setf (car args) (pinyinlib-build-regexp-string (car args)))
      args)
    (advice-add 'orderless-regexp :filter-args #'ad/orderless-regexp-pinyin))

  )

;; TODO consult-browser-bookmark consult-browser-history
(leaf consult
  :after vertico
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)

  :init
  (setq consult-async-min-input 1)
  (setq consult-async-split-style 'semicolon)
  (setq consult-line-start-from-top t)

  (with-eval-after-load 'projectile
    (setq consult-project-function #'projectile-project-root))

  :defer-config

  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<")
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; -------------------------- Source ------------------------------
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
                          (let ((mode (buffer-local-value 'major-mode buffer))
                                (name (buffer-name buffer))
                                (path (abbreviate-file-name (or (buffer-file-name buffer) ""))))
                            (not (or (eq mode 'dired-mode)
                                     (string-match "\\`.+~.+~\\'" name))))))))

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

  ;; Blob-source
  (defvar consult--source-blob
    `(:name     "Blob"
      :narrow   ?g
      :hidden   t
      :category buffer
      :face     transient-pink
      :state    ,#'consult--buffer-state
      :items
      ,(lambda ()
         (consult--buffer-query
          :sort 'visibility
          :as #'buffer-name
          :predicate
          (lambda (buffer)
            (string-match "\\`.+~.+~\\'" (buffer-name buffer))))))
    "Blob buffer candidate source for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-blob)

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

  ;; ------------------------- Preview ------------------------------
  (setq consult-preview-allowed-hooks '(global-font-lock-mode-check-buffers))

  ;; disable preview
  (consult-customize
   consult-recent-file consult-bookmark consult--source-recent-file
   consult--source-project-recent-file consult--source-bookmark
   consult-ripgrep consult-git-grep consult-grep
   :preview-key nil)

  ;; -------------------------- Extra -------------------------------

  (unless emacs/>=28.1p
    (require 'consult-xref)
    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  (require 'consult-register)
  ;; Optionally configure the register formatting.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Integrate with `leaf'
  (require 'consult-imenu)
  (setq consult-imenu-config
        '((emacs-lisp-mode
           :types
           ((?f "Functions" font-lock-function-name-face)
            (?m "Macros"    font-lock-function-name-face)
            (?l "Leaf"      font-lock-constant-face)
            (?t "Types"     font-lock-type-face)
            (?v "Variables" font-lock-variable-name-face)))))

  ;; (require 'consult-compile)

  ;; ------------------------- Function -----------------------------

  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  )

(leaf consult-dir
  :defer-config
  (when (eq consult-project-function #'projectile-project-root)
    (setq consult-dir-project-list-function #'consult-dir-projectile-dirs))

  ;; HACK zlua directory jump
  (defun consult-dir--zlua-dirs ()
    "Return list of zlua dirs."
    (nreverse (mapcar
               (lambda (p) (abbreviate-file-name (file-name-as-directory p)))
               ;; REQUIRE export `ZLUA_SCRIPT' in parent-shell
               (split-string (shell-command-to-string
                              "lua $ZLUA_SCRIPT -l | perl -lane 'print $F[1]'")
                             "\n" t))))

  (defvar consult-dir--source-zlua
    `(:name     "Zlua"
      :narrow   ?z
      :category file
      :face     consult-file
      :history  file-name-history
      :enabled  ,(lambda () (getenv "ZLUA_SCRIPT"))
      :items    ,#'consult-dir--zlua-dirs)
    "Zlua directory source for `consult-dir'.")
  (add-to-list 'consult-dir-sources 'consult-dir--source-zlua t)
  )

(leaf embark
  :after vertico
  ;; :init (setq embark-prompter 'embark-completing-read-prompter)
  :bind
  (:embark-general-map
   ((kbd "C-c C-a") . marginalia-cycle))
  (:vertico-map
   ((kbd "C-l") . embark-act)
   ((kbd "C-c C-o") . embark-export)
   ((kbd "C-c C-a") . marginalia-cycle))
  :defer-config
  ;; HACK Open source code of `symbol' in other window
  (advice-add 'embark-find-definition :before #'open-in-other-window))

(leaf embark-consult
  :after embark consult)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here

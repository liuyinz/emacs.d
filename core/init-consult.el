;;; init-consult.el --- Consult setup -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-06-12 20:29:21

;;; Commentary:

;;; Code:

(leaf consult
  :after vertico
  :bind
  ("M-y" . consult-yank-pop)
  ;;mode-specific-map
  ("C-c M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c m" . consult-man)
  ("C-c i" . consult-info)
  ("C-c y" . consult-yasnippet)
  ("C-c C-d" . consult-dir)
  ("C-c C-p" . consult-project-extra-find)
  ;;ctl-x-map
  ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ("C-x b"   . consult-buffer)                ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
  ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
  ;;goto-map
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
  ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ("M-g t" . consult-todo)
  ;;search-map
  ("M-s d" . consult-find)                  ;; Alternative: consult-fd
  ("M-s c" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s e" . consult-isearch-history)
  ("M-s u" . consult-focus-lines)
  ;;isearch-mode-map
  ("M-e"   . consult-isearch-history)         ;; orig. isearch-edit-string
  ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
  ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
  ("M-s L" . consult-line-multi)           ;; needed by consult-line to detect isearch
  ;;minibuffer-local-map
  ("M-s" . consult-history)                 ;; orig. next-matching-history-element
  ("M-r" . consult-history)

  :init
  (setq consult-async-min-input 1)
  (setq consult-async-split-style 'semicolon)
  (setq consult-line-start-from-top t)

  :defer-config
  (setq-default completion-in-region-function #'consult-completion-in-region)

  ;; SEE https://github.com/minad/consult/wiki#use-orderless-as-pattern-compiler-for-consult-grepripgrepfind
  (with-eval-after-load 'orderless
    (defun consult--orderless-regexp-compiler (input type &rest _config)
      (setq input (orderless-pattern-compiler input))
      (cons
       (mapcar (lambda (r) (consult--convert-regexp r type)) input)
       (lambda (str) (orderless--highlight input str))))
    (setq consult--regexp-compiler #'consult--orderless-regexp-compiler))

  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<")

  ;; -------------------------- Source ------------------------------

  ;; NOTE
  ;; 1. hidden: add regexp in `consult-buffer-filter' or filter with :predicate
  ;;    in `consult--source-buffer'
  ;; 2. extract: set :filter nil and :predicate in consult--source-*

  (appendq! consult-buffer-filter '("\\`\\*.*\\*\\'"
                                    "\\`.*\\.el\\.gz\\'"
                                    "\\`magit[:-].*\\'"
                                    "\\`COMMIT_EDITMSG\\'"
                                    "\\`.+~.+~\\'"))

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
                          (let ((mode (buffer-local-value 'major-mode buffer)))
                            (not (eq mode 'dired-mode)))))))

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
         (consult--buffer-query
          :mode 'dired-mode
          :filter nil
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
          :filter nil
          :predicate
          (lambda (buffer)
            (string-match-p "\\`.+~.+~\\'" (buffer-name buffer))))))
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
         (consult--buffer-query
          :mode 'org-mode
          :filter nil
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
           :toplevel "Functions"
           :types ((?f "Functions"   font-lock-function-name-face)
                   (?y "Types"       font-lock-type-face)
                   (?v "Variables"   font-lock-variable-name-face)
                   (?c "Commands"    font-lock-constant-face)
                   (?u "Customs"     font-lock-string-face)
                   (?a "Faces"       font-lock-type-face)
                   (?l "Leafs"       font-lock-keyword-face)
                   (?m "Macros"      font-lock-function-name-face)
                   (?k "Keys"        font-lock-variable-name-face)
                   (?t "Transients"  font-lock-type-face)))
          (js-mode
           :types ((?c "Classes"    font-lock-type-face)
                   (?f "Functions"  font-lock-function-name-face)
                   (?s "Constants"  font-lock-constant-face)
                   (?m "Methods"    font-lock-string-face)
                   (?p "Properties" font-lock-builtin-face)
                   (?v "Variables"  font-lock-variable-name-face)
                   (?e "Fields"     font-lock-warning-face)))
          (js-ts-mode
           :types ((?c "Class"      font-lock-type-face)
                   (?f "Function"   font-lock-function-name-face)
                   ;; (?s "Constants"  font-lock-constant-face)
                   ;; (?m "Methods"    font-lock-string-face)
                   ;; (?p "Properties" font-lock-builtin-face)
                   ;; (?v "Variables"  font-lock-variable-name-face)
                   ;; (?e "Fields"     font-lock-warning-face)
                   ))
          (python-mode
           :types ((?c "Classes"    font-lock-type-face)
                   (?f "Functions"  font-lock-function-name-face)
                   (?v "Variables"  font-lock-variable-name-face)))
          (sh-mode
           :types ((?f "Functions"  font-lock-function-name-face)
                   (?v "Variables"  font-lock-variable-name-face)))))

  (leaf consult-dir
    :defer-config
    (setq consult-dir-default-command
          (if (or (fboundp #'consult-project-extra-find)
                  (require 'consult-project-extra nil t))
              #'consult-project-extra-find
            #'project-find-file))

    ;; zlua directory jump
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
    (add-to-list 'consult-dir-sources 'consult-dir--source-zlua t))

  )

(provide 'init-consult)
;;; init-consult.el ends here

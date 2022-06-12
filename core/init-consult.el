;;; init-consult.el --- Consult setup -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-06-12 20:29:21

;;; Commentary:

;;; Code:

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

  :defer-config

  (with-eval-after-load 'projectile
    (setq consult-project-function #'projectile-project-root))

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
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

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
    (setq consult-dir-project-list-function #'consult-dir-projectile-dirs)
    (if (or (fboundp #'consult-projectile-find-file)
            (require 'consult-projectile nil t))
        (setq consult-dir-default-command #'consult-projectile-find-file)))

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

(provide 'init-consult)
;;; init-consult.el ends here
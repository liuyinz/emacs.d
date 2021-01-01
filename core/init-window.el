(use-package ace-window
  :hook (after-init . ace-window-display-mode)
  :bind (("M-o" . ace-window))
  :init
  (setq aw-keys '(?a ?s ?d ?f ?q ?w ?r ?t)
        aw-scope 'frame
        aw-background nil
        aw-dispatch-always nil
        aw-minibuffer-flag t
        aw-dispatch-alist '((?x aw-delete-window "Delete Window")
	                        (?v aw-split-window-vert "Split Vert Window")
	                        (?h aw-split-window-horz "Split Horz Window")
	                        (?o delete-other-windows "Delete Other Windows")
	                        (?m aw-swap-window "Swap Windows")
	                        (?M aw-move-window "Move Window")
	                        (?c aw-copy-window "Copy Window")
	                        (?j aw-switch-buffer-in-window "Select Buffer")
	                        (?n aw-flip-window)
	                        (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	                        (?? aw-show-dispatch-help))))

;; Enforce rules for popups
(defvar shackle--popup-window-list nil) ; all popup windows
(defvar-local shackle--current-popup-window nil) ; current popup window
(put 'shackle--current-popup-window 'permanent-local t)

(use-package shackle
  :disabled
  :hook (after-init . shackle-mode)
  :config
  (setq shackle-default-size 0.4
        shackle-default-rule nil
        shackle-default-alignment 'below
        shackle-inhibit-window-quit-on-same-windows t
        shackle-rules
        '((("*Help*" "*Apropos*") :select t :align 'below)
          ;; ("*package update results*" :size 0.5 :align 'right)
          ("*quickrun*" :other t)
          (" *undo-tree*" :select t :same t)
          ("*Flycheck checkers*" :select t)
          ("*nodejs*" :other t :select t)
          ((flycheck-error-list-mode
            flymake-diagnostics-buffer-mode)
           :select t :popup t :align 'below)
          ("*frequencies*" :other t :align 'right :size 0.5 :select t)
          (compilation-mode :select t :size 0.3 :align 'below)
          ;; ("*Completions*" :size 0.3 :align 'below)
          ("*Pp Eval Output*" :size 15 :align 'below)
          ("*Backtrace*" :select t :size 15 :align 'below)
          (("*Warnings*" "*Messages*") :size 0.3 :align 'below)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 :align 'below)
          ("^\\*vc-.*\\*$" :regexp t :size 0.3 :align 'below)
          ("*gud-debug*" :select t :size 0.4 :align 'below)
          ("\\*ivy-occur .*\\*" :regexp t :select t :size 0.3 :align 'below)
          ("*tldr*" :size 0.4 :align 'below)
          ("*Youdao Dictionary*" :size 15 :align 'below)
          ("*Finder*" :select t :size 0.3 :align 'below)
          ("^\\*macro expansion\\**" :regexp t :size 0.4 :align 'below)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below)
          ((" *Org todo*"
            "*Org Dashboard*"
            "*Org Select*") :select t :size 0.4 :align 'below)
          (" *Install vterm" :size 0.35 :same t :align 'below)
          (("*Paradox Report*" "*package update results*") :size 0.2 :align 'below)
          ("*Package-Lint*" :size 0.4 :align 'below)
          (("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :align 'below)
          ("*How Do You*" :select t :size 0.5 :align 'below)
          ("*ert*" :size 15 :align 'below)
          (overseer-buffer-mode :size 15 :align 'below)

          (("*lsp-help*" "*lsp session*") :size 0.3 :align 'below)
          ("*DAP Templates*" :select t :size 0.4 :align 'below)
          (dap-server-log-mode :size 15 :align 'below)

          (profiler-report-mode :select t :size 0.5 :align 'below)
          ("*ELP Profiling Restuls*" :select t :size 0.5 :align 'below)

          ((inferior-python-mode
            inf-ruby-mode
            swift-repl-mode) :size 0.4 :align 'below)
          ("*prolog*" :size 0.4 :align 'below)

          ((grep-mode
            rg-mode
            deadgrep-mode
            ag-mode
            pt-mode) :select t :size 0.4 :align 'below)
          (Buffer-menu-mode :select t :size 20 :align 'below)
          (gnus-article-mode :select t :size 0.7 :align 'below)
          (helpful-mode :select t :size 0.3 :align 'below)
          ((process-menu-mode
            cargo-process-mode) :select t :size 0.3 :align 'below)
          (list-environment-mode :select t :size 0.3 :align 'below)
          (tabulated-list-mode :size 0.4 :align 'below))))

;; :config
;; ;; Add keyword: `autoclose'
;; (defun shackle-display-buffer-hack (fn buffer alist plist)
;;   (let ((window (funcall fn buffer alist plist)))
;;     (setq shackle--current-popup-window window)

;;     (when (plist-get plist :autoclose)
;;       (push (cons window buffer) shackle--popup-window-list))
;;     window))
;; (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)
;; :init
;; (setq shackle-default-size 0.4
;;       shackle-default-alignment 'below
;;       shackle-default-rule nil
;;       shackle-rules
;;       '((("*Help*" "*Apropos*") :select t :size 0.3 :align 'below :autoclose t)
;;         (compilation-mode :select t :size 0.3 :align 'below :autoclose t)
;;         ("*Completions*" :size 0.3 :align 'below :autoclose t)
;;         ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
;;         ("*Backtrace*" :select t :size 15 :align 'below)
;;         (("*Warnings*" "*Messages*") :size 0.3 :align 'below :autoclose t)
;;         ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
;;         ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
;;         ("*Calendar*" :select t :size 0.3 :align 'below)
;;         (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 :align 'below)
;;         ("^\\*vc-.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
;;         ("*gud-debug*" :select t :size 0.4 :align 'below :autoclose t)
;;         ("\\*ivy-occur .*\\*" :regexp t :select t :size 0.3 :align 'below)
;;         (" *undo-tree*" :select t :autoclose t)
;;         ("*quickrun*" :size 15 :align 'below)
;;         ("*tldr*" :size 0.4 :align 'below :autoclose t)
;;         ("*package update results*" :size 0.4 :align 'below :autoclose t)
;;         ("*Youdao Dictionary*" :size 15 :align 'below :autoclose t)
;;         ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
;;         ("^\\*macro expansion\\**" :regexp t :size 0.4 :align 'below)
;;         ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)
;;         ((" *Org todo*" "*Org Dashboard*" "*Org Select*") :select t :size 0.4 :align 'below :autoclose t)
;;         (" *Install vterm" :size 0.35 :same t :align 'below)
;;         (("*Paradox Report*" "*package update results*") :size 0.2 :align 'below :autoclose t)
;;         ("*Package-Lint*" :size 0.4 :align 'below :autoclose t)
;;         (("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :align 'below :autoclose t)
;;         ("*How Do You*" :select t :size 0.5 :align 'below :autoclose t)
;;         ("*ert*" :size 15 :align 'below :autoclose t)
;;         (overseer-buffer-mode :size 15 :align 'below :autoclose t)
;;         (" *Flycheck checkers*" :select t :size 0.3 :align 'below :autoclose t)
;;         ((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
;;          :size 0.25 :align 'below :autoclose t)

;;         (("*lsp-help*" "*lsp session*") :size 0.3 :align 'below :autoclose t)
;;         ("*DAP Templates*" :select t :size 0.4 :align 'below :autoclose t)
;;         (dap-server-log-mode :size 15 :align 'below :autoclose t)

;;         (profiler-report-mode :select t :size 0.5 :align 'below)
;;         ("*ELP Profiling Restuls*" :select t :size 0.5 :align 'below)

;;         ((inferior-python-mode inf-ruby-mode swift-repl-mode) :size 0.4 :align 'below)
;;         ("*prolog*" :size 0.4 :align 'below)

;;         ((grep-mode rg-mode deadgrep-mode ag-mode pt-mode) :select t :size 0.4 :align 'below)
;;         (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
;;         (gnus-article-mode :select t :size 0.7 :align 'below :autoclose t)
;;         (helpful-mode :select t :size 0.3 :align 'below :autoclose t)
;;         ((process-menu-mode cargo-process-mode) :select t :size 0.3 :align 'below :autoclose t)
;;         (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
;;         (tabulated-list-mode :size 0.4 :align 'below))))

(provide 'init-window)

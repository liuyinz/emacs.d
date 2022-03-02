;;; init-window.el --- window setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; SEE https://github.com/cyrus-and/zoom
(leaf zoom
  :init (setq zoom-size '(0.618 . 0.618)))

(leaf popper
  :hook (after-init-hook . popper-mode)
  :bind
  (:popper-mode-map
   ("C-h z" . popper-toggle-latest)
   ("C-<tab>"   . popper-cycle)
   ("C-M-<tab>" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          ivy-occur-mode ivy-occur-grep-mode
          process-menu-mode list-environment-mode cargo-process-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*eshell.*\\*$" eshell-mode
          "^\\*shell.*\\*$"  shell-mode
          "^\\*term.*\\*$"   term-mode
          "^\\*vterm.*\\*$"  vterm-mode

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-test-moed))

  (setq popper-echo-dispatch-actions t)

  (with-eval-after-load 'projectile
    (setq popper-group-function #'popper-group-by-projectile))
  
  :defer-config
  (popper-echo-mode 1)
  
  (with-no-warnings
    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
          (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack))
  )

;; (leaf shackle
;;   :hook (after-init-hook . shackle-mode)
;;   :init
;;   (setq shackle-default-rule nil)
;;   (setq shackle-rules
;;         ;; SEE https://github.com/seagle0128/.emacs.d/blob/320ae719a1acb84c047e94bb6ee3f33e426f7b47/lisp/init-window.el#L204
;;         '(
;;           ;; builtin
;;           (("*Warnings*" "*Messages*") :size 0.3 :align 'below)
;;           (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 :align 'below)
;;           ("\\*[Wo]*Man.*\\*" :regexp t :popup t :select t :size 0.5 :align 'below)

;;           ("*vterm*" :align 'below :size 0.4)
;;           ;; ("*quickrun*" :select t :size 0.4 :align 'below)
;;           ("*Python*" :select t :size 0.4 :align 'below)
;;           ("*nodejs*" :select t :size 0.4 :align 'below)

;;           ("*format-all-errors*" :size 0.3 :align 'below)

;;           (" *Flycheck checkers*" :select t :size 0.3 :align 'below)
;;           ((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
;;            :select t :size 0.25 :align 'below)

;;           ("*rg*" :select t)
;;           ("\\`\\*edit-indirect .+\\*\\'" :regexp t :popup t :select t :size 0.4 :align 'below)

;;           ;; ("*Emacs Log*" :size 0.3 :align 'right)
;;           )))

(add-to-list 'display-buffer-alist
             '("^\\*quickrun"
               (display-buffer-reuse-window
	            display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.3)))

(add-to-list 'display-buffer-alist
             '("^\\*Emacs Log*"
               (display-buffer-reuse-window
	            display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . right)
               (window-width    . 0.25)))

(provide 'init-window)
;;; init-window.el ends here

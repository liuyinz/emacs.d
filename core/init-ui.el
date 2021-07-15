;;; init-ui.el --- ui settings -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(require 'subr-x)

(leaf transient
  :require t
  :init
  (setq transient-highlight-mismatched-keys nil
        transient-detect-key-conflicts t))

(leaf ns-auto-titlebar
  :hook (after-make-window-system-frame-hook . ns-auto-titlebar-mode))

(leaf doom-modeline
  :doc "deps: all-the-icons emacs-async"
  :hook (after-init-hook . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil
        doom-modeline-bar-width 0
        doom-modeline-height 15
        doom-modeline-persp-name nil
        doom-modeline-irc nil
        doom-modeline-project-detection 'projectile
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-checker-simple-format nil
        doom-modeline-indent-info nil
        doom-modeline-env-load-string "..."
        doom-modeline-vcs-max-length 20
        doom-modeline-window-width-limit (+ fill-column 20)
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-env-python-executable "/usr/local/bin/python3"))

(leaf doom-themes
  :hook (after-init-hook . (lambda ()
                             (require 'doom-themes)
                             (load-theme 'doom-vibrant t)))
  :init
  (add-hook 'after-load-theme-hook #'my/doom-theme-adjust)
  (defun my/doom-theme-adjust ()
    (custom-set-faces
     ;; vertico
     '(vertico-current ((t (:foreground "white"
                            :background "#484e5f"
                            :extend t))))
     ;; orderless
     '(orderless-match-face-0 ((t (:inherit font-lock-string-face
                                   :weight bold
                                   :inverse-video t))))
     '(orderless-match-face-1 ((t (:inherit error
                                   :weight bold
                                   :inverse-video t))))
     '(orderless-match-face-2 ((t (:inherit font-lock-type-face
                                   :weight bold
                                   :inverse-video t))))
     '(orderless-match-face-3 ((t (:inherit font-lock-keyword-face
                                   :weight bold
                                   :inverse-video t))))
     ;; evil
     '(cursor ((t (:inherit unspecified
                   :background "#fcce7b"
                   :foreground "#242730"))))
     '(isearch ((t (:inherit orderless-match-face-2))))
     '(lazy-highlight ((t (:inherit orderless-match-face-0
                           :foreground nil
                           :background nil))))
     '(evil-ex-substitute-matches ((t (:inherit orderless-match-face-1))))
     '(evil-ex-substitute-replacement ((t (:inherit orderless-match-face-0))))
     ;; completions
     '(completions-common-part ((t (:inherit orderless-match-face-0))))
     ;; company
     '(company-tooltip-mouse ((t (:inherit company-tooltip :background nil))))
     '(company-tooltip-selection ((t (:inherit vertico-current :background nil))))
     '(company-tooltip-search ((t (:inherit font-lock-string-face
                                   :background nil
                                   :foreground nil))))
     ;;consult
     '(consult-file ((t (:inherit font-lock-doc-face))))
     ;;marginalia
     '(marginalia-type ((t (:inherit font-lock-constant-face))))
     '(marginalia-key ((t (:inherit font-lock-keyword-face :weight bold))))
     '(marginalia-modified ((t (:inherit font-lock-string-face))))
     '(marginalia-date ((t (:inherit font-lock-keyword-face))))
     ;;diff-hl
     '(diff-hl-change ((t (:background nil))))
     '(diff-hl-insert ((t (:background nil))))
     '(diff-hl-delete ((t (:background nil))))
     ;; yasnippet
     '(yas-field-highlight-face ((t (:inherit font-lock-string-face
                                     :weight bold
                                     :background "#484e5f"))))
     ;; markdown-mode
     '(markdown-code-face ((t (:background "#323642" :extend t))))
     ;; avy
     '(avy-lead-face ((t (:foreground "#51afef" :background nil))))
     '(avy-lead-face-0 ((t (:foreground "#99d0f6" :background nil))))
     '(avy-lead-face-1 ((t (:foreground "#cfe9fb" :background nil))))
     '(avy-lead-face-2 ((t (:foreground "#f3f9fe" :background nil))))
     ;; highlight-defined
     '(highlight-defined-function-name-face
       ((t (:inherit unspecified))))
     '(highlight-defined-variable-name-face
       ((t (:inherit unspecified))))
     ;; window
     '(vertical-border ((t (:background nil))))
     ;; rg.el
     )))

(provide 'init-ui)
;;; init-ui.el ends here

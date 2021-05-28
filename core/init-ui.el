;;; init-ui.el --- ui settings -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(require 'subr-x)

(leaf doom-modeline
  :doc "deps: all-the-icons emacs-async"
  :hook (after-init-hook . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil
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

;; doom-theme
(leaf doom-themes
  :require t
  :config
  (load-theme 'doom-vibrant t)
  ;; modify faces
  (custom-set-faces
   '(cursor ((t (:inherit unspecified :background "#fcce7b" :foreground "#242730"))))
   '(isearch ((t (:foreground "#242730" :background "#fcce7b" :weight bold))))
   '(lazy-highlight ((t (:foreground "#242730" :background "#7bc257" :weight bold))))
   ;; selectrum
   '(selectrum-current-candidate ((t (:foreground "white"))))
   ;; vertico
   '(vertico-current ((t (:foreground "white" :background "#323642" :extend t))))
   ;; orderless
   '(orderless-match-face-0 ((t (:inherit font-lock-string-face :weight bold :inverse-video t))))
   '(orderless-match-face-1 ((t (:inherit error :weight bold :inverse-video t))))
   '(orderless-match-face-2 ((t (:inherit font-lock-type-face :weight bold :inverse-video t))))
   '(orderless-match-face-3 ((t (:inherit font-lock-keyword-face :weight bold :inverse-video t))))
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
   ;; markdown-mode
   '(markdown-code-face ((t (:background "#323642" :extend t))))
   ;; indent-guide
   '(indent-guide-face ((t (:inherit font-lock-comment-face))))
   ;; evil
   '(evil-ex-substitute-matches ((t (:inherit error :weight bold :inverse-video t))))
   '(evil-ex-substitute-replacement ((t (:inherit font-lock-string-face :weight bold :inverse-video t))))
   ;; avy
   '(avy-lead-face ((t (:foreground "#51afef" :background nil))))
   '(avy-lead-face-0 ((t (:foreground "#99d0f6" :background nil))))
   '(avy-lead-face-1 ((t (:foreground "#cfe9fb" :background nil))))
   '(avy-lead-face-2 ((t (:foreground "#f3f9fe" :background nil))))
   ;; rg.el
   ))

(provide 'init-ui)

;;; init-ui.el ends here

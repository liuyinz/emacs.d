;;; init-edit.el --- edit setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)

(leaf wgrep)
;; Hungry deletion
(leaf hungry-delete
  :blackout t
  :hook (after-init-hook . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Jump to things in Emacs tree-style
(leaf avy
  :hook (after-init-hook . avy-setup-default)
  :config
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        avy-style 'at-full
        avy-keys '(?a ?s ?d ?f ?h ?j ?k ?l ?q ?u ?w ?i ?e ?o))
  (setq avy-orders-alist
        '((avy-goto-char . avy-order-closest)
          (avy-goto-word-0 . avy-order-closest))))

;; undo-redo
(leaf undo-fu :require t)
(leaf undo-fu-session
  :after undo-fu
  :hook (after-init-hook . global-undo-fu-session-mode))

;; Flexible text folding
(leaf origami
  :hook (after-init-hook . global-origami-mode)
  :init (setq origami-show-fold-header t)
  :config (face-spec-reset-face 'origami-fold-header-face))

(leaf command-log-mode
  :blackout
  :commands (global-command-log-mode clm/toggle-command-log-buffer)
  :init
  (setq command-log-mode-open-log-turns-on-mode t
        command-log-mode-is-global t
        command-log-mode-window-size 40))

(leaf color-rg
  :commands (color-rg-search-project-with-type
             color-rg-search-input
             color-rg-search-input-in-project
             color-rg-search-input-in-current-file)
  :custom-face
  (color-rg-font-lock-header-line-text . '((t (:foreground "#8f60a2" :bold t))))
  (color-rg-font-lock-header-line-keyword . '((t (:foreground "#98c379" :bold t))))
  (color-rg-font-lock-header-line-edit-mode . '((t (:foreground "#56b6c2" :bold t))))
  (color-rg-font-lock-header-line-directory . '((t (:foreground "#61afef" :bold t :underline t))))
  (color-rg-font-lock-file . '((t (:foreground "#61afef" :bold t :underline t))))
  (color-rg-font-lock-match . '((t (:foreground "#98c379" :bold t))))
  (color-rg-font-lock-command . '((t (:foreground "#8f60a2" :bold t))))
  :init
  (setq color-rg-mac-load-path-from-shell nil
        color-rg-max-column 6000))

(leaf awesome-pair
  :hook ((c-mode-common-hook
          c-mode-hook
          c++-mode-hook
          java-mode-hook
          haskell-mode-hook
          emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          lisp-mode-hook
          maxima-mode-hook
          ielm-mode-hook
          sh-mode-hook
          makefile-gmake-mode-hook
          php-mode-hook
          python-mode-hook
          js-mode-hook
          go-mode-hook
          qml-mode-hook
          jade-mode-hook
          css-mode-hook
          ruby-mode-hook
          coffee-mode-hook
          rust-mode-hook
          qmake-mode-hook
          lua-mode-hook
          swift-mode-hook
          minibuffer-inactive-mode-hook) . awesome-pair-mode))

(provide 'init-edit)
;;; init-edit.el ends here

;;; init-ui.el --- ui settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; modeline
(use-package doom-modeline
  :straight t
  :delight t
  :hook (after-init-hook . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil
        doom-modeline-persp-name nil
        doom-modeline-irc nil
        doom-modeline-project-detection 'ffip
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
(use-package doom-themes
  :straight t
  :demand
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package which-key
  :disabled
  :straight t
  :delight t
  :hook (after-init-hook . which-key-mode)
  :init
  (setq which-key-dont-use-unicode t
        which-key-compute-remaps t
        which-key-show-remaining-keys t
        which-key-use-C-h-commands nil
        max-mini-window-height 0.3
        which-key-max-description-length 30
        which-key-add-column-padding 3
        which-key-popup-type 'minibuffer
        which-key-show-prefix 'left
        )
  (setq which-key-paging-prefixes '("C-x"))
  (setq which-key-paging-key "<f5>")
  :config
  (dolist (p '(("C-x a" . "abbrev")
               ("C-x t" . "tab")
               ("C-x n" . "narrow")
               ("C-x p" . "project")
               ("C-x 8" . "unicode")
               ("C-x @" . "modifior")
               ("C-x X" . "edebug")
               ("C-c !" . "flycheck")
               ("<leader> c" . "evil-nerd-commenter")
               ("<leader> s" . "color-rg")))
    (which-key-add-key-based-replacements (car p) (cdr p))))

(provide 'init-ui)

;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;; (set-fontset-font (frame-parameter nil 'font)
;; charset (font-spec :family "Source Han Serif"))
;; (setq face-font-rescale-alist '(("Source Han Serif" . 0.95))))
;;; init-ui.el ends here

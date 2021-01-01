;;; package --- init-ui.el

;; modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil
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

;;doom-theme
(use-package doom-themes
  :demand
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package centaur-tabs
  :disabled
  :hook ((after-init . centaur-tabs-mode)
         ((dired-mode vterm-mode diff-mode) . centaur-tabs-local-mode))
  :bind (:map centaur-tabs-mode-map
         ("M-j" . centaur-tabs-forward-tab)
         ("M-k" . centaur-tabs-backward-tab)
         ("M-u" . centaur-tabs-forward-group)
         ("M-l" . centaur-tabs-counsel-switch-group)
         ("<s-return>" . toggle-frame-fullscreen))
  :init
  (setq centaur-tabs-set-icons nil
        centaur-tabs-set-close-button nil
        centaur-tabs-set-modified-marker nil
        centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "Sarasa Mono SC" 160)

  (defun centaur-tabs-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p " *transient*" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*package update results*" name)
       (string-prefix-p "*lsp" name)
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name))))))

  (defun centaur-tabs-buffer-groups ()
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (derived-mode-p 'dired-mode)
           (derived-mode-p 'eshell-mode)
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-revision-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode))
           (memq major-mode '(vterm-mode)))
       "Default")
      ((memq major-mode '(org-mode org-agenda-mode diary-mode))
       "OrgMode")
      (t (centaur-tabs-get-group-name (current-buffer))))))

  ;; (with-eval-after-load 'projectile
  ;; (centaur-tabs-group-by-projectile-project))

  (set-face-attribute 'centaur-tabs-selected nil
                      :background "#1D252C"
                      :foreground "gray"
                      :weight 'bold)
  (set-face-attribute 'centaur-tabs-selected-modified nil
                      :background "#1D252C"
                      :foreground "#e27e8d"
                      :weight 'bold)
  (set-face-attribute 'centaur-tabs-unselected-modified nil
                      :background "#181E24"
                      :foreground "#e27e8d"))

(provide 'init-ui)

;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;; (set-fontset-font (frame-parameter nil 'font)
;; charset (font-spec :family "Source Han Serif"))
;; (setq face-font-rescale-alist '(("Source Han Serif" . 0.95))))

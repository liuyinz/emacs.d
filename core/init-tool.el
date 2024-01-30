;;; init-tool.el --- other tools -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;; SEE https://github.com/alphapapa/emacs-package-dev-handbook

;;; Code:

;; --------------------------- Debug -------------------------------

(leaf profiler
  :init
  (setq profiler-report-leaf-mark  ">")
  (defun ad/profiler-bytes-h (str)
    "reformat with human-readeable size"
    (let ((s (cl-count ?, str)))
      (cond
       ((= s 1) (concat (substring str 0 -4) " K"))
       ((= s 2) (concat (substring str 0 -8) " M"))
       ((>= s 3) (concat (substring str 0 -12) " G"))
       (t str))))
  (advice-add 'profiler-format-number :filter-return #'ad/profiler-bytes-h))

(leaf etrace)

;; ------------------------ Translate -----------------------------

;; SEE https://www.emacswiki.org/emacs/RegularExpression
(leaf xr)

;; REQUIRE brew install opencc
(leaf opencc)

(leaf pinyinlib :require t)

(leaf pandoc-mode)

;; ------------------------- Network ------------------------------

;; (leaf proxy-mode
;;   :init
;;   (when-let ((http (getenv "HTTP")))
;;     (setq proxy-mode-env-http-proxy (concat "http://" http))
;;     (setq proxy-mode-emacs-http-proxy
;;           `(("http"     . ,http)
;;             ("https"    . ,http)
;;             ("ftp"      . ,http)
;;             ("no_proxy" . "127.0.0.1"))))
;; 
;;   (when-let ((sock (getenv "SOCKS")))
;;     (setq proxy-mode-emacs-socks-proxy
;;           `("Default server"
;;             ,(substring sock 0 -5)
;;             ,(substring sock -4 nil)
;;             5)))
;;   )

;; ;; TODO set embark with proxy
;; (leaf with-proxy
;;   :require t
;;   :init
;;   (when-let ((http (getenv "HTTP")))
;;     (setq with-proxy-http-server http)))

;; -------------------------- record ------------------------------

(leaf keycast
  :init
  (setq keycast-mode-line-format "%k%c%r"))

(leaf keyfreq
  :hook ((after-init-hook . keyfreq-mode)
         (keyfreq-mode-hook . keyfreq-autosave-mode))
  :init
  (setq keyfreq-excluded-regexp
        '(;; built-in
          "\\`\\(mouse\\|scroll\\|keyboard\\|clipboard\\|minibuffer\\|package\\)-.*\\'"
          "\\`\\(backward\\|forward\\|move\\|isearch\\|describe\\|eval\\|exit\\)-.*\\'"
          "\\`\\(delete\\|kill\\|quit\\|save\\|abort\\|self\\|term\\|xterm\\)-.*\\'"
          "\\`\\(pcomplete\\|y-or-n-p\\|previous\\|next\\|right\\|left\\|zap\\)-.*\\'"
          "\\`\\(suspend\\|execute-extended\\|transient\\|indent\\electric\\)-.*\\'"
          "\\`\\(profiler-report\\|compilation\\|smerge\\)-.*\\'"
          ;; third-party
          "\\`\\(keyfreq\\|vertico\\|evil\\|corfu\\|vundo\\|yas\\|vterm\\)-.*\\'"
          "\\`\\(magit\\|helpful\\|web-mode\\|ilog\\|hungry-delete\\|meow\\)-.*\\'"
          "\\`\\(markdown-insert\\|markdown-table\\|my/transient\\|sis-set\\)-.*\\'"
          "\\`\\(acm\\|yaml-electric\\)-.*\\'"
          ))

  (setq keyfreq-excluded-commands
        '(
          undefined ignore mwheel-scroll hippie-expand newline newline-and-indent
          yank indent-for-tab-command push-button choose-completion
          comint-previous-input comint-send-input compile-goto-error embark-act dired
          dired-do-async-shell-command goto-line ispell-minor-check js-mode
          magit-mode-bury-buffer markdown-outdent-or-delete org-beginning-of-line
          org-ctrl-c-ctrl-c org-cycle org-delete-backward-char
          org-end-of-line org-force-self-insert org-return org-self-insert-command
          org-todo orgtbl-self-insert-command handle-switch-frame
          pixel-scroll-precision))
  :config

  ;; TODO support transient keybindings https://github.com/magit/transient/issues/113
  ;; HACK filter <menu-bar> bindings
  (defun ad/keyfreq-where-is (command)
    (string-join
     (cl-remove-if
      (lambda (key)
        (string-match-p "<\\(menu\\|vertical-scroll\\|horizontal-scroll\\)-bar>" key))
      (mapcar 'key-description (where-is-internal command)))
     ", "))
  (advice-add 'keyfreq-where-is :override #'ad/keyfreq-where-is)
  )

;; --------------------------- shell -------------------------------

;; TODO rewrite a shell-enhanced plugin
(leaf dwim-shell-command
  :commands dwim-shell-command-on-marked-files
  :init
  (defun my/open-with=vscode ()
    (interactive)
    (dwim-shell-command-on-marked-files
     "Open current file in vscode."
     (if (eq system-type 'darwin)
         (if (derived-mode-p 'prog-mode)
             (format "code --goto '<<f>>':%d:%d"
                     (current-line)
                     (current-column))
           "open '<<f>>'")
       "setsid -w xdg-open '<<f>>'")
     :shell-args '("-x" "-c")
     :silent-success t
     :utils (if (eq system-type 'darwin)
                "open"
              "xdg-open")))
  )

(provide 'init-tool)
;;; init-tool.el ends here

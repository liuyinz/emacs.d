;;; init-write.el --- Enjoy Writing!! -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(leaf writeroom-mode
  :commands writeroom-mode
  :hook (writeroom-mode-hook . toggle-cjk-writeroom)
  :init
  (setq writeroom-width 120)

  ;; toggle modes according to writeroom-mode
  (defvar writeroom-toggle-modes '((diff-hl-mode . nil)))
  (add-hook 'writeroom-mode-hook (lambda () (mode-hook-toggle
                                             writeroom-mode
                                             writeroom-toggle-modes)))

  (defun toggle-cjk-writeroom ()
    (interactive)
    (if (bound-and-true-p writeroom-mode)
        (cjk-font-setting "Source Han Serif" 1.4)
      (cjk-font-setting "Sarasa Mono SC" 1))))

;; TODO customize easy-hugo-buffer, refactor easy-hugo, tag filter
(leaf easy-hugo
  :commands easy-hugo easy-hugo-rg
  :init
  (setq easy-hugo-basedir  "~/Code/blog/"
        easy-hugo-postdir "content/posts/"
        easy-hugo-url  "https://liuyinz.github.io/"
        easy-hugo-preview-url "http://localhost:1313/"
        easy-hugo-server-flags "-D"
        easy-hugo-no-help t)
  :config
  (easy-hugo-enable-menu))

  (provide 'init-write)

;;; init-write.el ends here

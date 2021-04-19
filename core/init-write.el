;;; init-write.el --- Enjoy Writing!! -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(leaf writeroom-mode
  :commands writeroom-mode
  :hook (writeroom-mode-hook . toggle-cjk-writeroom)
  :init
  (setq writeroom-width 120)
  (defun toggle-cjk-writeroom ()
    (interactive)
    (if (bound-and-true-p writeroom-mode)
        (cjk-font-setting "Source Han Serif" 1.4)
      (cjk-font-setting "Sarasa Mono SC" 1))))

(leaf easy-hugo
  :commands easy-hugo
  :init
  (setq easy-hugo-basedir  "~/Code/blog/"
        easy-hugo-postdir "content/posts/"
        easy-hugo-url  "https://liuyinz.github.io/"
        easy-hugo-preview-url "http://localhost:1313/"
        easy-hugo-server-flags "-D")
  :config
  ;; HACK search with consult-ripgrep
  (defun easy-hugo-consult ()
    "Search for blog article with `consult-ripgrep'or `consult-grep'"
    (interactive)
    (easy-hugo-with-env
     (let ((dir (expand-file-name easy-hugo-postdir easy-hugo-basedir)))
       (if (featurep 'consult)
           (if (executable-find "rg")
               (consult-ripgrep dir nil)
             (consult-grep dir nil))
         (error "Module 'consult' is not loaded"))))))

(provide 'init-write)

;;; init-write.el ends here

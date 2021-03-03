;;; init-write.el --- Enjoy Writing!! -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;; commentary

;;; Code:

(leaf markdown-mode
  :doc "deps : edit-indirect"
  :mode
  ("README\\.md\\'" . gfm-mode)
  (("\\.md\\'" "\\.markdown\\'") . markdown-mode)
  :init
  (setq markdown-command "pandoc"
        markdown-enable-wiki-links t
        markdown-enable-math t
        markdown-use-pandoc-style-yaml-metadata t
        markdown-css-paths `(,(expand-file-name "github-markdown.css" my-dir-ext))))

(leaf writeroom :commands writeroom-mode)

(leaf easy-hugo
  :commands easy-hugo
  :init
  ;; TODO Impletion needed
  (setq easy-hugo-basedir  "~/Code/blog/"
        easy-hugo-url  "https://liuyinz.github.io/"
        easy-hugo-preview-url ""
        easy-hugo-postdir "content/posts")
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

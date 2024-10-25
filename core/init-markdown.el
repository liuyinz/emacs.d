;;; init-markdown.el --- setting for markdown -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;; SEE https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-markdown.el

;;; Code:

(leaf markdown-mode
  :mode
  ("\\.mdx?\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  ("CHANGELOG\\.md\\'" . markdown-view-mode)
  :init
  ;; `multimarkdown' is necessary for `highlight.js' and `mermaid.js'
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))

  (setq markdown-enable-wiki-links t
        markdown-enable-math t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        ;; markdown-gfm-uppercase-checkbox nil
        markdown-fontify-code-blocks-natively t
        markdown-enable-highlighting-syntax t
        markdown-gfm-additional-languages '("Mermaid"))
  (setq markdown-list-indent-width 2)
  (setq markdown-list-item-bullets
        '("◎" "●" "◇" "○" "◆" "►" "•"))

  :defer-config
  (prependq! markdown-code-lang-modes
             '(("mermaid" . mermaid-ts-mode)
               ("zsh" . sh-mode)
               ("jsx" . js-jsx-mode)
               ("tsx" . tsx-ts-mode))))

(leaf markdown-toc
  :hook (markdown-mode-hook . markdown-toc-mode)
  :init
  (setq markdown-toc-header-toc-start "<!-- markdown-toc start -->"
        markdown-toc-indentation-space markdown-list-indent-width
        markdown-toc-header-toc-title "\n## Contents")
  :defer-config

  ;; ISSUE https://github.com/ardumont/markdown-toc/issues/47
  (defun markdown-toc-refresh-before-save ()
    "Refresh markdown TOC before save."
    (when (and (derived-mode-p 'markdown-mode)
               (require 'markdown-toc)
               (markdown-toc--toc-already-present-p))
      (markdown-toc-generate-toc t)))
  (add-hook 'before-save-hook #'markdown-toc-refresh-before-save))

;; REQUIRE pip install grip colorama
(leaf grip-mode
  :commands grip-start-preview
  :defer-config
  ;; BUG lose md code block background
  (setq grip-preview-use-webkit t))

(leaf xwidget
  :commands xwidget-webkit-current-session)

(provide 'init-markdown)

;;; init-markdown.el ends here

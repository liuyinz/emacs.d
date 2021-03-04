;;; init-markdown.el --- setting for markdown -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;; TRICK ,@https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-markdown.el

;;; Code:

(leaf markdown-mode
  :doc "deps : edit-indirect; brew install multimarkdown"
  :mode
  ("README\\.md\\'" . gfm-mode)
  (("\\.md\\'" "\\.markdown\\'") . markdown-mode)
  :init
  ;; `multimarkdown' is necessary for `highlight.js' and `mermaid.js'
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))

  (setq markdown-enable-wiki-links t
        ;; markdown-enable-math t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t
        markdown-content-type "application/xhtml+xml"
        markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                             "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-gfm-additional-languages "Mermaid"
        markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body {
  box-sizing: border-box;
  max-width: 740px;
  width: 100%;
  margin: 40px auto;
  padding: 0 10px;
}
</style>
<link rel='stylesheet' href='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/default.min.css'>
<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
  document.querySelectorAll('pre code').forEach((code) => {
    if (code.className != 'mermaid') {
      hljs.highlightBlock(code);
    }
  });
});
</script>
<script src='https://unpkg.com/mermaid@8.4.8/dist/mermaid.min.js'></script>
<script>
mermaid.initialize({
  theme: 'default',  // default, forest, dark, neutral
  startOnLoad: true
});
</script>
")
  :config
  (add-to-list 'markdown-code-lang-modes '("mermaid" . mermaid-mode))
  ;; HACK Preview with built-in webkit
  (with-no-warnings
    (defun my-markdown-export-and-preview (fn)
      "Preview with `xwidget' if applicable, otherwise with the default browser."
      (if (featurep 'xwidget-internal)
          (webkit-browse-url (concat "file://" (markdown-export)) t)
        (funcall fn)))
    (advice-add #'markdown-export-and-preview :around #'my-markdown-export-and-preview)))

;; Table of contents
(leaf markdown-toc
  :doc "deps : markdown-mode"
  :commands markdown-toc-generate-or-refresh-toc markdown-toc-delete-toc)

;; Markdown Preview
(leaf grip-mode
  :doc " pip install grip"
  :commands grip-restart-preview grip-start-preview grip-stop-preview
  :init
  (setq grip-preview-use-webkit t
        grip-update-after-change t))

(provide 'init-markdown)

;;; init-markdown.el ends here

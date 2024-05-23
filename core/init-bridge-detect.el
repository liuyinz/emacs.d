;;; init-bridge-detect.el --- summary -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2024-05-21 00:21:09

;;; Commentary:

;;; Code:


;;; base function

(defun jsreact-p (ext)
  (or (string= ext "jsx")
      (memq major-mode '(jtsx-jsx-mode js-jsx-mode))))

(defun tsreact-p (ext)
  (or (string= ext "tsx")
      (memq major-mode '(jtsx-tsx-mode jtsx-typescript-mode tsx-ts-mode))))

(defun markdown-p (ext)
  (or (member ext '("md" "gfm"))
      (memq major-mode '(markdown-mode markdown-ts-mode))))

(defun handlebar-p (ext)
  (or (member ext '("handlebars" "hbs"))
      (eq major-mode 'handlebars-mode)))

(defun css-like-p (ext)
  (or (member ext '("css" "less" "scss"))
      (memq major-mode '(css-mode css-ts-mode less-css-mode scss-mode))))

(defun html-p (ext)
  (or (memq ext '("htm" "html"))
      (memq major-mode '(mhtml-mode html-mode html-ts-mode web-mode))))


;;; single server detect
;; (push '((jtsx-jsx-mode) . "tailwindcss") lsp-bridge-single-lang-server-mode-list)
;; ;; HACK remove sh-mode in default mode list and enable it only when sh-shell is not zsh
;; (setq lsp-bridge-single-lang-server-mode-list
;;       (remove (rassoc "bash-language-server" lsp-bridge-single-lang-server-mode-list)
;;               lsp-bridge-single-lang-server-mode-list))

;; (setq lsp-bridge-get-single-lang-server-by-project #'my/bridge-single-server-detect)
;; (defun my/bridge-single-server-detect (project-path filepath)
;;   "Detect right server config for single server."
;;   (save-excursion
;;     (let* ((ext (file-name-extension filepath))
;;            (toml-p (or (string= ext "toml")
;;                        (memq major-mode '(toml-ts-mode conf-toml-mode))))
;;            (non-zsh-sh-p
;;             (or (memq major-mode '(bash-mode bash-ts-mode))
;;                 (and (eq major-mode 'sh-mode) (not (string= ext "zsh"))))))
;;       (cond
;;        (toml-p "toml-language-server")
;;        ;; (non-zsh-sh-p "bash-language-server")
;;        ))))


;;; multi-server detect
(setq lsp-bridge-get-multi-lang-server-by-project 'my/bridge-multi-server-detect)
(defun my/bridge-multi-server-detect (project_path filepath)
  (save-excursion
    ;; detect for web dev
    ;; NOTE project-path return same value as filepath if lsp-bridge cannot detect project
    ;; so check it ahead, tailwindcss do not support single file mode
    (let ((ext (file-name-extension filepath))
          (tailwindcss-p (and (file-directory-p project_path)
                              (directory-files
                               project_path
                               'full
                               "tailwind\\.config\\.\\(j\\|cj\\|mj\\|t\\)s\\'"))))
      (cond
       ;; ext/multi
       ((and tailwindcss-p (jsreact-p ext)) "jsreact_tailwindcss")
       ((and tailwindcss-p (tsreact-p ext)) "tsreact_tailwindcss")
       ((and tailwindcss-p (html-p ext)) "html_emmet_tailwindcss")
       ((and tailwindcss-p (css-like-p ext)) "css_emmet_tailwindcss")
       ;; lib/multi
       ((css-like-p ext) "css_emmet")
       ((html-p ext) "html_emmet")))))



;;; multi-server languageId detect

;; SEE https://github.com/tailwindlabs/tailwindcss-intellisense/blob/master/packages/tailwindcss-language-service/src/util/languages.ts
;; SEE https://github.com/aca/emmet-ls#readme

(setq lsp-bridge-get-language-id 'my/bridge-get-language-id)

(defun my/bridge-get-language-id (project filepath server ext)
  (pcase server
    ;; tailwindcss filetypes:
    ;; html-kind: 'aspnetcorerazor','astro','astro-markdown','blade','django-html',
    ;; 'edge', 'ejs', 'erb', 'gohtml', 'GoHTML', 'gohtmltmpl', 'haml', 'handlebars',
    ;; 'hbs', 'html', 'HTML (Eex)', 'HTML (EEx)','html-eex', 'htmldjango', 'jade',
    ;; 'leaf', 'liquid', 'markdown', 'mdx','mustache','njk','nunjucks','phoenix-heex',
    ;; 'php', 'razor', 'slim','surface','twig',
    ;; css-kind 'css','less', 'postcss', 'sass','scss', 'stylus', 'sugarss','tailwindcss',
    ;; js-kind: 'javascript','javascriptreact', 'reason', 'rescript',
    ;; 'typescript','typescriptreact','glimmer-js','glimmer-ts',
    ("tailwindcss"
     (pcase ext
       ((pred jsreact-p) "javascriptreact")
       ((pred tsreact-p) "typescriptreact")
       ((pred markdown-p) "markdown")
       ((pred handlebar-p) "handlebars")
       ("js" "javascript")
       ("ts" "typescript")
       ("res" "rescript")
       (_ ext)))
    ;; emmet supports following filetype:
    ;; 'css', 'eruby', 'html', 'javascript', 'javascriptreact', 'less',
    ;; 'sass', 'scss', 'svelte', 'pug', 'typescriptreact', "vue"
    ("emmet-ls"
     (pcase ext
       ((pred jsreact-p) "javascriptreact")
       ((pred tsreact-p) "typescriptreact")
       ("js" "javascript")
       ("erb" "eruby")
       ((or "css" "html" "less" "sass" "scss" "svelte" "pug" "vue") ext)
       (_ "html")))
    ;; eslint lsp support
    ;; 'javascript', 'javascriptreact', 'typescript', 'typescriptreact',
    ;; 'html', 'vue', 'markdown'
    ("vscode-eslint-language-server"
     (pcase ext
       ((pred jsreact-p) "javascriptreact")
       ((pred tsreact-p) "typescriptreact")
       ((pred markdown-p) "markdown")
       ((pred html-p) "html")
       ("js" "javascript")
       ("ts" "typescript")
       ("vue" "vue")))))

(provide 'init-bridge-detect)
;;; init-bridge-detect.el ends here

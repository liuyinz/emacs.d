;;; init-bridge-detect.el --- summary -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2024-05-21 00:21:09

;;; Commentary:

;; SEE https://github.com/Microsoft/vscode-eslint#settings-options
;; SEE https://github.com/neoclide/coc-css
;; SEE https://github.com/neoclide/coc-json#configuration-options
;; SEE https://github.com/redhat-developer/yaml-language-server#language-server-settings
;; NOTE yaml-language-server, coc-json implements schemastore supports, however
;; vscode-json-language-server not yet, which means need to add schemastore catalog
;; to vscode-json-language-server.json manually.

;; ISSUE https://github.com/redhat-developer/yaml-language-server/issues/807
;; yaml server init error: "Cannot read properties of undefined (reading ’length’)"

;;; Code:


;;; base function

;; HACK filepath like "../.zshenv" will return nil if call `file-name-extension'
;; directly, so add a prefix to return zshenv
(defun filepath-ext (filepath)
  (or (file-name-extension (concat "a" (file-name-nondirectory filepath))) ""))

(defun typescript-ls-get-id (ext)
  (cond
   ((or (string= ext "js")
        (memq major-mode '(js-mode js-ts-mode rjsx-mode)))
    "javascript")
   ((or (string= ext "ts")
        (memq major-mode '(typescript-mode typescript-ts-mode)))
    "typescript")
   ((or (string= ext "jsx")
        (memq major-mode '(jtsx-jsx-mode js-jsx-mode)))
    "javascriptreact")
   ((or (string= ext "tsx")
        (memq major-mode '(jtsx-tsx-mode jtsx-typescript-mode tsx-ts-mode)))
    "typescriptreact")))

(defun css-ls-get-id (ext)
  (cond
   ((or (string= ext "css")
        (memq major-mode '(css-mode css-ts-mode)))
    "css")
   ((or (string= ext "scss")
        (eq major-mode 'scss-mode))
    "scss")
   ((or (string= ext "less")
        (eq major-mode 'less-css-mode))
    "less")))

(defun web-file-get-server (ext)
  (cond
   ((or (member ext '("htm" "html"))
        (memq major-mode '(mhtml-mode html-mode html-ts-mode)))
    "html")
   ((or (string= ext "astro")
        (eq major-mode 'astro-mode))
    "astro")
   ((or (member ext '("css" "less" "scss"))
        (memq major-mode '(css-mode css-ts-mode less-css-mode scss-mode)))
    "css")))

(defun handlebar-p (ext)
  (or (member ext '("handlebars" "hbs"))
      (eq major-mode 'handlebars-mode)))

(defun markdown-p (ext)
  (or (string= ext "md")
      (memq major-mode '(markdown-mode gfm-mode markdown-ts-mode))))

;; NOTE project-path return same value as filepath if lsp-bridge cannot detect project
;; so check it ahead, tailwindcss do not support single file mode
(defun tailwindcss-p (project_path)
  (and (file-directory-p project_path)
       (directory-files
        project_path
        'full
        "tailwind\\.config\\.\\(j\\|cj\\|mj\\|t\\)s\\'")))

(defun eslint-p (project_path)
  (and (file-directory-p project_path)
       (directory-files
        project_path
        'full
        "\\`\\(eslint\\.config\\.[m|c]?js\\|\\.eslintrc\\.\\(c?js\\|ya?ml\\|json\\)\\)\\'")))


;;; single server detect
(prependq! lsp-bridge-single-lang-server-mode-list
           '(((js-mode js-ts-mode rjsx-mode
                       typescript-mode typescript-ts-mode
                       jtsx-jsx-mode js-jsx-mode
                       jtsx-tsx-mode jtsx-typescript-mode tsx-ts-mode)
              . "typescript-ls")
             ;; BUG bug in lsp
             ;; ((markdown-ts-mode gfm-mode) . "marksman")
             (lua-ts-mode . "sumneko")))

(setq lsp-bridge-get-single-lang-server-by-project #'my/bridge-single-server-detect)
(defun my/bridge-single-server-detect (project_path filepath)
  (save-excursion
    (let* ((ext (filepath-ext filepath))
           (toml-p (or (and ext (string= ext "toml"))
                       (memq major-mode '(toml-ts-mode conf-toml-mode)))))
      (let ((server (cond
                     ((typescript-ls-get-id ext) "typescript-ls")
                     (toml-p "toml-language-server"))))
        (temp-log (format (concat "%s" (s-repeat 4 "\n%-13s: %S"))
                          (propertize "[lsp-bridge-single-server]" 'face 'success)
                          "project_path" project_path
                          "file_path" filepath
                          "file_ext" ext
                          "start_server" server))
        server))))


;;; multi-server detect
(setq lsp-bridge-multi-lang-server-extension-list nil)
;; (setq lsp-bridge-multi-lang-server-mode-list nil)

(setq lsp-bridge-get-multi-lang-server-by-project 'my/bridge-multi-server-detect)
(defun my/bridge-multi-server-detect (project_path filepath)
  (save-excursion
    ;; detect for web dev
    (let* ((ext (filepath-ext filepath))
           (tailwindcss-suffix (and (tailwindcss-p project_path) "_tailwindcss")))
      (let ((server
             (cond
              ;; TODO insert eslint
              ((member (typescript-ls-get-id ext) '("javascriptreact" "typescriptreact"))
               (concat "typescript_emmet" tailwindcss-suffix))
              ((web-file-get-server ext)
               (concat (web-file-get-server ext) "_emmet" tailwindcss-suffix)))))
        (temp-log (format (concat "%s" (s-repeat 4 "\n%-13s: %S"))
                          (propertize "[lsp-bridge-multi-server]" 'face 'error)
                          "project_path" project_path
                          "file_path" filepath
                          "file_ext" ext
                          "start_server" server))
        server))))


;;; multi-server languageId detect

;; SEE https://github.com/tailwindlabs/tailwindcss-intellisense/blob/master/packages/tailwindcss-language-service/src/util/languages.ts
;; SEE https://github.com/aca/emmet-ls#readme
;; SEE https://github.com/microsoft/vscode-eslint/blob/3147111b1bd430a9f29d7a66916e1d822eba3df3/package.json#L307-L316

(setq lsp-bridge-get-language-id 'my/bridge-get-language-id)

(defun my/bridge-get-language-id (project_path filepath server ext)
  (let ((id (pcase server
              ("typescript-ls" (typescript-ls-get-id ext))
              ("vscode-css-language-server" (css-ls-get-id ext))
              ("tailwindcss"
               (pcase ext
                 ((pred typescript-ls-get-id) (typescript-ls-get-id ext))
                 ((pred markdown-p) "markdown")
                 ((pred handlebar-p) "handlebars")
                 ("res" "rescript")
                 (_ ext)))
              ("emmet-ls"
               (pcase ext
                 ((pred typescript-ls-get-id) (typescript-ls-get-id ext))
                 ("erb" "eruby")
                 ((or "css" "html" "less" "sass" "scss" "svelte" "pug" "vue") ext)
                 (_ "html")))
              ("vscode-eslint-language-server"
               (pcase ext
                 ((pred typescript-ls-get-id) (typescript-ls-get-id ext))
                 ((pred markdown-p) "markdown")
                 (_ ext))))))
    (temp-log (format (concat "%s" (s-repeat 5 "\n%-13s: %S"))
                      (propertize "[lsp-bridge-get-id]" 'face 'warning)
                      "project_path" project_path
                      "file_path" filepath
                      "file_ext" ext
                      "start_server" server
                      "lang_id" id))
    id))

(provide 'init-bridge-detect)
;;; init-bridge-detect.el ends here

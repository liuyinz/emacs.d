;;; init-bridge-detect.el --- summary -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;; SEE https://github.com/typescript-language-server/typescript-language-server/blob/HEAD/docs/configuration.md
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
   ((or (string= ext "js") (memq major-mode '(js-mode js-ts-mode)))
    "javascript")
   ((or (string= ext "ts") (eq major-mode 'typescript-ts-mode))
    "typescript")
   ((or (string= ext "jsx") (eq major-mode 'js-jsx-mode))
    "javascriptreact")
   ((or (string= ext "tsx") (eq major-mode 'tsx-ts-mode))
    "typescriptreact")))

(defun css-ls-get-id (ext)
  (cond
   ((or (string= ext "css") (memq major-mode '(css-mode css-ts-mode)))
    "css")
   ((or (string= ext "scss") (eq major-mode 'scss-mode))
    "scss")
   ((or (string= ext "less") (eq major-mode 'less-css-mode))
    "less")))

(defun web-file-get-server (ext)
  (cond
   ((or (member ext '("htm" "html"))
        (memq major-mode '(mhtml-mode html-mode html-ts-mode)))
    "html")
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


;; customize func to define project path
(setq lsp-bridge-get-project-path-by-filepath nil)

;; server detect order:
;; 1. run   `lsp-bridge-get-multi-lang-server-by-project';
;; 2. match `lsp-bridge-multi-lang-server-extension-list';
;; 3. match `lsp-bridge-multi-lang-server-mode-list';
;; 4. run   `lsp-bridge-get-single-lang-server-by-project';
;; 5. match `lsp-bridge-single-lang-server-extension-list';
;; 6. match `lsp-bridge-single-lang-server-mode-list';

;;
;;; multi-server detect
(setq lsp-bridge-get-multi-lang-server-by-project #'my/bridge-multi-server-detect)

(defun my/bridge-multi-server-detect (project_path filepath)
  "Return multi server config according to project extra requirements.
PROJECT_PATH and FILEPATH is needed."
  (save-excursion
    (let* ((ext (filepath-ext filepath))
           (tailwindcss-suffix (and (tailwindcss-p project_path) "_tailwindcss"))
           (eslint-suffix (and (eslint-p project_path) "_eslint")))
      (when (or tailwindcss-suffix eslint-suffix)
        (let ((server
               (cond
                ((member (typescript-ls-get-id ext)
                         '("javascriptreact" "typescriptreact"))
                 (concat "typescript_emmet" tailwindcss-suffix eslint-suffix))
                ((member (typescript-ls-get-id ext)
                         '("javascript" "typescript"))
                 (concat "typescript" eslint-suffix))
                ((web-file-get-server ext)
                 (concat (web-file-get-server ext) "_emmet" tailwindcss-suffix)))))
          (temp-log (format (concat "%s" (s-repeat 4 "\n%-13s: %S"))
                            (propertize "[lsp-bridge-multi-server]" 'face 'error)
                            "project_path" project_path
                            "file_path" filepath
                            "file_ext" ext
                            "start_server" server))
          server)))))

(setq lsp-bridge-multi-lang-server-extension-list
      '((("tsx")   . "typescript_emmet")
        (("jsx")   . "javascript_emmet")
        (("html" "htm") . "html_emmet")
        (("css" "less" "scss") . "css_emmet")))

(setq lsp-bridge-multi-lang-server-mode-list
      `(((python-mode python-ts-mode) . ,lsp-bridge-python-multi-lsp-server)
        ((js-jsx-mode) . "javascript_emmet")
        ((tsx-ts-mode) . "typescript_emmet")
        ((mhtml-mode html-mode html-ts-mode) . "html_emmet")
        ((css-mode css-ts-mode less-css-mode scss-mode) . "css_emmet")))

;;; single server detect
;; (setq lsp-bridge-get-single-lang-server-by-project #'my/bridge-single-server-detect)
;; (defun my/bridge-single-server-detect (project_path filepath)
;;   (save-excursion
;;     (let* ((ext (filepath-ext filepath)))
;;       (let ((server (cond
;;                      ((typescript-ls-get-id ext) "typescript-ls")
;;                      ((or (and ext (string= ext "toml"))
;;                           (memq major-mode '(toml-ts-mode conf-toml-mode)))
;;                       "toml-language-server"))))
;;         (temp-log (format (concat "%s" (s-repeat 4 "\n%-13s: %S"))
;;                           (propertize "[lsp-bridge-single-server]" 'face 'success)
;;                           "project_path" project_path
;;                           "file_path" filepath
;;                           "file_ext" ext
;;                           "start_server" server))
;;         server))))

(prependq! lsp-bridge-single-lang-server-extension-list
           '((("toml") . "toml-language-server")
             (("vue")  . "volar")
             (("astro") . "astro-ls")
             (("mbt")  . "moonbit-ls")))

(prependq! lsp-bridge-single-lang-server-mode-list
           '(((toml-ts-mode conf-toml-mode) . "toml-language-server")
             ((js-mode js-ts-mode typescript-ts-mode js-jsx-mode tsx-ts-mode)
              . "typescript-ls")
             (lua-ts-mode . "sumneko")
             (web-vue-mode . "volar")
             (astro-ts-mode . "astro-ls")
             (moonbit-mode . "moonbit-ls")))


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


;; server customize after start

(add-hook 'lsp-bridge-mode-hook #'my/bridge-server-setup)
(defun my/bridge-server-setup ()
  (run-with-timer
   3 nil
   (lambda()
     (with-current-buffer (current-buffer)
       (when (bound-and-true-p acm-backend-lsp-server-names)
         (let ((servers acm-backend-lsp-server-names))
           (when (member "tailwindcss" servers)
             (modify-syntax-entry ?- "w")
             (setq-local lsp-bridge-enable-completion-in-string t))
          ;;; no need to setup at all.
           ;; (when (member "emmet-ls" servers)
           ;;   (cond
           ;;    ((member "vscode-css-language-server" servers)
           ;;     (setq-local lsp-bridge-completion-hide-characters
           ;;                 (seq-difference lsp-bridge-completion-hide-characters
           ;;                                 '("@" "!" "+"))))
           ;;    ((seq-intersection '("vscode-html-language-server"
           ;;                         "astro-ls"
           ;;                         "typescript-ls")
           ;;                       servers)
           ;;     ;; HACK trade off between emmet-ls and html
           ;;     (setq-local lsp-bridge-completion-hide-characters
           ;;                 (seq-difference lsp-bridge-completion-hide-characters
           ;;                                 '("$" "*" "#" "." "!"))))))
           ;; enable - in tailwindcss completion
           (temp-log (format (concat "%s" (s-repeat 2 "\n%-13s: %S"))
                             (propertize "[lsp-bridge-servers]" 'face 'warning)
                             "current_buffer" (current-buffer)
                             "servers" servers))))))))

(provide 'init-bridge-detect)
;;; init-bridge-detect.el ends here

;; Local Variables:
;; jinx-local-words: "mj"
;; End:

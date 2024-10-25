;;; init-web.el --- Setting for web related mode -*- lexical-binding: t no-byte-compile: t -*-

;; Author: 食無魚
;; Created: 2021-07-17 20:16:00

;;; Commentary:

;;; Code:



;;; css

(leaf css-mode
  :init
  (setq css-indent-offset 2)
  (setq css-fontify-colors nil))

;; ;; SCSS mode
;; (leaf scss-mode
;;   :init (setq scss-compile-at-save nil))

;; (leaf less-css-mode
;;   :init (setq less-css-compile-at-save nil))

;; TODO tailwind color completion font-awesome completion or to write a consult-fontawesome command
;; SEE https://github.com/FortAwesome/Font-Awesome/blob/6.x/metadata/icons.json
(defvar tailwindcss-frameworks
  '("nextjs" "laravel" "vite" "nuxtjs" "gatsby" "solidjs" "sveltekit" "angular"
    "ruby-on-rails" "remix" "phoenix" "parcel" "symfony" "meteor" "create-react-app"
    "adonisjs" "emberjs" "astro" "qwik" "rspack")
  "List of available frameworks supported by tailwindcss.")

(defun tailwindcss-framework-setup (framework)
  "Browser instructions for FRAMEWORK to use tailwindcss."
  (interactive
   (list (completing-read "Select framework: " tailwindcss-frameworks nil t)))
  (browse-url (format "https://tailwindcss.com/docs/guides/%s"
                      (url-hexify-string framework))))

;; TODO write a plugin for transform.tools
(defvar transform-tools-options
  '("svg-to-jsx" "svg-to-react-native" "html-to-jsx" "html-to-pug" "json-to-proptypes"
    "json-to-flow" "json-to-graphql" "json-to-typescript" "json-to-mobx-state-tree"
    "json-to-sarcastic" "json-to-io-ts" "json-to-rust-serde" "json-to-mongoose"
    "json-to-big-query" "json-to-mysql" "json-to-scala-case-class" "json-to-go"
    "json-to-go-bson" "json-to-yaml" "json-to-jsdoc" "json-to-kotlin" "json-to-java"
    "json-to-json-schema" "json-to-toml" "json-to-zod" "json-schema-to-typescript"
    "json-schema-to-openapi-schema" "json-schema-to-protobuf" "json-schema-to-zod"
    "css-to-js" "object-styles-to-template-literal" "css-to-tailwind" "js-object-to-json"
    "js-object-to-typescript" "graphql-to-typescript" "graphql-to-flow" "graphql-to-java"
    "graphql-to-resolvers-signature" "graphql-to-introspection-json" "graphql-to-schema-ast"
    "graphql-to-fragment-matcher" "graphql-to-components" "graphql-to-typescript-mongodb"
    "jsonld-to-nquads" "jsonld-to-expanded" "jsonld-to-compacted" "jsonld-to-flattened"
    "jsonld-to-framed" "jsonld-to-normalized" "typescript-to-flow" "typescript-to-typescript-declaration"
    "typescript-to-json-schema" "typescript-to-javascript" "typescript-to-zod" "flow-to-typescript"
    "flow-to-typescript-declaration" "flow-to-javascript" "xml-to-json" "yaml-to-json" "yaml-to-toml"
    "markdown-to-html" "toml-to-json" "toml-to-yaml" "cadence-to-go")
  "List of available options supported by transform.tools.")

(defun transform-tools-search (option)
  "Browser transform page for OPTION in transform.tools."
  (interactive
   (list (completing-read "Select option " transform-tools-options nil t)))
  (when (use-region-p)
    (simpleclip-set-contents
     (substring-no-properties
      (filter-buffer-substring (region-beginning) (region-end)))))
  (browse-url (format "https://transform.tools/%s" (url-hexify-string option))))


;;; html

(leaf impatient-mode
  :commands imp-visit-buffer
  :init
  (defun html-anchor-local-files ()
    "Return a list of local files which is anchord to current html buffer."
    (let (files)
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward
                  "<\\(link\\|script\\)[^>]+\\(href\\|src\\)=[\"']\\([^\"']+\\(\\.css\\|\\.js\\)\\)[\"']" nil t)
            (let* ((file (expand-file-name (match-string 3))))
              (when (file-exists-p file)
                (push file files))))))
      (reverse files)))

  (defun impatient--anchored-on-change (anchor-buf html-buf)
    (when (and (buffer-live-p anchor-buf)
               (buffer-modified-p anchor-buf)
               (buffer-live-p html-buf)
               (buffer-local-value 'impatient-mode html-buf))
      (with-current-buffer anchor-buf
        (basic-save-buffer))
      (with-current-buffer html-buf
        (imp--update-buffer))))

  (defun impatient-watch-anchored-files ()
    "Watch buffer changes of anchord files in current html buffer if impatient enabled."
    (interactive)
    (let ((html-buf (current-buffer)))
      (when impatient-mode
        (dolist (f (html-anchor-local-files))
          (let ((anchor-buf (or (get-file-buffer f) (find-file-noselect f))))
            (with-current-buffer anchor-buf
              (setq-local impatient-anchored-timer
                          (run-with-idle-timer
                           2
                           :repeat
                           #'impatient--anchored-on-change
                           anchor-buf
                           html-buf))
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (when (timerp impatient-anchored-timer)
                            (cancel-timer impatient-anchored-timer)))
                        nil t)))))))

  )

;; Major mode for editing web templates
(leaf web-mode
  ;; :hook (web-mode-hook . web-mode-setup)
  :mode "\\.\\(htm\\|html\\|phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\)$"
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-part-padding 2
        ;; web-mode-enable-auto-closing nil
        ;; web-mode-enable-auto-opening nil
        web-mode-enable-auto-pairing nil
        web-mode-enable-auto-quoting t
        ;; web-mode-enable-auto-expanding nil
        ;; web-mode-enable-auto-indentation nil
        web-mode-enable-current-element-highlight t
        ;; web-mode-enable-current-column-highlight nil
        ;; web-mode-enable-block-face t
        ;; web-mode-enable-part-face t
        ;; web-mode-enable-inlays t
        ;; web-mode-enable-sql-detection t
        ;; web-mode-enable-front-matter-block t
        ;; web-mode-enable-element-content-fontification nil
        ;; web-mode-enable-element-tag-fontification nil
        web-mode-enable-html-entities-fontification t)
  )


;;; js

(leaf js
  :mode "\\.[mc]js\\'"
  :init
  (setq js-indent-level 2
        js-chain-indent t
        js-jsx-indent-level 2))


;;; ts

;; NOTE support generics <> with ,<
(defun ah/typescript-generics-angle-pair ()
  (when (and (eql last-command-event ?<) (looking-back ",<" 1))
    (backward-delete-char 2)
    (insert "<>")
    (backward-char)))

(leaf typescript-ts-mode
  :mode "\\.ts\\'"
  :hook (typescript-ts-mode-hook . typescript-ts-mode-setup)
  :init
  (defun typescript-ts-mode-setup ()
    (add-hook 'post-self-insert-hook #'ah/typescript-generics-angle-pair nil t)))

(provide 'init-web)
;;; init-web.el ends here

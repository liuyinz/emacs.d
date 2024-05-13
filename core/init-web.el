;;; init-web.el --- Setting for web related mode -*- lexical-binding: t no-byte-compile: t -*-

;; Author: 食無魚
;; Created: 2021-07-17 20:16:00

;;; Commentary:

;;; Code:

;; --------------------------- Css --------------------------------

(leaf css-mode
  :init
  (setq css-indent-offset 2)
  (setq css-fontify-colors nil))

;; ;; SCSS mode
;; (leaf scss-mode
;;   :init (setq scss-compile-at-save nil))

;; (leaf less-css-mode
;;   :init (setq less-css-compile-at-save nil))

;; --------------------------- Html -------------------------------

(leaf impatient-mode
  :commands imp-visit-buffer)

;; Major mode for editing web templates
(leaf web-mode
  ;; :hook (web-mode-hook . web-mode-setup)
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\)$"
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
        web-mode-enable-html-entities-fontification t
        web-mode-enable-element-content-fontification t
        web-mode-enable-element-tag-fontification t
        )
  )

;; ---------------------------- JS --------------------------------

(leaf js
  :init
  (setq js-indent-level 2
        js-chain-indent t
        js-jsx-indent-level 2))

;; ---------------------------- TS --------------------------------

(leaf typescript-ts-mode
  :mode "\\.ts\\'"
  :hook (typescript-ts-mode-hook . typescript-ts-mode-setup)
  :init
  ;; NOTE support generics <> pair insertion, insert sinlge < use ,<
  (defun ah/typescript-generics-angle-pair ()
    (when (eql last-command-event ?<)
      (if (looking-back ",<" 1)
          (progn (backward-delete-char 2)
                 (insert "<"))
        (insert ">")
        (backward-char))))
  (defun typescript-ts-mode-setup ()
    (add-hook 'post-self-insert-hook #'ah/typescript-generics-angle-pair))

  )

(leaf jtsx
  :init
  (setq js-indent-level 2)
  (setq typescript-ts-mode-indent-offset 2)
  (setq jtsx-switch-indent-offset 0)
  (setq jtsx-indent-statement-block-regarding-standalone-parent nil)
  (setq jtsx-jsx-element-move-allow-step-out t)
  (setq jtsx-enable-jsx-electric-closing-element t)
  (setq jtsx-enable-all-syntax-highlighting-features t)
  (setq jtsx-enable-jsx-element-tags-auto-sync t)

  ;; HACK adding jtsx after treesit-auto to make sure it worked
  (defun jtsx-add-to-auto-mode-alist ()
    "Add file extension for jtsx-tsx/jsx-mode"
    (add-to-list 'auto-mode-alist '("\\(App\\.js\\|\\.jsx\\)\\'" . jtsx-jsx-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . jtsx-tsx-mode)))
  (advice-add 'treesit-auto-add-to-auto-mode-alist
              :after #'jtsx-add-to-auto-mode-alist)
  ;; lsp-bridge setup
  (with-eval-after-load 'lsp-bridge
    (prependq! lsp-bridge-single-lang-server-mode-list
               '(((jtsx-tsx-mode jtsx-typescript-mode) . "typescriptreact")
                 ((jtsx-jsx-mode) . "javascript"))))

  :config
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (dolist (pair '(("C-c h t" . jtsx-jump-jsx-element-tag-dwim)
                    ("C-c h o" . jtsx-jump-jsx-opening-tag)
                    ("C-c h c" . jtsx-jump-jsx-closing-tag)
                    ("C-c h r" . jtsx-rename-jsx-element)
                    ("C-c h j" . jtsx-move-jsx-element-tag-forward)
                    ("C-c h k" . jtsx-move-jsx-element-tag-backward)
                    ("C-c h J" . jtsx-move-jsx-element-forward)
                    ("C-c h K" . jtsx-move-jsx-element-backward)
                    ("C-c h a" . jtsx-move-jsx-element-step-in-forward)
                    ("C-c h b" . jtsx-move-jsx-element-step-in-backward)
                    ("C-c h w" . jtsx-wrap-in-jsx-element)
                    ("C-c h u" . jtsx-unwrap-jsx)
                    ("C-c h d" . jtsx-delete-jsx-node)))
      (keymap-set jtsx-jsx-mode-map (car pair) (cdr pair))))

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map)
  )

;; --------------------------- Node -------------------------------

(leaf nodejs-repl
  :bind
  (:js-ts-mode-map
   ("C-x C-e" . nodejs-repl-send-last-expression)
   ("C-c C-e" . nodejs-repl-send-region-or-buffer))
  :init
  (defun nodejs-repl-send-region-or-buffer ()
    "docstring"
    (interactive)
    (nodejs-repl)
    (if (use-region-p)
        (nodejs-repl-send-region (region-beginning) (region-end))
      (nodejs-repl-send-region (point-min)(point-max)))))

(defvar tailwindcss-frameworks
  '("nextjs" "laravel" "vite" "nuxtjs" "gatsby" "solidjs" "sveltekit" "angular"
    "ruby-on-rails" "remix" "phoenix" "parcel" "symfony" "meteor" "create-react-app"
    "adonisjs" "emberjs" "astro" "qwik" "rspack")
  "List of available frameworks supported by tailwindcss.")

(defun tailwindcss-framework-setup (framework)
  "Browser instructions for FRAMEWORK to use tailwindcss."
  (interactive
   (list (completing-read "Select framework: " tailwindcss-frameworks nil t)))
  (browse-url (format "https://tailwindcss.com/docs/guides/%s" (url-hexify-string framework))))

(provide 'init-web)
;;; init-web.el ends here

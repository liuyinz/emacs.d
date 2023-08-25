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
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
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
        ))

;; ---------------------------- JS --------------------------------

(leaf js
  :init
  (setq js-indent-level 2
        js-chain-indent t
        js-jsx-indent-level 2))

;; ---------------------------- TS --------------------------------

(leaf typescript-ts-mode :mode "\\.ts\\'")

;; --------------------------- Node -------------------------------

;; Adds node_modules/.bin directory to `exec_path'
(leaf add-node-modules-path
  :init
  ;; HACK https://github.com/codesuki/add-node-modules-path/issues/23#issuecomment-1594638810
  (setq add-node-modules-path-command '("echo \"$(npm root)/.bin\""))
  :hook ((web-mode-hook
          js-ts-mode-hook
          typescript-ts-mode-hook) . add-node-modules-path))

(leaf eslint-init)

(provide 'init-web)
;;; init-web.el ends here

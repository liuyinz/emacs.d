;;; init-web.el --- Setting for web related mode -*- lexical-binding: t no-byte-compile: t -*-

;; Author: 食無魚
;; Created: 2021-07-17 20:16:00

;;; Commentary:

;;; Code:

(leaf emmet-mode
  :hook (((css-mode-hook html-mode-hook mhtml-mode-hook web-mode-hook) . emmet-mode)
         (emmet-mode-hook . (lambda () (yas-minor-mode -1))))
  :bind
  (:emmet-mode-keymap
   ((kbd "C-j")   . my/emmet-expand)
   ([tab]         . emmet-next-edit-point)
   ((kbd "TAB")   . emmet-next-edit-point)
   ([backtab]     . emmet-prev-edit-point)
   ((kbd "S-TAB") . emmet-prev-edit-point))

  :init
  (setq emmet-indentation 2
        emmet-insert-flash-time -1
        emmet-indent-after-insert nil
        emmet-postwrap-goto-edit-point t)

  (defun my/emmet-expand ()
    "Expand emmet with select region or not."
    (interactive)
    (when (bound-and-true-p emmet-mode)
      (if (use-region-p)
          (emmet-wrap-with-markup)
        (emmet-expand-line))))
  )

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

;; ;; Major mode for editing web templates
;; (leaf web-mode
;;   :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
;;   :init
;;   (setq web-mode-markup-indent-offset 2
;;         web-mode-css-indent-offset 2
;;         web-mode-code-indent-offset 2
;;         ;; web-mode-enable-auto-closing nil
;;         ;; web-mode-enable-auto-opening nil
;;         web-mode-enable-auto-pairing nil
;;         web-mode-enable-auto-quoting t
;;         ;; web-mode-enable-auto-expanding nil
;;         ;; web-mode-enable-auto-indentation nil
;;         web-mode-enable-current-element-highlight t
;;         ;; web-mode-enable-current-column-highlight nil
;;         ;; web-mode-enable-block-face t
;;         ;; web-mode-enable-part-face t
;;         ;; web-mode-enable-inlays t
;;         ;; web-mode-enable-sql-detection t
;;         ;; web-mode-enable-front-matter-block t
;;         web-mode-enable-html-entities-fontification t
;;         web-mode-enable-element-content-fontification t
;;         web-mode-enable-element-tag-fontification t
;;         ))

;; ---------------------------- JS --------------------------------

(leaf js
  :init
  (setq js-indent-level 2
        js-chain-indent t
        js-jsx-indent-level 2))

(leaf js2-mode
  :mode
  ("\\.js\\'" . js2-mode)
  ("\\.jsx\\'" . js2-minor-mode)
  :interpreter ("node" . js2-mode)
  :init
  (setq js2-mode-assume-strict nil)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-mode-show-parse-errors nil))

(leaf imenu-extra)

;; ---------------------------- TS --------------------------------

(leaf typescript-mode)

;; --------------------------- Node -------------------------------

;; Adds node_modules/.bin directory to `exec_path'
(leaf add-node-modules-path
  :hook ((web-mode-hook js-mode-hook js2-mode-hook) . add-node-modules-path))

(provide 'init-web)
;;; init-web.el ends here

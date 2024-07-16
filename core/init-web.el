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
        web-mode-enable-html-entities-fontification t
        web-mode-enable-element-content-fontification t
        web-mode-enable-element-tag-fontification t
        )
  )

;; ASTRO
(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))



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

(leaf jtsx
  :require t
  :mode
  ("\\(App\\.js\\)\\|\\.jsx\\'" . jtsx-jsx-mode)
  ("\\.tsx\\'" . jtsx-tsx-mode)
  :init
  (setq js-indent-level 2)
  (setq typescript-ts-mode-indent-offset 2)
  (setq jtsx-switch-indent-offset 0)
  (setq jtsx-indent-statement-block-regarding-standalone-parent nil)
  (setq jtsx-jsx-element-move-allow-step-out t)
  (setq jtsx-enable-jsx-electric-closing-element t)
  (setq jtsx-enable-all-syntax-highlighting-features t)
  (setq jtsx-enable-jsx-element-tags-auto-sync t)

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
                    ("C-c h d" . jtsx-delete-jsx-node)
                    ("s-/"     . jtsx-comment-dwim)))
      (keymap-set mode-map (car pair) (cdr pair))))

  (defun jtsx-jsx-mode-setup ()
    (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-tsx-mode-setup ()
    (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map)
    (add-hook 'post-self-insert-hook #'ah/typescript-generics-angle-pair nil t))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-jsx-mode-setup)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-tsx-mode-setup)
  )


(provide 'init-web)
;;; init-web.el ends here

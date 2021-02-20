;;; init-web.el --- web setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf emmet-mode
  :hook ((sgml-mode-hook html-mode-hook css-mode-hook web-mode-hook) . emmet-mode)
  :init
  (setq emmet-indentation 2
        emmet-preview-default nil
        emmet-self-closing-tag-style " /"
        ;; emmet-indent-after-insert nil
        emmet-move-cursor-between-quotes nil)

  (defun my-emmet-expand ()
    (interactive)
    (emmet-preview-abort)
    (if (bound-and-true-p yas-minor-mode)
        (emmet-expand-yas)
      (emmet-preview-accept)))

  :config
  ;; hide info preview infomation
  (with-no-warnings
    (defun my-emmet-preview (beg end)
      (interactive (if (use-region-p)
                       (list (region-beginning) (region-end))
                     (list nil nil)))
      (emmet-preview-abort)
      (if (not beg)
          (message "Region not active")
        (setq emmet-old-show-paren show-paren-mode)
        (show-paren-mode -1)
        (let ((here (point)))
          (goto-char beg)
          (forward-line 1)
          (unless (= 0 (current-column))
            (insert "\n"))
          (let* ((opos (point))
                 (ovli (make-overlay beg end nil nil t))
                 (ovlo (make-overlay opos opos)))
            (overlay-put ovli 'face 'emmet-preview-input)
            (overlay-put ovli 'keymap emmet-preview-keymap)
            (overlay-put ovlo 'face 'emmet-preview-output)
            (setq emmet-preview-input  ovli)
            (setq emmet-preview-output ovlo)
            (add-hook 'before-change-functions 'emmet-preview-before-change t t)
            (goto-char here)
            (add-hook 'post-command-hook 'emmet-preview-post-command t t)))))

    (advice-add #'emmet-preview :override #'my-emmet-preview)))

(leaf css-mode
  :init
  (setq css-indent-offset 2)
  (setq css-fontify-colors nil))

;; SCSS mode
(leaf scss-mode
  :init (setq scss-compile-at-save nil))

(leaf less-css-mode
  :init (setq less-css-compile-at-save nil))

;; ;; CSS eldoc
;; (use-package css-eldoc
;;   :commands turn-on-css-eldoc
;;   :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

;; Major mode for editing web templates
(leaf web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :hook (web-mode-hook . my-web-mode-setup)
  :config
  (setq web-mode-part-padding 0
        web-mode-enable-comment-interpolation t
        ;; web-mode-enable-css-colorization nil
        web-mode-enable-comment-annotation t
        web-mode-enable-auto-indentation nil
        web-mode-enable-auto-expanding t
        web-mode-enable-current-element-highlight t
        web-mode-enable-sql-detection t
        web-mode-comment-formats '(("java" . "//")
                                   ("javascript" . "//")
                                   ("php" . "//")))
  (setq-default web-mode-markup-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-css-indent-offset 2)
  ;;highlight
  (set-face-attribute 'web-mode-current-element-highlight-face nil
                      :background "#5d7281"
                      :weight 'ultra-bold)
  ;;
  (defun my-web-mode-setup ()
    (when (equal web-mode-content-type "html")
      (flycheck-add-mode 'html-tidy 'web-mode)
      (flycheck-select-checker 'html-tidy)))

  ;; (modify-syntax-entry ?' "\"" web-mode-syntax-table)
  ;; (modify-syntax-entry ?` "\"" web-mode-syntax-table)
  ;; ;; "-" as word so company completes kabeb-case
  ;; (modify-syntax-entry ?_ "w" web-mode-syntax-table)
  ;; (modify-syntax-entry ?- "w" web-mode-syntax-table)
  ;; (modify-syntax-entry ?# "_" web-mode-syntax-table)
  )

;; JSON mode
;; (use-package json-mode)
;; (use-package haml-mode)
;; (use-package php-mode)

;; (use-package impatient-mode
;;   :hook ((web-mode-hook html-mode-hook css-mode-hook js-mode-hook js2-mode-hook) . impatient-mode)
;;   :config
;;   ;; @https://github.com/skeeto/impatient-mode/issues/22
;;   (defun imp-visit-buffer (&optional arg)
;;     "Visit the current buffer in a browser.
;; If given a prefix ARG, visit the buffer listing instead."
;;     (interactive "P")
;;     (unless (process-status "httpd")
;;       (httpd-start))
;;     (unless impatient-mode
;;       (impatient-mode))
;;     (unless (process-status "httpd") (httpd-start))
;;     (let ((url (format "http://%s:%d/imp/live/%s/"
;; 		               (cl-case httpd-host
;; 			             ((nil) "0.0.0.0")
;; 			             ((local) "localhost")
;; 			             (otherwise httpd-host))
;; 		               httpd-port
;; 		               (url-hexify-string (buffer-name)))))
;;       (browse-url url))))

;; ;; Live browser JavaScript, CSS, and HTML interaction
;; (use-package skewer-mode
;;   :blackout
;;   :hook (((js-mode js2-mode). skewer-mode)
;;          (css-mode . skewer-css-mode)
;;          (web-mode . skewer-html-mode)
;;          (html-mode . skewer-html-mode))
;;   :init
;;   ;; blackout
;;   (with-eval-after-load 'skewer-css
;;     (blackout 'skewer-css-mode))
;;   (with-eval-after-load 'skewer-html
;;     (blackout 'skewer-html-mode)))

;; REST
;; (use-package restclient
;;   :mode ("\\.http\\'" . restclient-mode)
;;   :config
;;   (use-package restclient-test
;;     :hook (restclient-mode . restclient-test-mode))
;;
;;   (with-eval-after-load 'company
;;     (use-package company-restclient
;;       :defines company-backends
;;       :init (add-to-list 'company-backends 'company-restclient))))
;;
(provide 'init-web)
;;; init-web.el ends here

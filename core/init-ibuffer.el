(use-package ibuffer
  :defines ibuffer-show-empty-filter-groups
  :ensure nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :commands (ibuffer ibuffer-use-other-window)
  :init
  ;; hide summary
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-show-empty-filter-groups nil)
  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map
           :summarizer
           (lambda (column-strings)
             (cl-loop for s in column-strings
                      sum (get-text-property (1- (length s)) 'size s) into total
                      finally return (file-size-human-readable total))))
    (let ((size (buffer-size)))
      (propertize (file-size-human-readable size)
                  'size size)))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark " " (name 16 16 :left :elide)
                " " modified read-only locked
                " " (size-h 6 -1 :center)
                " " (mode 16 16 :left :elide)
                ;; " " project-relative-file
                " " filename-and-process
                ))))

;; ;; defined groups
;; (setq ibuffer-saved-filter-groups
;;       '(("user"
;;          ("Dired" (mode . dired-mode))
;;          ("Config" (name . "^init-.+\\.el$"))
;;          ("org" (mode . org-mode))
;;          ("Sys" (name . "^\\*.+\\*$"))
;;          ("Magit" (name . "^magit"))
;;          )))
;; (add-hook 'ibuffer-mode-hook
;;           (lambda ()
;;             (ibuffer-switch-to-saved-filter-groups "user")))

;; ;; hidden filters
;; (setq-default ibuffer-hidden-filter-groups
;;               '("Sys" "Magit"))

;; ;; Collapse
;; (defun ibuffer-collapse-all-filter-groups ()
;;   "Collapse all filter groups at once"
;;   (interactive)
;;   (setq ibuffer-hidden-filter-groups
;;         (mapcar #'car (ibuffer-current-filter-groups-with-position)))
;;   (ibuffer-update nil t))

;; ;; "Expand all filter groups at once"
;; (defun ibuffer-expand-all-filter-groups ()
;;   (interactive)
;;   (setq ibuffer-hidden-filter-groups nil)
;;   (ibuffer-update nil t))

(provide 'init-ibuffer)

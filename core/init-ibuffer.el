;;; init-ibuffer.el --- ibuffer setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf ibuffer
  :defines ibuffer-show-empty-filter-groups
  :hook (ibuffer-mode-hook .  (lambda ()
                                (ibuffer-auto-mode)
                                (ibuffer-vc-set-filter-groups-by-vc-root)
                                (unless (eq ibuffer-sorting-mode 'alphabetic)
                                  (ibuffer-do-sort-by-alphabetic))))
  :init
  ;; hide summary
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-show-empty-filter-groups nil)
  :defer-config
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

  ;; ;; Modify the default ibuffer-formats
  ;; (setq ibuffer-formats
  ;;       '((mark " " (name 16 16 :left :elide)
  ;;               " " modified read-only locked
  ;;               " " (size-h 6 -1 :center)
  ;;               " " (mode 16 16 :left :elide)
  ;;               " " project-relative-file
  ;;               " " filename-and-process
  ;;               )))

  )

(leaf ibuffer-vc
  :defer-config
  (setq ibuffer-formats
        '((mark " " modified read-only vc-status-mini
                " " (name 18 18 :left :elide)
                " " (size-h 7 -1 :right)
                " " (mode 16 16 :left :elide)
                " " (vc-status 16 16 :center)
                " " vc-relative-file))))

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
;;; init-ibuffer.el ends here

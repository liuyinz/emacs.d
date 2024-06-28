;;; init-ibuffer.el --- ibuffer setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf ibuffer
  :hook (ibuffer-mode-hook . ibuffer-mode-setup)
  :bind
  ("C-x C-b" . ibuffer)
  (:ibuffer-mode-map
   ("G" . ibuffer-toggle)
   ("=" . ibuffer-diff-buffers))
  :init
  (setq ibuffer-expert t
        ibuffer-display-summary nil
        ibuffer-show-empty-filter-groups nil
        ibuffer-marked-char ?*)

  (defun ibuffer-toggle ()
    "Toggle to show or hide all filtered buffers in `ibuffer-mode'."
    (declare (modes ibuffer-mode))
    (interactive)
    (ibuffer-update 'show-all))

  (defun ibuffer-diff-buffers (&optional switches)
    "Display difference of two marked buffers in `'ibuffer-mode'."
    (declare (modes ibuffer-mode))
    (interactive (list (diff-switches)))
    (if-let* ((bufs (ibuffer-get-marked-buffers))
              ((= (length bufs) 2)))
        (diff-buffers (car bufs) (cadr bufs) switches)
      (user-error "diff: Must be two marked buffers")))

  (setq ibuffer-maybe-show-predicates '(my/ibuffer-not-show-predicate))
  (defun my/ibuffer-not-show-predicate (buf)
    (with-current-buffer buf
      (or (string-suffix-p ".el.gz" (buffer-name))
          (string-match "^\\*" (buffer-name))
          (memq major-mode ibuffer-help-buffer-modes)
          (and (string-match "^ " (buffer-name))
	           (null buffer-file-name)))))

  (setq ibuffer-fontification-alist
        '((35 (or (string-suffix-p ".el.gz" (buffer-name))
                  (string-match "^\\*" (buffer-name))
                  (memq major-mode ibuffer-help-buffer-modes)
                  (and (string-match "^ " (buffer-name))
	                   (null buffer-file-name)))
              font-lock-doc-face)
          (100 (derived-mode-p 'dired-mode) dired-directory)))

  (with-eval-after-load 'diredfl
    (setq ibuffer-marked-face 'diredfl-flag-mark
          ibuffer-deletion-face 'diredfl-deletion
          ibuffer-title-face 'diredfl-number
          ibuffer-filter-group-name-face 'diredfl-symlink))

  (setq ibuffer-formats
        '((mark " " (name 20 20 :left :elide)
                " " modified read-only locked
                " " (size-h 7 -1 :right)
                " " (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " name)))

  (defun ibuffer-mode-setup ()
    "Setup ibuffer mode."
    (ibuffer-auto-mode)
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  :defer-config
  (leaf ibuffer-vc :require t)

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
  )

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

;;; init-elisp.el --- Emacs lisp settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my-headerise-elisp ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " ---  -*- lexical-binding: t -*-\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))

(leaf elisp-mode
  :defvar flycheck-disabled-checkers calculate-lisp-indent-last-sexp
  :hook (emacs-lisp-mode-hook . my-elisp-mode-setup)
  ;; :functions (helpful-update
  ;;             my-lisp-indent-function
  ;;             function-advices
  ;;             end-of-sexp
  ;;             add-button-to-remove-advice
  ;;             describe-function-1@advice-remove-button
  ;;             helpful-update@advice-remove-button)
  ;; :bind (:emacs-lisp-mode-map
  ;;        ("C-c C-x" . ielm)
  ;;        ("C-c C-c" . eval-defun)
  ;;        ("C-c C-b" . eval-buffer))
  :config
  (defun my-elisp-mode-setup()
    "Setting for elisp."
    (elisp-format))

  ;; (when (boundp 'elisp-flymake-byte-compile-load-path)
  ;; (add-to-list 'elisp-flymake-byte-compile-load-path load-path))

  ;; Syntax highlighting of known Elisp symbols
  (leaf highlight-defined
    :hook (emacs-lisp-mode . highlight-defined-mode)
    :init (setq highlight-defined-face-use-itself nil)))

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(leaf eldoc :blackout)

(provide 'init-elisp)
;;; init-elisp.el ends here

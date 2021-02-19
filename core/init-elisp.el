;;; init-elisp.el --- Emacs lisp settings -*- lexical-binding: t no-byte-compile: t -*-
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
      (insert ";;; " fname " ---  -*- lexical-binding: t no-byte-compile: t -*- \n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))

(leaf elisp-mode
  :bind (:emacs-lisp-mode-map
         ("C-c C-x" . ielm)
         ("C-c C-c" . eval-defun)
         ("C-c C-b" . eval-buffer)))

;; (when (boundp 'elisp-flymake-byte-compile-load-path)
;; (add-to-list 'elisp-flymake-byte-compile-load-path load-path))

;; Syntax highlighting of known Elisp symbols
(leaf highlight-defined
  :blackout
  :hook (emacs-lisp-mode . highlight-defined-mode)
  :init (setq highlight-defined-face-use-itself nil))

(provide 'init-elisp)
;;; init-elisp.el ends here

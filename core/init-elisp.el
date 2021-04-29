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

;; Syntax highlighting of known Elisp symbols
(leaf highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode)
  :init (setq highlight-defined-face-use-itself nil))

;; A better *Help* buffer
(leaf helpful
  :doc "deps: f s dash elisp-refs"
  :require t)

(provide 'init-elisp)
;;; init-elisp.el ends here

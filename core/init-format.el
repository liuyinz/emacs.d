;;; init-format.el --- format setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf prettier
  :commands (prettier-info prettier-prettify prettier-prettify-region))

(leaf format-all
  :commands format-all-buffer)

(defun my-format ()
  "Formating files."
  (interactive)
  (whitespace-cleanup)
  (cond
   ((member major-mode '(web-mode
                         css-mode
                         html-mode
                         js-mode
                         js2-mode
                         json-mode
                         sgml-mode
                         yaml-mode)) (prettier-prettify))
   ((equal major-mode 'emacs-lisp-mode) (elisp-format))
   (t (format-all-buffer))))

(leaf editorconfig :blackout)
;; :hook (after-init-hook . editorconfig-mode))

(provide 'init-format)
;;; init-format.el ends here

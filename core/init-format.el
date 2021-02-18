;;; init-format.el --- format setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf editorconfig
  :blackout t
  :hook (prog-mode-hook . editorconfig-mode))

(leaf prettier
  :commands (prettier-info prettier-prettify prettier-prettify-region))

(leaf format-all
  :doc "deps: inheritenv language-id"
  :commands format-all-buffer format-all-ensure-formatter)

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
   (t (lambda ()
        (format-all-ensure-formatter)
        (format-all-buffer)))))


(provide 'init-format)
;;; init-format.el ends here

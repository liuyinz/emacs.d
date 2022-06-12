;;; init-embark.el --- embark setup -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-06-12 20:33:40

;;; Commentary:

;;; Code:

(leaf embark
  :after vertico
  ;; :init (setq embark-prompter 'embark-completing-read-prompter)
  :bind
  (:embark-general-map
   ((kbd "C-c C-a") . marginalia-cycle))
  (:vertico-map
   ((kbd "C-l") . embark-act)
   ((kbd "C-c C-o") . embark-export)
   ((kbd "C-c C-a") . marginalia-cycle))
  :defer-config
  ;; HACK Open source code of `symbol' in other window
  (advice-add 'embark-find-definition :before #'open-in-other-window))

(leaf embark-consult
  :after embark consult)

(provide 'init-embark)
;;; init-embark.el ends here

;;; init-spell.el --- Setup spelling -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-12 02:07:45

;;; Commentary:

;;; Code:

(leaf ispell
  :init
  (when (executable-find "aspell")
    (setq ispell-program-name (executable-find "aspell"))
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))
  ;; (setq ispell-personal-dictionary )
  )

(leaf flyspell
  :hook ((prog-mode-hook . flyspell-prog-mode))
  :init
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil
        flyspell-use-meta-tab nil
        flyspell-highlight-flag nil
        flyspell-highlight-properties nil
        flyspell-persistent-highlight nil))

(leaf flyspell-correct
  :init
  (setq flyspell-correct--cr-key "'"
        flyspell-correct-highlight nil))

(leaf consult-flyspell
  :after consult
  :init
  (setq consult-flyspell-select-function (lambda ()
                                           (flyspell-correct-at-point)
                                           (consult-flyspell))))

(provide 'init-spell)
;;; init-spell.el ends here

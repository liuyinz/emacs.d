;;; init-spell.el --- Setup spelling -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-12 02:07:45

;;; Commentary:

;;; Code:

;; REQUIRE brew install enchant aspell
(leaf jinx
  :hook ((text-mode-hook prog-mode-hook conf-mode-hook markdown-mode) . jinx-mode))

;; (leaf ispell
;;   :init
;;   (when (executable-find "aspell")
;;     (setq ispell-program-name (executable-find "aspell"))
;;     (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--camel-case"))
;;     )

;; (setq ispell-personal-dictionary )
;; (when (executable-find "hunspell")
;;   (setq ispell-program-name (executable-find "hunspell"))
;;   (setq ispell-local-dictionary "en_US")
;;   (setq ispell-local-dictionary-alist
;;         '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']"
;;            nil
;;            ("-d" "en_US" "zh_CN") nil utf-8)))
;;   (when (boundp 'ispell-hunspell-dictionary-alist)
;;     (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))
;; )

;; (leaf flyspell
;;   :hook ((prog-mode-hook . flyspell-prog-mode))
;;   :init
;;   (setq flyspell-issue-message-flag nil
;;         flyspell-issue-welcome-flag nil
;;         flyspell-use-meta-tab nil
;;         flyspell-highlight-flag nil
;;         flyspell-highlight-properties nil
;;         flyspell-persistent-highlight nil))
;; 
;; (leaf flyspell-correct
;;   :init
;;   (setq flyspell-correct--cr-key "'"
;;         flyspell-correct-highlight nil))
;; 
;; (leaf consult-flyspell
;;   :after consult
;;   :init
;;   (setq consult-flyspell-select-function (lambda ()
;;                                            (flyspell-correct-at-point)
;;                                            (consult-flyspell))))

(provide 'init-spell)
;;; init-spell.el ends here

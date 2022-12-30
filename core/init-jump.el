;;; init-jump.el --- Jumping tool -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-22 17:43:48

;;; Commentary:

;;; Code:

(leaf binky-mode
  :hook (after-init-hook . binky-mode)
  :init
  (setq binky-mark-auto '(?1 ?2 ?3 ?4 ?5 ?6 ?7)
        binky-mark-back ?'
        binky-preview-show-header t
        binky-preview-auto-first nil
        binky-preview-delay 0.3
        binky-margin-string "\x2630"
        binky-margin-side 'right)
  :defer-config
  (binky-margin-mode)
  (appendq! binky-exclude-regexps
            '("\\`magit.*\\'")))

(leaf xref
  :init
  (when emacs/>=28.1p
    (setq xref-search-program #'ripgrep
          xref-show-xrefs-function 'xref-show-definitions-completing-read
          xref-show-definitions-function 'xref-show-definitions-completing-read)))

(leaf goto-addr
  :hook ((after-init-hook . global-goto-address-mode)
         (prog-mode-hook . goto-address-prog-mode)))

(leaf webjump
  :init
  (setq webjump-sites
        '(;; Internet search engines.
          ("StackOverFlow" . [simple-query "stackoverflow.com"
                                           "stackoverflow.com/search?q=" ""])
          ("Google" . [simple-query "www.google.com"
                                    "www.google.com/search?q=" ""])
          ("Github" . [simple-query "www.github.com"
                                    "www.github.com/search?q=" ""])
          ("Melpa" . [simple-query "melpa.org"
                                   "melpa.org/#/?q=" ""])
          ("Baidu" . [simple-query "www.baidu.com"
                                   "www.baidu.com/s?wd=" ""])
          ("Zhihu" . [simple-query "www.zhihu.com"
                                   "www.zhihu.com/search?type=content&q=" ""])
          ("V2ex" . [simple-query "www.sov2ex.com"
                                  "www.sov2ex.com/?q=" ""])
          ("Wiki" . [simple-query "wikipedia.org"
                                  "wikipedia.org/wiki/" ""])))
  :defer-config
  ;; HACK support visual selection texts in webjump()
  (defun ad/webjump-read-string-enable-visual (prompt)
    "Patch for visual selection avaible"
    (let* ((region-text (if (use-region-p)
                            (buffer-substring-no-properties
                             (region-beginning)
                             (region-end)) nil))
           (input (read-string (concat prompt ": ") region-text)))
      (if (webjump-null-or-blank-string-p input) nil input)))
  (advice-add 'webjump-read-string :override #'ad/webjump-read-string-enable-visual))

(leaf avy
  :hook (after-init-hook . avy-setup-default)
  :defer-config
  (setq avy-all-windows t
        avy-all-windows-alt t
        avy-background t
        avy-style 'at-full
        avy-keys '(?a ?s ?d ?f ?h ?j ?k ?l ?q ?u ?w ?i ?e ?o))
  (setq avy-orders-alist
        '((avy-goto-char . avy-order-closest)
          (avy-goto-word-0 . avy-order-closest)
          (avy-goto-paren . avy-order-closest)))

  ;; SEE https://github.com/abo-abo/avy/wiki/custom-commands#jumping-to-an-open-paren
  ;; https://stackoverflow.com/a/50063226/13194984
  (defun avy-goto-paren ()
    "Jump to a paren start or end."
    (interactive)
    (let ((avy-command this-command))
      (avy-jump "[][(){}]")))

  ;; Pinyin support
  (leaf ace-pinyin
    :init (setq ace-pinyin-simplified-chinese-only-p nil)
    :defer-config (ace-pinyin-global-mode +1))

  (leaf ace-link
    :require t
    :config
    (ace-link-setup-default)
    (global-set-key (kbd "M-o") 'ace-link-addr)))

(provide 'init-jump)
;;; init-jump.el ends here

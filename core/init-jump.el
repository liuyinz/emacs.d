;;; init-jump.el --- Jumping tool -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-22 17:43:48

;;; Commentary:

;;; Code:

(leaf binky
  :hook (after-init-hook . binky-mode)
  :init
  (setq binky-recent-sort-by 'frequency
        ;; binky-preview-show-header t
        ;; binky-preview-side 'right
        binky-preview-in-groups t
        ;; binky-preview-delay 0.3
        binky-margin-string "\x2630"
        binky-indicator-side 'right
        binky-command-prefix nil)
  (setq binky-marks
        `((back . ?')
          (quit . ?q)
          (recent . (?1 ?2 ?3 ?4 ?5))
          (manual . ,(remove ?q (number-sequence ?a ?z)))))
  (setq binky-preview-column
        '((mark    0.03  4)
          (name    0.20  15)
          (line    nil   nil)
          (project 0.14  nil)
          (context 0     nil)))
  :defer-config
  (appendq! binky-exclude-regexps '("\\`magit.*\\'"))
  (leaf binky-margin
    :require t
    :config
    (binky-margin-mode)))

(leaf xref
  :init
  (setq xref-search-program #'ripgrep
        xref-show-xrefs-function 'xref-show-definitions-completing-read
        xref-show-definitions-function 'xref-show-definitions-completing-read))

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

(provide 'init-jump)
;;; init-jump.el ends here

;;; init-pair.el --- Setup pair editing -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-22 17:28:22

;;; Commentary:

;;; Code:

;; disable show-paren-mode by default
(leaf paren
  :hook (after-init-hook . (lambda () (show-paren-mode -1)))
  :init
  (setq show-paren-style 'parenthesis
        show-paren-context-when-offscreen 'overlay))

(leaf elec-pair
  :hook (after-init-hook . electric-pair-mode)
  :defer-config
  ;; SEE https://emacs-china.org/t/html-electric-pair-mode-js/13904/11?u=cheunghsu
  (defvar electric-pair-extra-inhibit-mode-chars-alist
    '((t . nil))
    "A list of major-mode and inhibit chars.
Each element is in the form of (MODE . (CHAR/CHAR-STRING/CHAR-FUNCTION ...)).
MODE
    A mode, or t for all modes.
CHAR
    A character to match the input. for example:
        ?\{
CHAR-STRING
    A pair of character and string, the character to match the input,
    the string for ‘looking-back’. for example:

        (?\{ . \":{\")
CHAR-FUNCTION
    A pair of character and function, the character to match the input,
    the function accept the input character as parameter. for example:
        (?\{ . (lambda (_c)
                 (eq ?: (char-before (1- (point))))))")
  (defun electric-pair-extra-inhibit (c)
    (let ((alist
           (append
            (assoc-default major-mode electric-pair-extra-inhibit-mode-chars-alist)
            (assoc-default t          electric-pair-extra-inhibit-mode-chars-alist))))
      (or (cl-member c
                     alist
                     :test
                     (lambda (c it)
                       (cond
                        ((characterp it) (equal c it))
                        ((and (consp it) (equal c (car it)))
                         (cond ((stringp   (cdr it)) (looking-back (cdr it) 1))
                               ((functionp (cdr it)) (funcall (cdr it) c)))))))
          (electric-pair-default-inhibit c))))
  (setq electric-pair-inhibit-predicate #'electric-pair-extra-inhibit)

  (appendq! electric-pair-extra-inhibit-mode-chars-alist
            '((js-mode . (?<))
              (js2-mode . (?<))
              (org-mode . (?<))))
  )

(leaf isolate
  :require t
  :defer-config
  (add-hook 'isolate-add-mode-hook #'my/meow-motion-temporary))

(provide 'init-pair)
;;; init-pair.el ends here

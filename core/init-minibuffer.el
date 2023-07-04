;;; init-minibuffer.el --- minibuffer sets -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; SEE https://github.com/purcell/emacs.d/blob/master/lisp/init-minibuffer.el

(leaf vertico
  :hook (after-init-hook . vertico-mode)
  :init
  (setq vertico-cycle t
        vertico-count 15
        resize-mini-windows t)
  :bind
  ((kbd "C-c C-r") . vertico-repeat)
  (:vertico-map
   ((kbd "RET") . vertico-directory-enter)
   ((kbd "DEL") . vertico-directory-delete-char))

  :defer-config

  (leaf vertico-multiform
    :require t
    :config
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid (vertico-grid-annotate . 20)))
    (vertico-multiform-mode 1))

  ;; SEE https://github.com/minad/vertico/wiki#input-at-bottom-of-completion-list
  (defun ad/vertico--prompt-bottom (lines)
    "Display LINES in bottom."
    (move-overlay vertico--candidates-ov (point-min) (point-min))
    (unless (eq vertico-resize t)
      (setq lines (nconc (make-list (max 0 (- vertico-count (length lines))) "\n") lines)))
    (let ((string (apply #'concat lines)))
      (add-face-text-property 0 (length string) 'default 'append string)
      (overlay-put vertico--candidates-ov 'before-string string)
      (overlay-put vertico--candidates-ov 'after-string nil))
    (vertico--resize-window (length lines)))
  (advice-add 'vertico--display-candidates :override #'ad/vertico--prompt-bottom)

  ;; SEE https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (defun ad/vertico-customize-candidate (orig cand prefix suffix index _start)
    (setq cand (funcall orig cand prefix suffix index _start))
    (concat (if (= vertico--index index)
                (propertize "> " 'face 'font-lock-warning-face)
              "  ")
            cand))
  (advice-add 'vertico--format-candidate :around #'ad/vertico-customize-candidate)

  )

(leaf marginalia
  :hook (vertico-mode-hook . marginalia-mode)
  :defer-config
  (setq marginalia-align 'right
        marginalia-align-offset -1))

(leaf orderless
  :after vertico
  :require t
  :defer-config

  ;; SEE https://github.com/minad/consult/wiki#minads-orderless-configuration
  (setq completion-styles '(orderless basic)
        orderless-component-separator #'orderless-escapable-split-on-space)

  ;; Apply orderless style with initialism in category
  (orderless-define-completion-style my/orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-category-defaults nil
        completion-category-overrides
        '((file     (styles partial-completion))
          (command  (styles my/orderless-with-initialism))
          (variable (styles my/orderless-with-initialism))
          (symbol   (styles my/orderless-with-initialism))))

  ;; define dispatch
  (defvar my/orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?` . orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun my/orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) my/orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) my/orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))
  (setq orderless-style-dispatchers '(my/orderless-dispatch))

  ;; SEE https://github.com/cute-jumper/pinyinlib.el#pinyinlib-build-regexp-string
  (with-eval-after-load 'pinyinlib
    (defun ad/orderless-regexp-pinyin (args)
      "Patch `orderless-regexp' with pinyin surpport"
      (setf (car args) (pinyinlib-build-regexp-string (car args)))
      args)
    (advice-add 'orderless-regexp :filter-args #'ad/orderless-regexp-pinyin))

  )

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here

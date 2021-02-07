;;; init-ivy.el --- ivy settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ivy core
(leaf counsel
  :blackout ivy-mode counsel-mode
  :defun ivy-set-actions
  :hook
  (after-init-hook . ivy-mode)
  (ivy-mode-hook . counsel-mode)
  :bind
  ("C-s" . swiper-isearch)
  (:ivy-minibuffer-map
   ([escape] . minibuffer-keyboard-quit)
   ;; ("C-j" . ivy-next-line-and-call)
   ;; ("C-k" . ivy-previous-line-and-call)
   ("C-l" . ivy-dispatching-done)
   ;; ("C-u" . ivy-dispatching-call)
   )
  (:counsel-find-file-map
   ("C-h" . counsel-up-directory)
   ([backspace] . counsel-up-directory))
  (:swiper-isearch-map
   ("M-q" . swiper-query-replace)
   ("C-t" . isearch-toggle-color-rg))
  :init
  (setq enable-recursive-minibuffers nil
        ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-on-del-error-function nil
        ivy-height 15
        ivy-fixed-height-minibuffer nil
        ivy-use-selectable-prompt t
        ivy-wrap t
        ivy-initial-inputs-alist nil
        ivy-more-chars-alist '((t . 2))
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ;; ivy-display-style 'fancy
        swiper-action-recenter t)

  (setq counsel-find-file-at-point t
        counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"
        counsel-yank-pop-separator "\n────────\n")

  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command
          "rg -S --no-heading --line-number --color never %s %s")
    (when (executable-find "gls")
      (setq counsel-find-file-occur-use-find nil
            counsel-find-file-occur-cmd
            "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first")))
  :config

  (defun counsel-counsel ()
    "call command related to counsel"
    (interactive)
    (counsel-M-x "counsel- "))

  (ivy-set-actions
   'counsel-find-file
   '(("c" counsel-find-file-copy "copy")
     ("d" counsel-find-file-delete "delete")
     ("r" counsel-find-file-move "move or rename")
     ("m" counsel-find-file-mkdir-action "mkdir")
     ("s" counsel-find-file-as-root "open as root")
     ("x" counsel-find-file-extern "open external")
     ("l" find-file-literally  "open literally")
     ("w" find-file-other-window "other window")
     ("f" find-file-other-frame "other frame")))

  (ivy-set-actions
   'find-file-at-point
   '(("c" counsel-find-file-copy "copy")
     ("d" counsel-find-file-delete "delete")
     ("r" counsel-find-file-move "move or rename")
     ("m" counsel-find-file-mkdir-action "mkdir")
     ("s" counsel-find-file-as-root "open as root")
     ("x" counsel-find-file-extern "open external")
     ("l" find-file-literally  "open literally")
     ("w" find-file-other-window "other window")
     ("f" find-file-other-frame "other frame")))

  (ivy-set-actions
   'ivy-switch-buffer
   '(("k" ivy--kill-buffer-action "kill")
     ("r" ivy--rename-buffer-action "rename")
     ("w" ivy--switch-buffer-other-window-action "other window")
     ("x" counsel-open-buffer-file-externally "open externally")))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; More friendly display transformer for Ivy
  (leaf ivy-rich
    :hook (ivy-mode-hook . ivy-rich-mode)
    :init
    (setq ivy-rich-path-style 'abbrev)
    ;; For better performance
    (setq ivy-rich-parse-remote-buffer nil)
    (setq ivy-rich-parse-remote-file-path nil))

  ;; Integrate yasnippet
  (leaf ivy-yasnippet
    :bind ("C-c C-y" . ivy-yasnippet))

  (leaf ivy-prescient
    :require t
    :blackout ivy-prescient-mode
    :hook (ivy-mode-hook . ivy-prescient-mode)))

(leaf prescient
  :blackout prescient-persisit-mode
  :hook (after-init-hook . prescient-persist-mode)
  :init (setq prescient-history-length 300))

;; Select from xref candidates with Ivy
(leaf ivy-xref
  :init
  (when (boundp 'xref-show-definitions-function)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


(leaf counsel-osx-app
  :bind (:counsel-mode-map
         ("s-<f6>" . counsel-osx-app)))

;; ;; Tramp ivy interface
;; (use-package counsel-tramp
;;   :bind (:counsel-mode-map
;;          ("C-c c T" . counsel-tramp))))

(provide 'init-ivy)
;;; init-ivy.el ends here

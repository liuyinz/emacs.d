;; ivy core
(use-package counsel
  :functions ivy-set-actions
  :blackout (ivy-mode counsel-mode)
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :bind (
         ("C-s" . swiper-isearch)
         :map swiper-isearch-map
         ("C-t" . swiper-isearch-toggle)
         ("C-q" . swiper-query-replace)
         ;; ("C-t" . isearch-toggle-color-rg)
         :map isearch-mode-map
         ("C-t" . swiper-isearch-toggle)
         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)
         :map ivy-minibuffer-map
         ("C-j" . ivy-next-line-and-call)
         ("C-k" . ivy-previous-line-and-call)
         ("C-l" . ivy-dispatching-done)
         ("C-u" . ivy-dispatching-call))
  :init
  (setq enable-recursive-minibuffers t
        ivy-more-chars-alist '((t . 2))
        ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-on-del-error-function nil
        ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-use-selectable-prompt t
        ivy-wrap t
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ;; ivy-display-style 'fancy
        swiper-action-recenter t
        )

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

  ;; Integrate yasnippet
  (use-package ivy-yasnippet
    :bind ("C-c C-y" . ivy-yasnippet))

  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :init
    (when (boundp 'xref-show-definitions-function)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


  (use-package counsel-osx-app
    :bind (:map counsel-mode-map
           ("s-<f6>" . counsel-osx-app))))

;; ;; Tramp ivy interface
;; (use-package counsel-tramp
;;   :bind (:map counsel-mode-map
;;          ("C-c c T" . counsel-tramp))))

;; More friendly display transformer for Ivy
(use-package ivy-rich
  :hook ((counsel-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

(provide 'init-ivy)

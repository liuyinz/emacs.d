(use-package color-rg
  :quelpa (color-rg :fetcher github :repo "manateelazycat/color-rg"))

;; ;; Fast search tool `ripgrep'
;; (use-package rg
;;   :defines projectile-command-map
;;   :hook (after-init . rg-enable-default-bindings)
;;   :bind (:map rg-global-map
;;          ("c" . rg-dwim-current-dir)
;;          ("f" . rg-dwim-current-file)
;;          ("m" . rg-menu)
;;          :map rg-mode-map
;;          ("m" . rg-menu))
;;   :init (setq rg-group-result t
;;               rg-show-columns t)
;;   :config
;;   (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

;;   (with-eval-after-load 'projectile
;;     (defalias 'projectile-ripgrep #'rg-project)
;;     (bind-key "s R" #'rg-project projectile-command-map))

;;   (with-eval-after-load 'counsel
;;     (bind-keys
;;      :map rg-global-map
;;      ("R" . counsel-rg)
;;      ("F" . counsel-fzf))))

(provide 'init-rg)

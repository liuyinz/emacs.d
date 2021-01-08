
;; Environment
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand
  :defines exec-path-from-shell-check-startup-files
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
        exec-path-from-shell-arguments '("-l"))
  :config
  ;; Cache $PATH once for all, refer @https://github.com/manateelazycat/cache-path-from-shell/blob/master/cache-path-from-shell.el
  (defvar cache-path-from-shell-loaded-p nil)
  (defadvice exec-path-from-shell-initialize (around cache-path-from-shell-advice activate)
    (if cache-path-from-shell-loaded-p
        (message "All shell environment variables has loaded in Emacs, yow!")
      (setq cache-path-from-shell-loaded-p t)
      ad-do-it))
  (exec-path-from-shell-initialize))

;; keep ~/.emacs.d clean
(use-package no-littering
  :demand
  :defines recentf-exclude
  :functions (no-littering-expand-etc-file-name no-littering-expand-var-file-name)
  :init
 (setq no-littering-etc-directory (expand-file-name "etc/" my-dir-cache)
        no-littering-var-directory (expand-file-name "var/" my-dir-cache))
  :config
  ;; exclude these in recentf
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  ;; save auto-save file if needed
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; (use-package with-proxy
;;   :init
;;   (setq with-proxy-http-server "127.0.0.1:7890"))

(provide 'init-sys) 

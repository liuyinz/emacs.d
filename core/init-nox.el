;;; init-nox.el --- setting for nox  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf nox
  :doc "deps: company-mode posframe"
  :hook ((js-mode-hook
          rust-mode-hook
          python-mode-hook
          sh-mode-hook
          c-mode-common-hook
          c-mode-hook
          c++-mode-hook) . nox-ensure)
  :init
  (setq nox-autoshutdown t)
  ;; (setq nox-python-server "pyright")
  ;; (setq nox-python-server-dir (concat my-dir-cache "/nox/mspyls"))
  )

(provide 'init-nox)
;;; init-nox.el ends here

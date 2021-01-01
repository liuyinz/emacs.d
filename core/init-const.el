(defconst my-dir-core
  (expand-file-name "core" user-emacs-directory)
  "User dir for emacs configs.")

(defconst my-dir-ext
  (expand-file-name "ext" user-emacs-directory)
  "User dir for external tools.")

(defconst my-dir-snippets
  (expand-file-name "snippets" user-emacs-directory)
  "User dir for code snippets.")

(defconst my-dir-cache
  (expand-file-name ".cache" user-emacs-directory)
  "User dir for recentf,places and so on.")

(defconst my-dir-elpa
  (expand-file-name "elpa" my-dir-cache)
  "User dir for packages from melpa")

;; ensure dir exists
(dolist (dir (mapcar #'symbol-value '(my-dir-cache
                                      my-dir-core
                                      my-dir-elpa
                                      my-dir-ext
                                      my-dir-snippets)))
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defconst my-homepage
  "https://github.com/liuyinz/.emacs.d"
  "The Github Page of mine")

(setq user-full-name "食無魚")

(setq user-mail-address "liuyinz@gmail.com")

(defvar my-proxy "127.0.0.1:4780" "Set network proxy")

(provide 'init-const)

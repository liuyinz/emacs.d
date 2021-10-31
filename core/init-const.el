;;; init-const.el --- defconst settings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst my/dir-core
  (expand-file-name "core" user-emacs-directory)
  "User dir for Emacs configs.")

(defconst my/dir-ext
  (expand-file-name "ext" user-emacs-directory)
  "User dir for external tools.")

(defconst my/dir-lib
  (expand-file-name "lib" user-emacs-directory)
  "User dir for submodules.")

(defconst my/dir-cache
  (expand-file-name ".cache" user-emacs-directory)
  "User dir for recentf,places and so on.")

(defconst user-home-page
  "https://github.com/liuyinz"
  "The Github Page of mine.")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25)
           (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

(defconst emacs/>=25.3p
  (or emacs/>=26p
      (and (= emacs-major-version 25)
           (>= emacs-minor-version 3)))
  "Emacs is 25.3 or above.")

(defconst emacs/>=28.1p
  (or emacs/>=29p
      (and (= emacs-major-version 28)
           (>= emacs-minor-version 1)))
  "Emacs is 28.1 or above.")

(provide 'init-const)
;;; init-const.el ends here

(require 'cl-lib)

(use-package dash-at-point)

; (use-package leetcode
;   :commands leetcode
;   :init
;   (setq leetcode-prefer-language "javascript"
;         leetcode-prefer-sql "mysql"
;         leetcode-save-solutions t
;         leetcode-directory "~/Documents/repo/leetcode"))

;; Search tools
;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

(provide 'init-code)

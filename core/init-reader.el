;;; init-reader.el --- summary -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2023-07-05 02:54:33

;;; Commentary:

;;; Code:

(leaf nov
  :mode ("\\.epub\\'" . nov-mode)
  ;; :hook (nov-mode-hook .)
  :defer-config
  (with-no-warnings
    ;; WORKAROUND: errors while opening `nov' files with Unicode characters
    ;; @see https://github.com/wasamasa/nov.el/issues/63
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (let* ((name (nov-content-unique-identifier-name content))
             (selector (format "package>metadata>identifier[id='%s']"
                               (regexp-quote name)))
             (id (car (esxml-node-children (esxml-query selector content)))))
        (and id (intern id))))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))
  )

(leaf elfeed
  :init
  (setq url-queue-timeout 30)
  (setq elfeed-initial-tags '(unread))
  (setq elfeed-search-filter "@6-months-ago")
  ;; (setq elfeed-show-entry-switch #'switch-to-buffer
  ;;       elfeed-show-entry-delete #'elfeed-kill-buffer)
  (setq elfeed-curl-extra-arguments '("--proxy" "socks5://127.0.0.1:7890"
                                      "--retry" "3"
                                      "--insecure"))
  (setq elfeed-feeds
        '(("https://sachachua.com/blog/feed" sacha emacs)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("http://www.masteringemacs.org/feed/" emacs)
          ("http://pragmaticemacs.com/feed/" emacs)
          ("https://oremacs.com/atom.xml" emacs)
          ("https://pinecast.com/feed/emacscast" emacs)
          ("https://emacstil.com/feed.xml" emacs)
          ("https://www.reddit.com/r/emacs.rss" reddit emacs)
          ("https://emacstalk.codeberg.page/podcast/index.xml" emacs)
          ("https://manateelazycat.github.io/feed.xml" lazycat emacs)))
  :defer-config

  (leaf elfeed-webkit
    :require t
    :init
    (advice-add 'elfeed-show-mode :after #'elfeed-webkit-toggle)
    :bind
    (:elfeed-show-mode-map
     ("%" . elfeed-webkit-toggle)))

  (defun eli/elfeed-overview ()
    "Get an overview of all feeds."
    (interactive)
    (with-current-buffer (elfeed-search-buffer)
      (elfeed-save-excursion
        (let* ((inhibit-read-only t)
               (standard-output (current-buffer)))
          (erase-buffer)
          (eli/elfeed-overview--update-list)
          (dolist (entry elfeed-search-entries)
            (funcall elfeed-search-print-entry-function entry)
            (insert "\n"))
          (setf elfeed-search-last-update (float-time))))
      (when (zerop (buffer-size))
        ;; If nothing changed, force a header line update
        (force-mode-line-update))
      (run-hooks 'elfeed-search-update-hook)))

  (defun eli/elfeed-overview--update-list ()
    "Update `elfeed-search-filter' list."
    (let* ((head (list nil))
           (tail head)
           (count 0))
      (dolist (feed elfeed-feeds)
        (let* ((lexical-binding t)
               (filter (elfeed-search-parse-filter
                        (concat "=" (or (car-safe feed)
                                        feed))))
               (func (byte-compile (elfeed-search-compile-filter filter))))
          (with-elfeed-db-visit (entry feed)
            (when (funcall func entry feed count)
              (setf (cdr tail) (list entry)
                    tail (cdr tail)
                    count (1+ count))
              (elfeed-db-return)))))
      (let ((entries (cdr head))
            (elfeed-search-sort-function
             (lambda (a b)
               (let ((a-date (elfeed-entry-date a))
                     (b-date (elfeed-entry-date b)))
                 (> a-date b-date)))))
        (setf entries (sort entries elfeed-search-sort-function))
        (setf elfeed-search-entries
              entries))))
  (push elfeed-db-directory recentf-exclude))

(provide 'init-reader)
;;; init-reader.el ends here

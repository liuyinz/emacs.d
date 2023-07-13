;;; init-undo.el --- Setup undo -*- lexical-binding: t no-byte-compile: t -*-

;; Author: liuyinz <liuyinz95@gmail.com>
;; Created: 2022-12-12 06:52:58

;;; Commentary:

;;; Code:

(leaf undo-fu-session
  :hook (after-init-hook . undo-fu-session-global-mode)
  :init
  (setq undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(leaf vundo
  :init
  (setq vundo-window-max-height 5)
  :bind
  ("C-c u" . vundo)
  (:vundo-mode-map
   :package vundo
   ("l" . vundo-forward)
   ("h" . vundo-backward)
   ("j" . vundo-next)
   ("k" . vundo-previous)))

(leaf undo-hl
  :hook ((text-mode-hook prog-mode-hook) . undo-hl-mode)
  :defer-config
  (appendq! undo-hl-undo-commands '(meow-undo meow-redo))
  (setq undo-hl-flash-duration 0.15))

(leaf simple
  :init
  (setq undo-no-redo t)

  ;; jump after inserted text after undo-redo
  (advice-add #'primitive-undo :override #'my/primitive-undo)
  (defun my/primitive-undo (n list)
    "Undo N records from the front of the list LIST.
Return what remains of the list."

    ;; This is a good feature, but would make undo-start
    ;; unable to do what is expected.
    ;;(when (null (car (list)))
    ;;  ;; If the head of the list is a boundary, it is the boundary
    ;;  ;; preceding this command.  Get rid of it and don't count it.
    ;;  (setq list (cdr list))))

    (let ((arg n)
          ;; In a writable buffer, enable undoing read-only text that is
          ;; so because of text properties.
          (inhibit-read-only t)
          ;; We use oldlist only to check for EQ.  ++kfs
          (oldlist buffer-undo-list)
          (did-apply nil)
          (next nil))
      (while (> arg 0)
        (while (setq next (pop list))     ;Exit inner loop at undo boundary.
          ;; Handle an integer by setting point to that value.
          (pcase next
            ((pred integerp) (goto-char next))
            ;; Element (t . TIME) records previous modtime.
            ;; Preserve any flag of NONEXISTENT_MODTIME_NSECS or
            ;; UNKNOWN_MODTIME_NSECS.
            (`(t . ,time)
             ;; If this records an obsolete save
             ;; (not matching the actual disk file)
             ;; then don't mark unmodified.
             (let ((visited-file-time (visited-file-modtime)))
               ;; Indirect buffers don't have a visited file, so their
               ;; file-modtime can be bogus.  In that case, use the
               ;; modtime of the base buffer instead.
               (if (and (numberp visited-file-time)
                        (= visited-file-time 0)
                        (buffer-base-buffer))
                   (setq visited-file-time
                         (with-current-buffer (buffer-base-buffer)
                           (visited-file-modtime))))
	           (when (time-equal-p time visited-file-time)
                 (unlock-buffer)
                 (set-buffer-modified-p nil))))
            ;; Element (nil PROP VAL BEG . END) is property change.
            (`(nil . ,(or `(,prop ,val ,beg . ,end) pcase--dontcare))
             (when (or (> (point-min) beg) (< (point-max) end))
               (error "Changes to be undone are outside visible portion of buffer"))
             (put-text-property beg end prop val))
            ;; Element (BEG . END) means range was inserted.
            (`(,(and beg (pred integerp)) . ,(and end (pred integerp)))
             ;; (and `(,beg . ,end) `(,(pred integerp) . ,(pred integerp)))
             ;; Ideally: `(,(pred integerp beg) . ,(pred integerp end))
             (when (or (> (point-min) beg) (< (point-max) end))
               (error "Changes to be undone are outside visible portion of buffer"))
             ;; Set point first thing, so that undoing this undo
             ;; does not send point back to where it is now.
             (goto-char beg)
             (delete-region beg end))
            ;; Element (apply FUN . ARGS) means call FUN to undo.
            (`(apply . ,fun-args)
             (let ((currbuff (current-buffer)))
               (if (integerp (car fun-args))
                   ;; Long format: (apply DELTA START END FUN . ARGS).
                   (pcase-let* ((`(,delta ,start ,end ,fun . ,args) fun-args)
                                (start-mark (copy-marker start nil))
                                (end-mark (copy-marker end t)))
                     (when (or (> (point-min) start) (< (point-max) end))
                       (error "Changes to be undone are outside visible portion of buffer"))
                     (apply fun args) ;; Use `save-current-buffer'?
                     ;; Check that the function did what the entry
                     ;; said it would do.
                     (unless (and (= start start-mark)
                                  (= (+ delta end) end-mark))
                       (error "Changes to be undone by function different from announced"))
                     (set-marker start-mark nil)
                     (set-marker end-mark nil))
                 (apply fun-args))
               (unless (eq currbuff (current-buffer))
                 (error "Undo function switched buffer"))
               (setq did-apply t)))
            ;; Element (STRING . POS) means STRING was deleted.
            (`(,(and string (pred stringp)) . ,(and pos (pred integerp)))
             (let ((valid-marker-adjustments nil)
                   (apos (abs pos)))
               (when (or (< apos (point-min)) (> apos (point-max)))
                 (error "Changes to be undone are outside visible portion of buffer"))
               ;; Check that marker adjustments which were recorded
               ;; with the (STRING . POS) record are still valid, ie
               ;; the markers haven't moved.  We check their validity
               ;; before reinserting the string so as we don't need to
               ;; mind marker insertion-type.
               (while (and (markerp (car-safe (car list)))
                           (integerp (cdr-safe (car list))))
                 (let* ((marker-adj (pop list))
                        (m (car marker-adj)))
                   (and (eq (marker-buffer m) (current-buffer))
                        (= apos m)
                        (push marker-adj valid-marker-adjustments))))
               ;; Insert string and adjust point
               (if (< pos 0)
                   (progn
                     (goto-char (- pos))
                     (insert string))
                 (goto-char pos)
                 (insert string)
                 ;; HACK jump after inserted text
                 (goto-char (+ pos (length string))))
               ;; Adjust the valid marker adjustments
               (dolist (adj valid-marker-adjustments)
                 ;; Insert might have invalidated some of the markers
                 ;; via modification hooks.  Update only the currently
                 ;; valid ones (bug#25599).
                 (if (marker-buffer (car adj))
                     (set-marker (car adj)
                                 (- (car adj) (cdr adj)))))))
            ;; (MARKER . OFFSET) means a marker MARKER was adjusted by OFFSET.
            (`(,(and marker (pred markerp)) . ,(and offset (pred integerp)))
             (warn "Encountered %S entry in undo list with no matching (TEXT . POS) entry"
                   next)
             ;; Even though these elements are not expected in the undo
             ;; list, adjust them to be conservative for the 24.4
             ;; release.  (Bug#16818)
             (when (marker-buffer marker)
               (set-marker marker
                           (- marker offset)
                           (marker-buffer marker))))
            (_ (error "Unrecognized entry in undo list %S" next))))
        (setq arg (1- arg)))
      ;; Make sure an apply entry produces at least one undo entry,
      ;; so the test in `undo' for continuing an undo series
      ;; will work right.
      (if (and did-apply
               (eq oldlist buffer-undo-list))
          (setq buffer-undo-list
                (cons (list 'apply 'cdr nil) buffer-undo-list))))
    list)
  )

(provide 'init-undo)
;;; init-undo.el ends here

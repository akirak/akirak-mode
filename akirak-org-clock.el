;;; akirak-org-clock.el ---  -*- lexical-binding: t -*-

(require 'org-clock)

(defun akirak-org-clock--rebuild-history ()
  (let ((message-log-max nil))
    (message "Rebuilding the clock history"))
  (setq org-clock-history
        (seq-take
         (thread-last
           (org-map-entries (lambda ()
                              (let ((marker (point-marker))
                                    (time (akirak-org-clock--last-clock-time)))
                                (when time
                                  (cons time marker))))
                            nil
                            'agenda)
           (delq nil)
           (seq-sort-by #'car (lambda (x y) (not (time-less-p x y))))
           (mapcar #'cdr))
         org-clock-history-length)))

(defun akirak-org-clock--last-clock-time ()
  (save-match-data
    (let ((bound))
      (when (and (search-forward org-clock-string
                                 (org-entry-end-position) t)
                 (re-search-forward (org-re-timestamp 'inactive)
                                    (line-end-position)
                                    t))
        (org-time-string-to-time (match-string 0))))))

(defun akirak-org-clock--find-or-create-logbook ()
  "Go to the end of the log book of the entry."
  (org-back-to-heading)
  (let ((bound (org-entry-end-position)))
    (if (re-search-forward org-logbook-drawer-re bound t)
        (beginning-of-line 1)
      (forward-line)
      (if (re-search-forward org-property-drawer-re bound t)
          (insert "\n")
        (while (looking-at org-planning-line-re)
          (forward-line)))
      (insert ":LOGBOOK:\n:END:\n")
      (beginning-of-line 0)))
  (point-marker))

(defun akirak-org-clock-transfer-entries (dest)
  (let ((dest-logbook (with-current-buffer (marker-buffer dest)
                        (org-with-wide-buffer
                         (goto-char dest)
                         (akirak-org-clock--find-or-create-logbook)))))
    (let (entries)
      (save-excursion
        (save-restriction
          (widen)
          (org-back-to-heading)
          (narrow-to-region (point) (org-entry-end-position))
          (while (re-search-forward (rx-to-string `(and bol (* (any " \\t"))
                                                        ,org-clock-string
                                                        (+ (any " \\t"))))
                                    nil t)
            (beginning-of-line 1)
            (let ((start (point))
                  (end (line-beginning-position 2)))
              (push (buffer-substring-no-properties (point) end) entries)
              (delete-region (point) end)
              (goto-char start)))
          (goto-char (point-min))
          (replace-regexp (rx bol (* (any " \\t")) ":LOGBOOK:\n"
                              (* (any " \\t"))  ":END:\n")
                          "")))
      (with-current-buffer (marker-buffer dest-logbook)
        (org-with-wide-buffer
         (goto-char dest-logbook)
         (while entries
           (insert (pop entries)))
         (org-hide-drawer-all)))
      (org-back-to-heading))))

;;;###autoload
(defun akirak-org-clock-transfer-avy ()
  (interactive)
  (let ((dest (save-selected-window
                (save-excursion
                  (akirak-org-avy-heading t)
                  (point-marker)))))
    (akirak-org-clock-transfer-entries dest)))

(provide 'akirak-org-clock)
;;; akirak-org-clock.el ends here

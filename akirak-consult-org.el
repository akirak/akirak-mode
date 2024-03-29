;;; akirak-consult-org.el --- Extensions to consult-org -*- lexical-binding: t -*-

(require 'consult-org)
(require 'akirak-org-clock)

;;;###autoload
(defun consult-org-clock-goto (&optional arg)
  (interactive "P")
  (pcase arg
    (`nil (if (org-clocking-p)
              (org-clock-goto)
            (consult-org-clock-history)))
    ('(4) (consult-org-clock-history))
    ('(16) (consult-org-clock-history t))))

(defun consult-org-clock-history (&optional rebuild)
  ;; Based on `consult-org-heading'.
  "Jump to an Org heading.

  MATCH and SCOPE are as in `org-map-entries' and determine which
  entries are offered.  By default, all entries of the current
  buffer are offered."
  (interactive "P")
  (when (or rebuild (not org-clock-history))
    (akirak-org-clock--rebuild-history))
  (consult--read
   (consult--with-increased-gc (consult-org-clock--headings))
   :prompt "Go to heading: "
   :category 'consult-org-heading
   :sort nil
   :require-match t
   :history '(:input consult-org--history)
   :narrow (consult-org--narrow)
   :state (consult--jump-state)
   :lookup #'consult--lookup-candidate))

(defun consult-org-clock--headings ()
  ;; Based on `consult-org--headings'.
  (let (buffer)
    (thread-last
      org-clock-history
      (mapcar (lambda (marker)
                (when (and (markerp marker)
                           (buffer-live-p (marker-buffer marker)))
                  (unless (eq buffer (marker-buffer marker))
                    (setq buffer (marker-buffer marker)
                          org-outline-path-cache nil))
                  (org-with-point-at marker
                    marker
                    (pcase-let ((`(_ ,level ,todo ,prio . _) (org-heading-components))
                                (cand (org-format-outline-path
                                       (org-get-outline-path 'with-self 'use-cache)
                                       most-positive-fixnum
                                       (buffer-name buffer))))
                      (setq cand (concat cand (consult--tofu-encode (point))))
                      (add-text-properties 0 1
                                           `(consult--candidate
                                             ,(point-marker)
                                             consult-org--heading (,level ,todo . ,prio))
                                           cand)
                      cand)))))
      (delq nil))))

(provide 'akirak-consult-org)
;;; akirak-consult-org.el ends here

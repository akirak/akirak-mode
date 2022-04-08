;;; akirak-org-dog.el ---  -*- lexical-binding: t -*-

(require 'org-dog)
(require 'akirak-org-capture)

;; TODO Use transient instead of the Org select interface

(cl-defstruct akirak-org-dog-capture-payload title todo body)

(defun akirak-org-dog-capture--to-template (payload)
  (akirak-org-capture-make-entry-body
    (akirak-org-dog-capture-payload-title payload)
    :todo (akirak-org-dog-capture-payload-todo payload)
    :body (akirak-org-dog-capture-payload-body payload)))

(defun akirak-org-dog--goto-backlog ()
  (widen)
  (goto-char (point-min))
  (cond
   ((re-search-forward (rx bol "* Backlog"
                           (or (and (* blank) eol)
                               (and (+ blank) ":")))
                       nil t))
   ((re-search-forward org-heading-regexp nil t)
    (beginning-of-line 1)
    (insert "\n* Backlog\n")
    (forward-line -1))
   (t
    (goto-char (point-max))
    (insert "\n* Backlog"))))

;;;###autoload
(defun akirak-org-dog-capture-backlog (&optional title)
  (interactive)
  (let ((body (if title "%?" ""))
        (title (or title "%?")))
    (cl-flet
        ((make-payload
           (started link)
           (make-akirak-org-dog-capture-payload
            :title title
            :todo (if started "STARTED" "TODO")
            :body (if link
                      (concat "%a\n" body)
                    body))))
      (akirak-org-capture-with-doct
       `(("Backlog"
          :keys ""
          :file ,(completing-read "Capture to backlog: "
                                  (org-dog-file-completion
                                   :class 'org-dog-facade-datetree-file)
                                  nil t)
          :function akirak-org-dog--goto-backlog
          :children
          (("Plain entry"
            :keys "b"
            :template (lambda ()
                        (akirak-org-dog-capture--to-template
                         ,(make-payload nil nil))))
           ("With a link"
            :keys "l"
            :template (lambda ()
                        (akirak-org-dog-capture--to-template
                         ,(make-payload nil t))))
           ("Immediately clock in"
            :keys "i"
            :clock-in t :clock-resume t
            :children
            (("Plain entry"
              :keys "b"
              :template (lambda ()
                          (akirak-org-dog-capture--to-template
                           ,(make-payload t nil))))
             ("With a link"
              :keys "l"
              :template (lambda ()
                          (akirak-org-dog-capture--to-template
                           ,(make-payload t t)))))))))))))

;;;###autoload
(defun akirak-org-dog-capture-datetree-dwim (&optional title)
  "Capture the current context into a datetree."
  (interactive)
  (let* ((content (when (use-region-p)
                    (string-chop-newline (buffer-substring-no-properties
                                          (region-beginning)
                                          (region-end)))))
         (template (akirak-org-capture-make-entry-body
                     (or title "%?")
                     :body
                     (concat (if title "%?" "")
                             (if content
                                 (cond
                                  ((derived-mode-p 'prog-mode)
                                   (concat "\n#+begin_src "
                                           (string-remove-prefix "-mode" major-mode)
                                           "\n" content
                                           "\n#+end_src"))
                                  (t
                                   (concat "\n#+begin_quote\n"
                                           content
                                           "\n#+end_quote")))
                               "")
                             "\n%a")))
         (file (completing-read "Capture to datetree: "
                                (org-dog-file-completion
                                 :class 'org-dog-datetree-file)
                                nil t))
         (org-capture-entry (car (doct
                                  `(("Datetree"
                                     :keys "x"
                                     :file ,file
                                     :function org-reverse-datetree-goto-date-in-file
                                     :clock-in t :clock-resume t
                                     :template ,template))))))
    (org-capture)))

(provide 'akirak-org-dog)
;;; akirak-org-dog.el ends here

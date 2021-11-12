;;; akirak-org-capture.el --- Basic definitions for org-capture -*- lexical-binding: t -*-

(defcustom akirak-org-capture-default-drawer
  ":PROPERTIES:
:CREATED_TIME: %U
:END:
"
  "String appended to the body of `org-capture' by default."
  :type 'string)

(cl-defun akirak-org-capture-make-entry-body (headline &key
                                                       todo tags
                                                       (drawer akirak-org-capture-default-drawer)
                                                       (body t))
  "Build the template body of a capture template."
  (declare (indent 0))
  (concat "* " (if todo
                   (concat (if (stringp todo)
                               todo
                             "%?") " ")
                 "")
          headline
          (pcase tags
            (`nil "")
            ((pred stringp) (format " :%s:" tags))
            ((pred listp) (format " :%s:" (string-join tags ":")))
            (`all " %^G")
            (t " %^g"))
          "\n"
          (or drawer akirak-org-capture-default-drawer "")
          (pcase body
            (`nil "")
            ((pred stringp) body)
            (t "%?"))))

(defun akirak-org-capture-make-link-entry-body (url)
  (akirak-org-capture-make-entry-body
    (org-link-make-string url
                          (akirak-readable-url-title url))
    :body t))

(provide 'akirak-org-capture)
;;; akirak-org-capture.el ends here

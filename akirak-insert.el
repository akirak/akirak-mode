;;; akirak-insert.el ---  -*- lexical-binding: t -*-

(require 'akirak-readable)
(require 'akirak-clipboard)

(declare-function org-link-make-string "ol")

;;;###autoload
(defun akirak-insert-url-dwim (url)
  "Insert URL in a format depending on the major mode."
  (interactive (list (akirak-clipboard-complete-url "URL to insert: ")))
  (cond
   ((derived-mode-p 'org-mode)
    (insert (org-link-make-string url (akirak-readable-url-title url))))
   ((derived-mode-p 'markdown-mode)
    (akirak-insert-url-as-markdown url))
   (t (insert url))))

;;;###autoload
(defun akirak-insert-url-as-markdown (url)
  "Insert URL in the markdown format."
  (interactive (list (akirak-clipboard-complete-url "URL to insert: ")))
  (insert (format "[%s](%s)"
                  (or (akirak-readable-url-title url)
                      (read-string "Title for the URL: "))
                  url)))

(provide 'akirak-insert)
;;; akirak-insert.el ends here

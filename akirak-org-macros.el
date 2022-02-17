;;; akirak-org-macros.el ---  -*- lexical-binding: t -*-

(declare-function org-reverse-datetree-refile-to-file "ext:org-reverse-datetree")
(declare-function org-starter-locate-file "ext:org-starter")
(declare-function akirak-org-journal-ensure-group "akirak-org-journal")

(defgroup akirak-org-macros nil
  ""
  :prefix "akirak-org-")

(defmacro akirak-org-def-refile-fn (filename)
  "Define a function that refiles the current entry to FILENAME."
  `(defun ,(intern (concat "akirak-org-refile-to-" (file-name-base filename))) (arg)
     ,(format "Refile the current entry to %s." filename)
     (interactive "P")
     (org-reverse-datetree-refile-to-file
      (org-starter-locate-file ,filename nil t) nil
      :ask-always arg
      :prefer '("CREATED_TIME" "CREATED_AT" "CLOSED"))))

(cl-defmacro akirak-org-journal-group-target (heading &key tags todo)
  "Return a function that ensures a journal group.

For HEADING, TAGS, and TODO, see `akirak-org-journal-ensure-group'."
  (declare (indent 1))
  `(lambda () (akirak-org-journal-ensure-group ,heading
                :tags ,tags)))

(provide 'akirak-org-macros)
;;; akirak-org-macros.el ends here

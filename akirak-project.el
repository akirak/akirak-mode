;;; akirak-project.el --- Extra functions for projects -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'project)

;;;###autoload
(defun akirak-project-import-from-magit ()
  "Add projects from `magit-repository-directories'."
  (interactive)
  (require 'magit-repos)
  ;; FIXME: Don't depend on private APIs of project.el.
  (project--ensure-read-project-list)
  (let ((n 0))
    (dolist (dir (magit-list-repos))
      (let ((dir (abbreviate-file-name dir)))
        (unless (assoc dir project--list)
          (add-to-list 'project--list (list dir) 'append)
          (cl-incf n))))
    (when (> n 0)
      (message "Added %d projects" n))))

(provide 'akirak-project)
;;; akirak-project.el ends here

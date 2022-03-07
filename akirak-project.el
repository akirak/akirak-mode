;;; akirak-project.el --- Extra functions for projects -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'embark)

(declare-function github-linguist-lookup "ext:github-linguist")
(declare-function github-linguist-update-projects "ext:github-linguist")

;;;###autoload
(defun akirak-project-rescan ()
  (interactive)
  (akirak-project-import-from-magit)
  (akirak-project-maintain-list)
  (when (fboundp 'github-linguist-update-projects)
    (github-linguist-update-projects)))

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
      (project--write-project-list)
      (message "Added %d projects" n))))

;;;###autoload
(defun akirak-project-maintain-list ()
  "Update paths in the project list to conform to the policy."
  (interactive)
  (project--ensure-read-project-list)
  (let (modified)
    (dolist (cell project--list)
      (let ((path (car cell)))
        (cond
         ((not (file-directory-p path))
          (delq cell project--list)
          (message "Dropped project %s" path)
          (setq modified t))
         ((file-name-absolute-p path)
          (let (abbr (abbreviate-file-name path))
            (unless (equal abbr path)
              (setcar cell abbr)
              (setq modified t)))))))
    (when modified
      (project--write-project-list))))

;;;###autoload
(defun akirak-project-switch (dir)
  "Switch to a project at DIR.

This is an alternative to `project-switch-project' which does not
display alternative actions."
  (interactive (list (akirak-prompt-project-root
                      "Switch to a project: ")))
  (if (file-directory-p (expand-file-name ".git" dir))
      (magit-status dir)
    (dired dir)))

(defun akirak-prompt-project-root (prompt)
  "Select a project root with a PROMPT string."
  (completing-read prompt (akirak-project-root-completions
                           (project-known-project-roots))
                   nil t))

;; Based on `project--file-completion-table' from project.el 0.8.1, but with a
;; different category.
(defun akirak-project-root-completions (roots)
  "Return a completion table for project ROOTS."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata . ((category . project-root)))
      (complete-with-action action roots string pred))))

;;;###autoload
(defun akirak-project-root-annotator (root)
  (when-let (language-alist (github-linguist-lookup root))
    (cl-labels
        ((dim (str) (propertize str 'face 'font-lock-comment-face))
         (propertize-name (str) (propertize str 'face 'marginalia-string)) )
      (concat (dim " (")
              (mapconcat #'propertize-name
                         (thread-last language-alist
                           (seq-take-while (pcase-lambda (`(,_language . ,percent))
                                             (> percent 30.0)))
                           (mapcar #'car))
                         (dim ", "))
              (dim ")")))))

(provide 'akirak-project)
;;; akirak-project.el ends here

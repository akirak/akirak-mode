;;; akirak-project.el ---  -*- lexical-binding: t -*-

(require 'f)
(require 'dash)
(require 'cl-lib)

(declare-function magit-repository-directories "ext:magit-repos")

(defcustom akirak-project-group-function #'akirak-project-group-magit-repos
  "Function that returns a list of parent directories of projects."
  :type 'function)

;;;###autoload
(defun akirak-project-group-list ()
  "Call `akirak-project-group-function'."
  (funcall akirak-project-group-function))

(defun akirak-project-group-magit-repos ()
  "Return the parent directories of `magit-list-repos'."
  (require 'magit-repos)
  (cl-labels
      ((go (level parent)
           (if (= level 0)
               (list parent)
             (->> (f-directories parent)
                  (--map (go (1- level) it))
                  (-flatten-n 1)))))
    (->> magit-repository-directories
         (-map (pcase-lambda (`(,root . ,level))
                 (when (and (> level 0)
                            (f-directory-p root))
                   (go (1- level) root))))
         (-flatten-n 1)
         (-map #'f-slash)
         (-map #'f-short))))

;;;###autoload
(defun akirak-project-group-current ()
  "Return the current project group directory, if any."
  (when-let* ((parent (-some->> (project-current)
                        (project-root)
                        (f-parent)))
              (parents (akirak-project-group-list)))
    (cl-find parent parents :test #'file-equal-p)))

(provide 'akirak-project)
;;; akirak-project.el ends here

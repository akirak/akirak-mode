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

(defun akirak-project-discover-toplevels ()
  "Discover projects directly under the home directory."
  (project-remember-projects-under "~/"))

(defun akirak-project-discover-archives ()
  "Discover projects under ~/archives/oss/."
  (let ((oss-root "~/archives/oss/"))
    (when (file-exists-p oss-root)
      (project-remember-projects-under (file-truename oss-root)
                                       'recursive))))

(defun akirak-project-discover-workspaces ()
  "Discover projects under ~/work/."
  (pcase-dolist (`(,path ,target . ,_)
                 (directory-files-and-attributes
                  "~/work/" t (rx bol (not (any "."))) t))
    (pcase target
      (`nil)
      (`t
       (project-remember-projects-under path t))
      ((pred stringp)
       (project-remember-projects-under target t)))))

(defcustom akirak-project-discover-hooks
  '(akirak-project-discover-toplevels
    akirak-project-discover-archives
    akirak-project-discover-workspaces)
  "List of functions to run to discover projects."
  :type 'hook)

;;;###autoload
(defun akirak-project-discover ()
  "Remember projects in the home directory."
  (interactive)
  (run-hooks 'akirak-project-discover-hooks))

(provide 'akirak-project)
;;; akirak-project.el ends here

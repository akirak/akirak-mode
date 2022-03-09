;;; akirak-scratch.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-scratch-elisp ()
  "Create or display a scratch buffer for the current project."
  (interactive)
  (let* ((project (project-current))
         (default-directory (if project
                                (project-root project)
                              default-directory))
         (buffer-name (format "scratch-%s.el"
                              (thread-last
                                (string-remove-suffix "/" default-directory)
                                (file-name-nondirectory)))))
    (pop-to-buffer (or (get-buffer buffer-name)
                       (with-current-buffer (generate-new-buffer buffer-name)
                         (lisp-interaction-mode)
                         (current-buffer))))))

(provide 'akirak-scratch)
;;; akirak-scratch.el ends here

;;; akirak-window.el ---  -*- lexical-binding: t -*-

;;;; Predicates

(defun akirak-window-left-side-window-p (&optional window)
  (and (window-dedicated-p window)
       (not (window-in-direction 'left window))))

(defun akirak-window-right-side-window-p (&optional window)
  (and (window-dedicated-p window)
       (not (window-in-direction 'right window))))

(defun akirak-window-bottom-side-window-p (&optional window)
  (and (window-dedicated-p window)
       (not (window-in-direction 'below window))))

;;;; Window manipulation

(defun akirak-windoow-split--aggressively ()
  (cond
   ((> (akirak-window--available-width) 80)
    (split-window-horizontally))
   ((and (not (window-dedicated-p))
         (not (window-minibuffer-p))
         (window-splittable-p (selected-window)))
    (split-window-below))))

(defun akirak-window--available-width (&optional window)
  "Return the available width for a new window."
  (let ((window (or (selected-window)))
        (windows (list window))
        (leftw window)
        (rightw window))
    (while (setq leftw (window-in-direction 'left leftw))
      (push leftw windows))
    (while (setq rightw (window-in-direction 'right rightw))
      (push rightw windows))
    (-sum (-map (lambda (wnd)
                  (if (window-dedicated-p wnd)
                      0
                    (- (+ (window-width wnd)
                          ;; perfect-margin.el sets window margins
                          (pcase (window-margins wnd)
                            (`(,_) 0)
                            (`(,left . ,right) (+ left right))))
                       80)))
                windows))))

;;;###autoload
(defun akirak-window-split-and-select ()
  (interactive)
  (pcase current-prefix-arg
    ('(4)
     (progn
       (delete-window)
       (balance-windows)))
    (_
     (if-let ((window (akirak-windoow-split--aggressively)))
         (progn
           (select-window window)
           (balance-windows))
       (message "No window was created")))))

;;;###autoload
(defun akirak-window-split-vertically ()
  (interactive)
  (split-window-vertically)
  (balance-windows))

;;;###autoload
(defun akirak-window-delete-below ()
  (interactive)
  (let ((initial-window (selected-window))
        w)
    (while (setq w (window-in-direction 'below))
      (when (and (window-valid-p w)
                 (window-live-p w)
                 (not (window-minibuffer-p w)))
        (delete-window w))
      (select-window initial-window))))

;;;###autoload
(defun akirak-window-cleanup (&optional arg)
  " Clean up windows or call `abort-recursive-edit'."
  (interactive "P")
  (if arg
      (akirak-window-delete-below)
    (let (killed)
      (walk-window-tree (lambda (w)
                          (cond
                           ((member (buffer-name (window-buffer w))
                                    '("*direnv*"
                                      " *LV*"
                                      "*Warnings*"))
                            (quit-window nil w)
                            (setq killed t))
                           ((< (window-height w) 7)
                            (delete-window w)
                            (setq killed t)))))
      (unless killed
        (abort-recursive-edit)))))

(provide 'akirak-window)
;;; akirak-window.el ends here

;;; akirak-header-line.el --- Custom header line -*- lexical-binding: t -*-

(defcustom akirak-header-line-mode-blacklist
  '(git-commit-mode
    lisp-interaction-mode
    org-agenda-mode
    helpful-mode
    help-mode
    Info-mode
    eww-mode
    comint-mode
    dired-mode
    tabulated-list-mode
    vterm-mode
    magit-mode)
  ""
  :type '(repeat symbol))

(defconst akirak-header-line-value '((:eval (akirak-header-line-render))))

(defvar akirak-header-line--left-format nil)
(defvar akirak-header-line--right-format nil)
(defvar akirak-header-line--orig-format nil)

;;;###autoload
(define-minor-mode akirak-header-line-mode
  "A minor mode to display buffer information in the header line."
  :global t
  :init-value nil
  (if akirak-header-line-mode
      (akirak-header-line--enable)
    (akirak-header-line--disable)))

(cl-defun akirak-header-line--enable ()
  (setq akirak-header-line--left-format
        '("  "
          (:eval (akirak-header-line--project-and-buffer))
          "%n "
          mode-line-modified))
  (setq akirak-header-line--right-format '("(%l,%c) "))
  (add-hook 'after-change-major-mode-hook #'akirak-header-line--setup)
  (dolist (w (window-list))
    (with-current-buffer (window-buffer w)
      (unless header-line-format
        (akirak-header-line--setup)))))

(defun akirak-header-line--disable ()
  (remove-hook 'after-change-major-mode-hook #'akirak-header-line--setup)
  (dolist (buf (buffer-list))
    (let ((val (buffer-local-value 'header-line-format buf)))
      (when (equal val akirak-header-line-value)
        (with-current-buffer buf
          (setq-local header-line-format nil))))))

(defun akirak-header-line--setup ()
  (unless (or header-line-format
              (apply #'derived-mode-p akirak-header-line-mode-blacklist))
    (setq-local header-line-format akirak-header-line-value)))

(defun akirak-header-line-render ()
  (let ((l (format-mode-line akirak-header-line--left-format))
        (r (format-mode-line akirak-header-line--right-format)))
    (concat l
            (make-string (- (window-total-width)
                            (length l)
                            (length r)
                            (or (bound-and-true-p mlscroll-width-chars) 0))
                         ?\s)
            r
            (if (bound-and-true-p mlscroll-mode)
                (format-mode-line '((:eval (mlscroll-mode-line))))
              ""))))

;;;; Formatting functions

(defun akirak-header-line--project-and-buffer ()
  (let* ((base (buffer-base-buffer))
         (filename (buffer-file-name base)))
    (if filename
        (concat (if-let (project (project-current))
                    (let ((root (project-root project)))
                      (format "[%s] %s"
                              (file-name-nondirectory (string-remove-suffix "/" root))
                              (file-relative-name filename
                                                  (expand-file-name root))))
                  (file-name-nondirectory filename))
                (if base
                    " -> [indirect]%b"
                  ""))
      "%b")))

(provide 'akirak-header-line)
;;; akirak-header-line.el ends here

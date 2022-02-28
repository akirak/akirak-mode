;;; akirak-image.el ---  -*- lexical-binding: t -*-

(require 'pcase)
(require 'akirak-url)

(defvar url-http-end-of-headers)

(defconst akirak-image-process-buffer "*Akirak-Image-Process*")

(defcustom akirak-image-dir "~/resources/images/"
  ""
  :type 'directory)

(defun akirak-image-escape-filename (filename)
  (replace-regexp-in-string (rx (not (any "-_." alnum))) "-" filename))

(defun akirak-image-file-name-from-url (url-string &optional mime-type)
  (let* ((url (url-generic-parse-url url-string))
         (host (string-remove-prefix "www." (url-host url)))
         (path (cdr (split-string (url-filename url) "/")))
         (filename (car (last path)))
         (prefix (if (> (length path) 2)
                     (thread-first path
                                   (seq-take 2)
                                   (string-join "-")
                                   (concat "_"))
                   ""))
         (basename (file-name-base filename))
         (suffix (substring (sha1 url-string) 0 6))
         (extension (save-match-data
                      (pcase mime-type
                        ((or `nil
                             "binary/octet-stream")
                         (file-name-extension filename))
                        ;; TODO: Handle mime types containing +
                        ((rx bol "image/" (group (+ (any lower))))
                         (match-string 1 mime-type))
                        (_
                         (error "Did not match a mime type %s" mime-type))))))
    (concat (akirak-image-escape-filename (url-host url))
            "/"
            (akirak-image-escape-filename prefix)
            (akirak-image-escape-filename basename)
            "-"
            (akirak-image-escape-filename suffix)
            "."
            extension)))

;;;###autoload
(defun akirak-image-insert-offline-link (url)
  "Download URL and insert an image link to the local path."
  (interactive (list (akirak-url-complete "Insert an image link to URL: ")))
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must be run in org-mode"))
  (unless (and akirak-image-dir (file-directory-p akirak-image-dir))
    (error "Variable `akirak-image-dir' points to a non-existent directory %s"
           akirak-image-dir))
  (let* ((buffer (url-retrieve-synchronously url t t 5))
         (outfile (or (catch 'filename
                        (condition-case err
                            (unwind-protect
                                (with-current-buffer buffer
                                  (goto-char (point-min))
                                  (unless (url-http-parse-headers)
                                    (error "URL %s does not return a valid content"))
                                  (let* ((outfile (expand-file-name (akirak-image-file-name-from-url
                                                                     url
                                                                     url-http-content-type)
                                                                    akirak-image-dir))
                                         (outdir (file-name-directory outfile)))
                                    (unless (file-directory-p outdir)
                                      (make-directory outdir))
                                    (delete-region (point-min) (1+ url-http-end-of-headers))
                                    (setq buffer-file-name outfile)
                                    (save-buffer)
                                    (throw 'filename outfile)))
                              (kill-buffer buffer))
                          (error nil)))
                      (let* ((outfile (expand-file-name (akirak-image-file-name-from-url url)
                                                        akirak-image-dir))
                             (outdir (file-name-directory outfile)))
                        (unless (file-directory-p outdir)
                          (make-directory outdir))
                        (unless (file-exists-p outfile)
                          (let ((default-directory outdir))
                            (call-process "xh" nil akirak-image-process-buffer nil
                                          url "-o" (file-name-nondirectory outfile))))
                        outfile))))
    (when (string-suffix-p ".webp" outfile)
      (let ((new-file (concat (string-remove-suffix ".webp" outfile)
                              ".png")))
        (call-process "convert" nil akirak-image-process-buffer nil
                      outfile new-file)
        (setq outfile new-file)))
    (unless (looking-at (rx bol))
      (insert "\n"))
    (insert "#+DOWNLOADED: " url " @ " (format-time-string "%F %R:%S") "\n"
            "[[file:" (abbreviate-file-name outfile) "]]\n")))

;;;###autoload
(defun akirak-image-org-move-downloads ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be run in org-mode"))
  (let ((count 0))
    (unwind-protect
        (save-match-data
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward (rx bol (* space) "#+DOWNLOADED:" (+ space)) nil t)
             (let ((url (thing-at-point 'url)))
               (forward-line)
               (if (re-search-forward org-link-plain-re (line-end-position 1) t)
                   (let* ((filename (match-string 2))
                          (region (seq-take (match-data 0) 2))
                          (newfile (expand-file-name (akirak-image-file-name-from-url url)
                                                     akirak-image-dir))
                          (outdir (file-name-directory newfile)))
                     (cond
                      ((and (string-prefix-p (expand-file-name akirak-image-dir)
                                             (expand-file-name filename))
                            (file-exists-p filename)))
                      ((file-exists-p filename)
                       (progn
                         (unless (file-directory-p outdir)
                           (make-directory outdir))
                         (rename-file filename newfile t)
                         (apply #'delete-region region)
                         (goto-char (car region))
                         (insert (concat "file:" (abbreviate-file-name newfile)))
                         (cl-incf count)))
                      (t
                       (progn
                         (delete-region (line-beginning-position 0)
                                        (line-end-position 1))
                         (akirak-image-insert-link url)
                         (cl-incf count)))))
                 (delete-region (line-beginning-position 1)
                                (line-end-position 1))
                 (akirak-image-insert-link url)
                 (cl-incf count))))))
      (message "Replaced %d links in the file" count))))

(provide 'akirak-image)
;;; akirak-image.el ends here

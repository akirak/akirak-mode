;;; akirak-url.el --- A library for URL patterns -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/trivial-elisps

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides constants and functions related to some variations of
;; URLs.

;;; Code:

(require 'akirak-readable)
(declare-function org-link-make-string "ol")

(defgroup akirak-url nil
  "Utilities for URLs."
  :prefix "akirak"
  :group 'akirak)

;;;; Processing URLs: Matching, sanitizing, etc.

;; Taken from my clipurl.el.
;; <https://github.com/akirak/clipurl.el/blob/master/clipurl.el>
(eval-and-compile
  (defconst akirak-url--html-xalpha
    ;; TODO: Add thorough tests and fix this pattern
    (let* ((safe "-$=_@.&+")
           (extra "!*(),~")
           ;; I don't think people would want URLs containing
           ;; double/single quotes, but the spec contains them.
           ;;
           ;; (extra "!*\"'(),")
           (escape '(and "%" (char hex) (char hex))))
      `(or ,escape (char alpha digit ,safe ,extra))))

  (defconst akirak-url--html-host-pattern
    (let* ((xalpha akirak-url--html-xalpha)
           (ialpha `(and (char alpha) (* ,xalpha)))
           (hostname `(and ,ialpha (* (and "." ,ialpha))))
           (hostnumber '(and (+ (char digit))
                             (repeat 3 (and "." (+ (char digit)))))))
      `(or ,hostname ,hostnumber)))

  (defconst akirak-url-html-regexp
    (rx "http" (?  "s")
        "://"
        (eval akirak-url--html-host-pattern)
        (?  ":" (+ (char digit)))
        (?  (or (and (+ "/" (+ (eval akirak-url--html-xalpha)))
                     (?  "/"))
                "/"))
        (?  "?" (and (+ (eval akirak-url--html-xalpha))
                     (* "+" (+ (eval akirak-url--html-xalpha)))))
        (?  "#" (+ (or (+ (eval akirak-url--html-xalpha))
                       (char "/?")))))))

(defun akirak-url--match-html-string (string)
  "Return the first match of a web page url in STRING, if any."
  (save-match-data
    (when (string-match akirak-url-html-regexp string)
      (match-string 0 string))))

(defun akirak-url--clean (url)
  "Remove undesired parameters from URL, if any."
  (save-match-data
    (if-let (start (string-match (rx (group (any "?&"))
                                     (group "utm_source=" (+ (not (any "&"))))
                                     (group (? "&")))
                                 url))
        (concat (substring url 0 start)
                (if (equal (match-string 3 url) "&")
                    (concat (match-string 1 url)
                            (substring url (nth 1 (match-data))))
                  ""))
      url)))

;;;; Completing URLs

;;;###autoload
(defun akirak-url-complete (prompt &optional initial-input history)
  "Complete a url from the clipboard with PROMPT."
  (completing-read prompt
                   (akirak-url--completions (akirak-url--recent-kills))
                   nil nil initial-input history))

(defun akirak-url--completions (urls)
  "Return a completion table for URLS."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata . ((category . url)))
      (complete-with-action action urls string pred))))

;;;;; Recent URLs from the kill ring

(defcustom akirak-url-max-recent-items 5
  ""
  :type 'number)

(defun akirak-url--recent-kills ()
  (let ((i 0)
        (n 0)
        (len (length kill-ring))
        result)
    (catch 'finish
      (while (and (< i len)
                  (< n akirak-url-max-recent-items))
        (if-let (k (current-kill i t))
            (when-let (url (akirak-url--match-html-string k))
              (unless (member url result)
                (push (akirak-url--clean (substring-no-properties url))
                      result)
                (cl-incf n)))
          (throw 'finish t))
        (cl-incf i)))
    (nreverse result)))

;;;; Inserting a link to a URL

(defvar akirak-url-insert-history nil)

;;;###autoload
(defun akirak-url-insert-dwim (url)
  "Insert URL in a format depending on the major mode."
  (interactive (list (akirak-url-complete "URL to insert: " nil
                                          akirak-url-insert-history)))
  (cond
   ((derived-mode-p 'org-mode)
    (insert (org-link-make-string url (akirak-readable-url-title url))))
   ((derived-mode-p 'markdown-mode)
    (akirak-url-insert-as-markdown url))
   (t (insert url))))

;;;###autoload
(defun akirak-url-insert-as-markdown (url)
  "Insert URL in the markdown format."
  (interactive (list (akirak-clipboard-complete-url "URL to insert: ")))
  (insert (format "[%s](%s)"
                  (or (akirak-readable-url-title url)
                      (read-string "Title for the URL: "))
                  url)))

(provide 'akirak-url)
;;; akirak-url.el ends here

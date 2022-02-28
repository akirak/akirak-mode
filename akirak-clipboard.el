;;; akirak-clipboard.el --- Access to clipboard contents -*- lexical-binding: t -*-

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

;; 

;;; Code:

(require 'akirak-url)
(require 'cl-lib)
(require 'subr-x)

(defun akirak-clipboard-strings ()
  "Return a list of strings from the clipboard and the kill ring."
  (let* ((selection (funcall interprogram-paste-function))
         (selections (if (listp selection)
                         selection
                       (list selection))))
    (append selections kill-ring)))

(defun akirak-clipboard-urls ()
  "Return a list of urls from the clipboard and the kill ring."
  (cl-remove-duplicates
   (thread-last (akirak-clipboard-strings)
     (mapcar #'akirak-url-match-html-string)
     (delq nil))
   :test #'string-equal))

(defun akirak-clipboard-complete-url (prompt)
  "Complete a url from the clipboard with PROMPT."
  (completing-read prompt
                   (akirak-clipboard--url-completion
                    (akirak-clipboard-urls))))

(defun akirak-clipboard--url-completion (urls)
  "Return a completion table for URLS."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata . ((category . url)))
      (complete-with-action action urls string pred))))

(provide 'akirak-clipboard)
;;; akirak-clipboard.el ends here

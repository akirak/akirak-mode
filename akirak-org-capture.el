;;; akirak-org-capture.el --- Basic definitions for org-capture -*- lexical-binding: t -*-

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

(require 'org)
(require 'ol)
(require 'akirak-readable)

(defcustom akirak-org-capture-default-drawer
  ":PROPERTIES:
:CREATED_TIME: %U
:END:
"
  "String appended to the body of `org-capture' by default."
  :type 'string)

(cl-defun akirak-org-capture-make-entry-body (headline &key
                                                       todo tags
                                                       (drawer akirak-org-capture-default-drawer)
                                                       (body t))
  "Build the template body of a capture template."
  (declare (indent 0))
  (concat "* " (if todo
                   (concat (if (stringp todo)
                               todo
                             "%?") " ")
                 "")
          headline
          (pcase tags
            (`nil "")
            ((pred stringp) (format " :%s:" tags))
            ((pred listp) (format " :%s:" (string-join tags ":")))
            (`all " %^G")
            (t " %^g"))
          "\n"
          (or drawer akirak-org-capture-default-drawer "")
          (pcase body
            (`nil "")
            ((pred stringp) body)
            (t "%?"))))

(defun akirak-org-capture-make-link-entry-body (url)
  (akirak-org-capture-make-entry-body
    (org-link-make-string url
                          (akirak-readable-url-title url))
    :body t))

(provide 'akirak-org-capture)
;;; akirak-org-capture.el ends here

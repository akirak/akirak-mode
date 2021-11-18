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
(require 'akirak-read)

(defvar org-capture-templates)

(defgroup akirak-org-capture
  nil
  ""
  :group 'akirak
  :group 'org-capture)

(defcustom akirak-org-capture-default-drawer
  ":PROPERTIES:
:CREATED_TIME: %U
:END:
"
  "String appended to the body of `org-capture' by default."
  :type 'string)

;;;###autoload
(cl-defun akirak-org-capture-make-entry-body (headline &key
                                                       todo tags
                                                       (drawer akirak-org-capture-default-drawer)
                                                       (body t))
  "Build the template body of a capture template.

HEADLINE is a string put in the headline.

TODO specifies the todo keyword of the entry.

TAGS specifies tags of the entry and can be one of the following
values:

 - nil, which means no tag is set.

 - A string.

 - A list of strings.

 - t, which lets you pick a tag from the current buffer (%^g).

 - `all', which lets you pick a tag from all Org buffers (%^G).

If DRAWER is given, it is inserted after the headline. By
default, `akirak-org-capture-default-drawer' is inserted. If you
specify an empty string, it will be empty.

BODY is appended to the template after the drawer. It can be one
of the following values:

 - A string literal.

 - t, which means the cursor is moved to the point."
  (declare (indent 0))
  (concat "* " (if todo
                   (concat todo " ")
                 "")
          (pcase headline
            ((pred stringp) headline)
            (`(url ,url) (org-link-make-string url (akirak-readable-url-title url)))
            (`prompt "%^{headline}")
            (`t "%?")
            (_ (error "Invalid headline value: %s" headline)))
          (pcase tags
            (`nil "")
            ((pred stringp) (format " :%s:" tags))
            ((pred listp) (format " :%s:" (string-join tags ":")))
            (`all " %^G")
            (`t " %^g"))
          "\n"
          (or drawer akirak-org-capture-default-drawer "")
          (pcase body
            (`nil "")
            ((pred stringp) body)
            (`t "%?"))))

;;;###autoload
(defun akirak-org-capture-add-templates (templates)
  "Add TEMPLATES to `org-capture-templates' without duplicates."
  (declare (indent 1))
  (prog1 org-capture-templates
    (pcase-dolist (`(,key . ,args) templates)
      (if-let (cell (assoc key org-capture-templates))
          (setcdr cell args)
        (add-to-list 'org-capture-templates
                     (cons key args)
                     t)))))

(provide 'akirak-org-capture)
;;; akirak-org-capture.el ends here

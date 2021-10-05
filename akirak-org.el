;;; akirak-org.el --- Functions for Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: convenience outlines files
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

;; This is a collection of functions for Org mode.

;;; Code:

(require 'org)

(defun akirak-org-sort-with-property ()
  "Sort entries in the buffer based on the sort property."
  (interactive)
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward org-heading-regexp nil t)
     (when-let (sort (org-entry-get nil "SORT"))
       (org-sort-entries nil (car (string-to-list sort))))
     (end-of-line 1))))

(define-minor-mode akirak-org-sort-with-property-on-save-mode
  "Sort entries based on the sort property on saving."
  nil
  " OrgSortP"
  nil
  (if akirak-org-sort-with-property-on-save-mode
      (add-hook 'before-save-hook
                #'akirak-org-sort-with-property
                nil t)
    (remove-hook 'before-save-hook
                 #'akirak-org-sort-with-property
                 t)))

(provide 'akirak-org)
;;; akirak-org.el ends here

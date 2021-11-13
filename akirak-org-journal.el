;;; akirak-org-journal.el --- Utilities for org-journal -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.4") (org-journal "2.1"))
;; Keywords: outlines
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

;; This is a collection of helper libraries for Org mode.

;;; Code:

(require 'org)
(require 'org-journal)

;;;###autoload
(defun akirak-org-journal-find-location (&optional time)
  "Go to the beginning of the journal file.

This can be used for defining an `org-capture' template to create
an entry in the journal.

Optionally, you can specify TIME for pointoing to a particular
date, which is given to `org-journal-new-entry'."
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t time)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (widen)
  (re-search-backward (rx bol "* "))
  (org-show-entry))

;;;###autoload
(cl-defun akirak-org-journal-ensure-group (heading &key tags)
  "Go to a particular group in the journal.

This moves the point to a group with a HEADING. If the heading
does not , it is created.

Optionally, you can specify TAGS which is added to the heading."
  (declare (indent 1))
  (akirak-org-journal-find-location)
  (let ((end (save-excursion
               (org-end-of-subtree 'invisible))))
    (if (re-search-forward (rx-to-string `(and bol "**" (+ space) ,heading))
                           end t)
        (end-of-line)
      (goto-char end)
      (progn
        (just-one-space -1)
        (delete-horizontal-space)
        (insert "\n** " heading))
      (when tags
        (org-set-tags tags))
      (org-end-of-line))))

(cl-defmacro akirak-org-journal-group-target (heading &key tags)
  "Return a function that ensures a journal group.

For HEADING and TAGS, see `akirak-org-journal-ensure-group'."
  (declare (indent 1))
  `(lambda () (akirak-org-journal-ensure-group ,heading
                :tags ,tags)))

;;;###autoload
(defun akirak-org-journal-overview (&optional arg)
  "Go to the current date in the journal and display an overview.

If ARG is non-nil, it also updates a dynamic block immediately
after the heading, if any.

If the point is already in the designated location, it will open
`org-agenda' in another window."
  (interactive "P")
  (let ((point (point)))
    (org-journal-new-entry t)
    (re-search-backward (rx bol "* "))
    (if (equal point (point))
        (let ((org-agenda-window-setup 'other-window))
          (org-agenda nil "a"))
      (org-content 3)
      (org-show-set-visibility 'local)
      ;; I have a dynamic block for time tracking in each journal.
      ;; I want to update its contents when I review the journal.
      (when arg
        (save-excursion
          (forward-line)
          (when (looking-at org-dblock-start-re)
            (org-dblock-update)))))))

(provide 'akirak-org-journal)
;;; akirak-org-journal.el ends here

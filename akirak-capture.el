;;; akirak-capture.el --- My capture workflow -*- lexical-binding: t -*-

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

(require 'org-starter)
(require 'transient)
(require 'org-capture)

(require 'akirak-org-capture)
(require 'akirak-org-journal)
(require 'akirak-clipboard)

(declare-function org-clocking-p "org-clock")

;;;; Main

(transient-define-suffix akirak-capture-org-research ()
  :class 'transient-suffix
  :description "Start a research topic"
  (interactive)
  (let ((org-capture-entry `("" ""
                             entry
                             (function
                              ,(akirak-org-journal-group-target "Research"
                                 :tags "@research"))
                             ,(akirak-org-capture-make-entry-body
                                "%?"
                                :todo "STARTED"
                                :body nil)
                             :clock-in t
                             :clock-resume t)))
    (org-capture)))

(transient-define-suffix akirak-capture-news-session ()
  :class 'transient-suffix
  :description "Start a news session"
  (interactive)
  (let ((org-capture-entry `("" ""
                             entry
                             (function akirak-org-journal-find-location)
                             ,(akirak-org-capture-make-entry-body
                                "News"
                                :tags "@news"
                                :body t)
                             :clock-in t
                             :clock-resume t)))
    (org-capture)))

(transient-define-suffix akirak-capture-link-to-clock (url)
  :class 'transient-suffix
  :description "Add a link to the clock"
  (interactive (list (completing-read "Capture Url to clock: "
                                      (akirak-clipboard-urls))))
  (akirak-capture-entry-to-clock
   (akirak-org-capture-make-link-entry-body url)))

(transient-define-suffix akirak-capture-link-to-info (url)
  :class 'transient-suffix
  :description "Save a link to info group"
  (interactive (list (completing-read "Capture Url to clock: "
                                      (akirak-clipboard-urls))))
  (let ((org-capture-entry `("" ""
                             entry
                             (function
                              ,(akirak-org-journal-group-target "Information"))
                             ,(akirak-org-capture-make-link-entry-body url))))
    (org-capture)))

(transient-define-prefix akirak-capture-dispatch ()
  [:description
   akirak-capture--clock-description
   :if (lambda () (org-clocking-p))
   ("@u" akirak-capture-link-to-clock)]
  ["Journal"
   :if (lambda () (bound-and-true-p org-journal-dir))
   ("r" akirak-capture-org-research)
   ("n" akirak-capture-news-session)
   ("iu" akirak-capture-link-to-info)]
  ["Others"
   ;; ("r" "Region" akirak-capture-region)
   ;; ("s" "Screen" akirak-capture-screen)
   ("o" "Org Capture" org-capture)]
  (interactive)
  (transient-setup 'akirak-capture-dispatch))

;;;###autoload
(defun akirak-capture (arg)
  "Capture something.

When two universal prefixes are given as ARG, it jumps to the
recently captured location, as in `org-capture'."
  (interactive "P")
  (pcase arg
    ('(16) (org-capture '(16)))
    (_ (akirak-capture-dispatch))))

;;;; Private utility functions
(defun akirak-capture--clock-description ()
  "Return the description of the current clock."
  (if (and org-clock-marker
           (markerp org-clock-marker)
           (buffer-live-p (marker-buffer org-clock-marker)))
      (org-with-point-at org-clock-marker
        (let ((filename (file-name-nondirectory
                         (buffer-file-name
                          (org-base-buffer (current-buffer))))))
          (concat (if (org-clocking-p)
                      "Clocking: "
                    "Last clock: ")
                  (substring-no-properties
                   (org-format-outline-path
                    (org-get-outline-path t t)
                    (window-width)
                    filename
                    " > ")))))
    "No last clock"))

;;;; Public helper functions
(cl-defun akirak-capture-entry-to-clock (template &key entry-end prepend)
  "Capture an entry to the current clock.

TEMPLATE is a string.

If ENTRY-END is non-nil, the point is moved to the end of the
entry. See `org-entry-end-position'.for details.

For other options such as PREPEND, see `org-capture-templates'."
  (let ((org-capture-entry `("" ""
                             entry
                             (function
                              (lambda ()
                                (org-goto-marker-or-bmk org-clock-marker)
                                (org-back-to-heading)
                                (when ,entry-end
                                  (goto-char (org-entry-end-position)))))
                             ,template
                             :prepend ,prepend)))
    (org-capture)))

(provide 'akirak-capture)
;;; akirak-capture.el ends here

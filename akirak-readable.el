;;; akirak-readable.el --- Wrap readable -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords:
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

(require 'memoize)

(declare-function org-web-tools--get-url "ext:org-web-tools")
(declare-function org-web-tools--html-title "ext:org-web-tools")

(defun akirak-readable-url-title (url)
  (when-let (html (with-timeout (5)
                    (message "Fetching %s..." url)
                    (org-web-tools--get-url url)))
    (org-web-tools--html-title html)))

(memoize 'akirak-readable-url-title 600)

(provide 'akirak-readable)
;;; akirak-readable.el ends here

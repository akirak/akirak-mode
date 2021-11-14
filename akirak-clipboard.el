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

(defun akirak-clipboard-strings ()
  "Return a list of strings from the clipboard and the kill ring."
  (let* ((selection (funcall interprogram-paste-function))
         (selections (if (listp selection)
                         selection
                       (list selection))))
    (append selections kill-ring)))

(defun akirak-clipboard-urls ()
  "Return a list of urls from the clipboard and the kill ring."
  (cl-flet
      ((remove-dups (items)
                    (cl-remove-duplicates items :test #'string-equal)))
    (thread-last (akirak-clipboard-strings)
      (mapcar #'akirak-url-match-html-string)
      (delq nil)
      (remove-dups))))

(provide 'akirak-clipboard)
;;; akirak-clipboard.el ends here

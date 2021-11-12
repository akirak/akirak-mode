;;; akirak-avy.el --- Extra Avy functions -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (avy "0.5") (akirak-url "0.1"))
;; Keywords: convenience
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

;; This library provides extra avy functions.

;;; Code:

(require 'avy)
(require 'akirak-url)

(defun akirak-avy-html-url (&optional callback)
  "Return a URL to a web page in the visible area.

If CALLBACK is a function, it is called with the selected url."
  (save-window-excursion
    (avy-with akirak-capture-url--avy
      (pcase (avy-jump akirak-url-html-regexp)
        (`(,beg . ,end)
         (let ((url (buffer-substring-no-properties beg end)))
           (prog1 url
             (when callback
               (funcall callback url)))))))))

(provide 'akirak-avy)
;;; akirak-avy.el ends here

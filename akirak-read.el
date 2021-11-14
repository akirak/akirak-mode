;;; akirak-read.el --- Reading various objects with completion -*- lexical-binding: t -*-

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

(require 'akirak-clipboard)

(defvar akirak-read-url-history nil)

(cl-defun akirak-read-url-1 (prompt &key initial-input)
  "Read a url.

This function uses `completing-read' for getting a url from the
 user. See the documentation of the function for `PROMPT and
 INITIAL-INPUT."
  (completing-read prompt (akirak-clipboard-urls)
                   nil nil initial-input akirak-read-url-history))

(provide 'akirak-read)
;;; akirak-read.el ends here

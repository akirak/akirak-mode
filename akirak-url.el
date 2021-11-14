;;; akirak-url.el --- A library for URL patterns -*- lexical-binding: t -*-

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

;; This library provides constants and functions related to some variations of
;; URLs.

;;; Code:

(defgroup akirak-url nil
  "Utilities for URLs."
  :prefix "akirak"
  :group 'akirak)

;; Taken from my clipurl.el.
;; <https://github.com/akirak/clipurl.el/blob/master/clipurl.el>
(eval-and-compile
  (defconst akirak-url--html-xalpha
    ;; TODO: Add thorough tests and fix this pattern
    (let* ((safe "-$=_@.&+")
           (extra "!*(),~")
           ;; I don't think people would want URLs containing
           ;; double/single quotes, but the spec contains them.
           ;;
           ;; (extra "!*\"'(),")
           (escape '(and "%" (char hex) (char hex))))
      `(or ,escape (char alpha digit ,safe ,extra))))

  (defconst akirak-url--html-host-pattern
    (let* ((xalpha akirak-url--html-xalpha)
           (ialpha `(and (char alpha) (* ,xalpha)))
           (hostname `(and ,ialpha (* (and "." ,ialpha))))
           (hostnumber '(and (+ (char digit))
                             (repeat 3 (and "." (+ (char digit)))))))
      `(or ,hostname ,hostnumber)))

  (defconst akirak-url-html-regexp
    (rx "http" (?  "s")
        "://"
        (eval akirak-url--html-host-pattern)
        (?  ":" (+ (char digit)))
        (?  (or (and (+ "/" (+ (eval akirak-url--html-xalpha)))
                     (?  "/"))
                "/"))
        (?  "?" (and (+ (eval akirak-url--html-xalpha))
                     (* "+" (+ (eval akirak-url--html-xalpha)))))
        (?  "#" (+ (or (+ (eval akirak-url--html-xalpha))
                       (char "/?")))))))

(defun akirak-url-match-html-string (string)
  "Return the first match of a web page url in STRING, if any."
  (save-match-data
    (when (string-match akirak-url-html-regexp string)
      (match-string 0 string))))

(provide 'akirak-url)
;;; akirak-url.el ends here

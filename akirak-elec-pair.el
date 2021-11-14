;;; akirak-elec-pair.el --- Convenient commands based on elec-pair.el -*- lexical-binding: t -*-

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

(require 'elec-pair)

;;;###autoload
(defun akirak-elec-pair-replace (c)
  "Replace a pair of parentheses/brackets of C around the point."
  (interactive "cTarget paren: ")
  (let ((regexp (regexp-quote (char-to-string c))))
    (let* ((start (save-excursion
                    (unless (looking-at regexp)
                      (re-search-backward regexp))))
           (end (save-excursion
                  (goto-char start)
                  (forward-sexp)
                  (point)))
           (overlay (make-overlay start end))
           (replacement-char (progn
                               (overlay-put overlay 'face 'highlight)
                               (read-char "New paren: ")))
           (replacement-close-char (or (nth 1 (electric-pair-syntax-info replacement-char))
                                       (alist-get replacement-char
                                                  '((?\{ . ?\})))
                                       (error "Cannot find syntax info for %c"
                                              replacement-char))))
      (save-excursion
        (delete-overlay overlay)
        (goto-char start)
        (delete-char 1)
        (insert-char replacement-char)
        (goto-char end)
        (backward-delete-char 1)
        (insert-char replacement-close-char)))))

;;;###autoload
(defun akirak-elec-pair-delete (c)
  "Delete a pair of parentheses/brackets of C around the point."
  (interactive "cTarget paren: ")
  (let ((regexp (regexp-quote (char-to-string c))))
    (let* ((start (save-excursion
                    (unless (looking-at regexp)
                      (re-search-backward regexp))))
           (end (save-excursion
                  (goto-char start)
                  (forward-sexp)
                  (point))))
      (save-excursion
        (goto-char start)
        (delete-char 1)
        (goto-char (1- end))
        (backward-delete-char 1)))))

(provide 'akirak-elec-pair)
;;; akirak-elec-pair.el ends here

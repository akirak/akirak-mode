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

(defcustom akirak-elec-pair-special-chars
  (string-to-list "%$#")
  "List of characters indicating an orientation of interpolation."
  :type '(repeat character))

(defun akirak-elec-pair--new-bracket-pair ()
  "Get the new bracket pairs from the user."
  (let* ((open (read-char (format "Open paren (or %s for interpolation): "
                                  (mapconcat #'char-to-string
                                             akirak-elec-pair-special-chars
                                             ""))))
         (specialp (memq open akirak-elec-pair-special-chars))
         (open-char (if specialp
                        (read-char "Open paren: ")
                      open))
         (close-char (or (nth 1 (electric-pair-syntax-info open-char))
                         (alist-get open-char '((?\{ . ?\})))
                         (error "Cannot find syntax info for %c" open-char))))
    (list open-char
          close-char
          (when specialp (char-to-string open)))))

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

;;;###autoload
(defun akirak-elec-pair-wrap ()
  "Wrap the current sexp or region with a pair."
  (interactive)
  (pcase-let* ((`(,start . ,end) (if (region-active-p)
                                     (cons (region-beginning) (region-end))
                                   (bounds-of-thing-at-point 'sexp)))
               (overlay (make-overlay start end))
               (`(,open-char ,close-char ,prefix)
                (progn
                  (overlay-put overlay 'face 'highlight)
                  (akirak-elec-pair--new-bracket-pair))))
    (save-excursion
      (delete-overlay overlay)
      (goto-char start)
      (when prefix
        (insert prefix))
      (insert-char open-char)
      (goto-char (if prefix
                     (+ (length prefix) end 1)
                   (1+ end)))
      (insert-char close-char))))

(provide 'akirak-elec-pair)
;;; akirak-elec-pair.el ends here

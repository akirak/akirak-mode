;;; akirak-org.el --- A collection of helpers for Org -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

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

;; This library provides convenience functions for `org-mode'.

;;; Code:

(require 'org)

;;;###autoload
(defun akirak-org-sort-buffer ()
  "Sort entries in the buffer according to sorting_type property values."
  (interactive)
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward (org-re-property "sorting_type") nil t)
     (let ((line (thing-at-point 'line t)))
       (if (string-match org-property-re line)
           (org-save-outline-visibility t
             (org-sort-entries nil
                               (thread-last (match-string 3 line)
                                 (string-to-list)
                                 (car))))
         (error "Property didn't match")))
     (goto-char (org-entry-end-position)))))

(define-minor-mode akirak-org-sort-buffer-mode
  "Sort entries in the buffer before save."
  nil nil nil
  (if akirak-org-sort-buffer-mode
      (add-hook 'before-save-hook #'akirak-org-sort-buffer nil t)
    (remove-hook 'before-save-hook #'akirak-org-sort-buffer t)))

;;;###autoload
(defun akirak-org-add-timestamp (&rest args)
  "Add a timestamp to the current entry.

If the current command is run with a prefix argument, prevent
from running."
  (interactive)
  (unless current-prefix-arg
    (let ((prop (cl-case this-command
                  (org-insert-heading "CREATED_TIME")
                  (akirak-org-add-timestamp (org-read-property-name))
                  (otherwise "TIMESTAMP"))))
      (org-set-property prop
                        (org-timestamp-format
                         (org-timestamp-from-time (current-time) t t)
                         (org-time-stamp-format t t))))))

;;;###autoload
(defun akirak-org-add-empty-checkbox ()
  "Add an empty check box to the current item."
  (interactive)
  (let ((checkbox-regexp (rx "[" (or "X" (optional space)) "] "))
        (item-regexp (rx bol (* space) "- ")))
    (cl-labels ((maybe-insert-checkbox
                  ()
                  (unless (looking-at checkbox-regexp)
                    (insert "[ ] "))))
      (if (region-active-p)
          (let* ((pos (point))
                 (beg (region-beginning))
                 (end (region-end))
                 (src (buffer-substring-no-properties beg end)))
            (delete-region beg end)
            (insert
             (with-temp-buffer
               (insert src)
               (goto-char (point-min))
               (while (re-search-forward item-regexp (point-max) t)
                 (maybe-insert-checkbox))
               (buffer-string)))
            (goto-char pos))
        (save-excursion
          (beginning-of-line)
          (when (re-search-forward item-regexp (line-end-position) t)
            (maybe-insert-checkbox)))))))

;;;###autoload
(defun akirak-org-yank-into-new-block (&optional arg)
  "Create a new block with the yanked text as its content.

With ARG, pick a text from the kill ring instead of the last one."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (beginning-of-line 1)
  (let ((begin (point))
        done)
    (unwind-protect
        (progn
          (if arg
              (yank-pop)
            (yank))
          ;; Select the pasted text.
          (push-mark begin)
          (setq mark-active t)
          (call-interactively #'org-insert-structure-template)
          (setq done t)
          ;; Unselect the pasted text
          (deactivate-mark)
          (let ((case-fold-search t))
            (save-excursion
              (goto-char begin)
              (when (looking-at (rx (* space) "#+begin_src" space))
                (let ((lang (thread-last
                              (akirak-complete-major-mode "Source language: ")
                              (string-remove-suffix "-mode"))))
                  (end-of-line 1)
                  (insert lang))))
            (re-search-forward (rx bol (* space) "#+end_")))
          ;; If there is whitespace at the beginning of the pasted text,
          ;; the block will have preceding space as well.
          ;;
          ;; Thus you have to re-indent the entire block to ensure
          ;; that it has no preceding space at the bol.
          (indent-region begin (point))
          (forward-line 1)
          ;; Insert an empty line.
          (unless (looking-at (rx eol))
            (insert "\n\n")
            (beginning-of-line 0)))
      (unless done
        ;; If the user has cancelled `org-insert-structure-template',
        ;; restore the previous state.
        (deactivate-mark)
        (delete-region begin (point))))))

;;;###autoload
(defun akirak-org-angle-open (&optional arg)
  "Do-what-i-mean \"<\" in `org-mode'."
  (interactive "P")
  (if (org-region-active-p)
      (let ((count (if (numberp arg)
                       arg
                     1))
            (pos (point))
            (begin (region-beginning))
            (end (region-end)))
        (goto-char begin)
        (insert (make-string count ?<))
        (goto-char (+ end count))
        (insert (make-string count ?>))
        (if (<= pos count)
            (goto-char pos)
          (goto-char (+ end (* 2 count)))))
    (save-match-data
      (cond
       ;; Insert a source block.
       ((and (looking-at (rx ">" eol))
             (looking-back (rx bol "<" (group (+ (any alnum "-"))))
                           (line-beginning-position)))
        (let* ((needle (match-string 1))
               (mode (or (cl-some (pcase-lambda (`(,pat . ,mode))
                                    (when (string-match-p pat (concat "." needle))
                                      mode))
                                  auto-mode-alist)
                         (let ((sym (intern (concat needle "-mode"))))
                           (when (and (commandp sym)
                                      (not (memq sym minor-mode-list)))
                             sym))
                         (akirak-complete-major-mode "Language: " needle))))
          (delete-region (line-beginning-position)
                         (line-end-position))
          (org-insert-structure-template
           (concat "src " (string-remove-suffix "-mode" (symbol-name mode))))
          (if arg
              (progn
                (org-end-of-line 0)
                (insert " "))
            (org-open-line 1))))
       ;; Insert a block from `org-structure-template-alist'.
       ((and (looking-at (rx eol))
             (looking-back (rx bol (any alpha))
                           (line-beginning-position)))
        (if-let (type (cdr (assoc (match-string 0) org-structure-template-alist)))
            (progn
              (backward-delete-char 1)
              (org-insert-structure-template type)
              (org-open-line 1))
          (insert "<>")
          (backward-char)))
       (t
        (let ((count (if (numberp arg)
                         arg
                       1)))
          (insert (make-string count ?<)
                  (make-string count ?>))
          (backward-char count)))))))

;;;###autoload
(defun akirak-org-clocked-entry-or-agenda (&optional arg)
  "Toggle display of the clocked entry or display an agenda."
  (interactive "P")
  (cond
   (arg
    ;; I don't know what `org-agenda-window-setup' value would be suitable here.
    ;; 'other-window is my current setting.
    (org-agenda))
   ((org-clocking-p)
    (let ((buffer (org-dog-indirect-buffer org-clock-marker)))
      (if-let (window (get-buffer-window buffer))
          (quit-window nil window)
        (org-switch-to-buffer-other-window buffer))))
   (t
    (let ((org-agenda-window-setup 'current-window))
      (org-agenda nil "n")))))

(provide 'akirak-org)
;;; akirak-org.el ends here

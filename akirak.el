;;; akirak.el --- A collection of opinionated settings -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: local
;; URL: https://github.com/akirak/akirak-mode

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

;; FIXME

;;; Code:

(defgroup akirak nil
  "Keybindings for akirak."
  :group 'local)

(defun akirak--init-bindings ()
  "Return an initial value of `akirak-minor-mode-map'."
  (let ((m (make-sparse-keymap)))
    m))

(defvar akirak-minor-mode-map
  (akirak--init-bindings)
  "Keymap activated in `akirak-mode' and `akirak-minor-mode'.")

;;;###autoload
(define-minor-mode akirak-minor-mode
  "Turn on preferences of @akirak in the buffer."
  :init-value nil
  :lighter " akirak"
  :keymap akirak-minor-mode-map
  nil)

;;;###autoload
(define-globalized-minor-mode akirak-mode akirak-minor-mode
  akirak-minor-mode-on
  nil)

;;;###autoload
(defun akirak-minor-mode-on ()
  "Turn on `akirak-minor-mode'."
  (interactive)
  (akirak-minor-mode 1))

;;;###autoload
(defun akirak-minor-mode-off ()
  "Turn off `akirak-minor-mode'."
  (interactive)
  (akirak-minor-mode -1))

(provide 'akirak)
;;; akirak.el ends here

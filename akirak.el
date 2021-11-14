;;; akirak.el --- A collection of commands and utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (avy "0.5") (org-starter "0.2") (transient "0.3") (f "0.20") (org "9.4") (org-journal "2.1") (memoize "1.1"))
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

;; TODO: Commentary

;;; Code:

(require 'akirak-elec-pair)
(require 'akirak-git-clone)
(require 'akirak-readable)
(require 'akirak-capture)
(require 'akirak-org-journal)

(provide 'akirak)
;;; akirak.el ends here

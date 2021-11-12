;;; akirak-git-clone.el --- Clone Git repositories from flake refs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (f "0.20"))
;; Keywords: vc
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

;; This library helps you browse remote repositories.

;; It accepts both flake references and web browse URLs.

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'rx)

(defgroup akirak-git-clone
  nil
  "Clone Git repositories."
  :group 'github)

(defcustom akirak-git-clone-root "~/archives/personal/git/"
  "Root of the repositories."
  :type 'directory)

(defcustom akirak-git-clone-browser-function
  #'dired
  "Function used to open a directory."
  :type 'function)

(cl-defstruct akirak-git-clone-source
  "Type for representing a repository."
  type origin local-path rev-or-ref params)

(defun akirak-git-clone--parse (flake-ref)
  "Parse FLAKE-REF."
  (pcase flake-ref
    ((rx bol "github:" (group (+ (not (any "/")))
                              "/"
                              (+ (not (any "/"))))
         (?  "/" (group (+ (not (any "?")))))

         (?  "?" (group (+ anything))))
     (let ((local-path (f-join "github.com" (match-string 1 flake-ref)))
           (rev-or-ref (match-string 2 flake-ref))
           (params (match-string 3 flake-ref))
           (origin (format "https://github.com/%s.git"
                           (match-string 1 flake-ref))))
       (make-akirak-git-clone-source :type 'github
                                     :origin origin
                                     :local-path local-path
                                     :rev-or-ref rev-or-ref
                                     :params params)))
    ((rx bol "https://"
         (group (or "github.com"
                    "gitlab.com"))
         "/"
         (group (+ (not (any "/")))
                "/"
                (+ (not (any "/")))))
     (let* ((host (match-string 1 flake-ref))
            (match (match-string 2 flake-ref))
            (path (if (string-match-p (rx ".git" eol) match)
                      (substring match 0 -4)
                    match))
            (local-path (f-join host path))
            (origin (format "https://%s/%s.git" host path)))
       (make-akirak-git-clone-source :type 'github
                                     :origin origin
                                     :local-path local-path)))
    ;; Quick-and-dirty pattern for Git URLs.
    ;; Maybe import more comprehensive regexp from git-identity.el
    ((rx bol (or "https" "git" "ssh") "://"
         (?  (+ (any "-_." alnum)) "@")
         (group (+ (any "-_" alnum)) (+ "." (+ (any "-_" alnum))))
         (?  ":" (+ (char digit)))
         "/"
         (group (+? anything))
         ".git"
         (?  "/")
         eol)
     (let* ((host (match-string 1 flake-ref))
            (match (match-string 2 flake-ref))
            (path (if (string-match-p (rx ".git" eol) match)
                      (substring match 0 -4)
                    match))
            (local-path (f-join host path))
            (origin flake-ref))
       (make-akirak-git-clone-source :type 'git
                                     :origin origin
                                     :local-path local-path)))
    (_
     (error "Unsupported ref: %s" flake-ref))))

(defun akirak-git-clone--clone (origin dest)
  "Clone a Git repository from ORIGIN to DEST."
  (let ((parent (f-parent dest)))
    (unless (file-directory-p parent)
      (make-directory parent t)))
  (message "Cloning %s to %s..." origin dest)
  (let ((proc (start-process "flake clone"
                             "*flake clone*"
                             "git"
                             "clone"
                             "--filter=blob:none"
                             origin dest)))
    (set-process-sentinel proc
                          `(lambda (process _event)
                             (when (eq 'exit (process-status process))
                               (if (= 0 (process-exit-status process))
                                   (akirak-git-clone-browse ,dest)
                                 (message "Returned non-zero from git-clone")))))))

;;;###autoload
(defun akirak-git-clone (flake-ref)
  "Clone a repository from FLAKE-REF."
  (interactive (list (read-string "Flake ref: ")))
  (unless (file-directory-p akirak-git-clone-root)
    (error "First set akirak-git-clone-root to an existing directory"))
  (let* ((obj (akirak-git-clone--parse flake-ref))
         (origin (akirak-git-clone-source-origin obj))
         (repo (expand-file-name (akirak-git-clone-source-local-path obj)
                                 akirak-git-clone-root)))
    (when (akirak-git-clone-source-rev-or-ref obj)
      (error "Rev or ref is unsupported now"))
    (if (file-directory-p repo)
        (akirak-git-clone-browse repo)
      (akirak-git-clone--clone origin repo))))

(defun akirak-git-clone-browse (dir)
  "Browse DIR using `akirak-git-clone-browser-function'."
  (funcall akirak-git-clone-browser-function (file-name-as-directory dir)))

(provide 'akirak-git-clone)
;;; akirak-git-clone.el ends here

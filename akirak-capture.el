;;; akirak-capture.el --- My capture workflow -*- lexical-binding: t -*-

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

(require 'org-starter)
(require 'transient)
(require 'org-capture)
(require 'ol)

(require 'akirak-org-capture)
(require 'akirak-org-journal)
(require 'akirak-transient)
(require 'akirak-clipboard)

(declare-function org-clocking-p "org-clock")
(declare-function which-function "which-func")
(declare-function project-root "project")

;;;; Generic headline

(defvar akirak-capture-org-headline nil)

;;;;; URL link

(defclass akirak-capture-url-variable (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj akirak-capture-url-variable))
  "TODO: Document on OBJ."
  (let ((value (eval (oref obj variable))))
    (pcase-let ((`(url ,url) value))
      (oset obj value url))
    (set (oref obj variable) value)))

(cl-defmethod transient-infix-set ((obj akirak-capture-url-variable) value)
  "TODO: Document function with OBJ and VALUE as args."
  (oset obj value value)
  (set (oref obj variable) `(url ,value)))

(cl-defmethod transient-format-value ((obj akirak-capture-url-variable))
  "TODO: Document on OBJ."
  (concat
   (propertize "(" 'face 'transient-inactive-value)
   (if-let (value (oref obj value))
       (propertize value 'face 'transient-value)
     "")
   (propertize ")" 'face 'transient-inactive-value)))

(transient-define-infix akirak-capture--url-headline ()
  :class 'akirak-capture-url-variable
  :reader #'akirak-transient-url-reader
  :variable 'akirak-capture-org-headline
  :prompt "Url: "
  :description "Link to Url from headline")

;;;; Generic options

;;;;; Toggle class

(defclass akirak-capture-toggle-variable (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj akirak-capture-toggle-variable))
  "TODO: Document on OBJ."
  (let ((value (eval (oref obj variable))))
    (oset obj value value)
    (set (oref obj variable) value)))

(cl-defmethod transient-infix-read ((obj akirak-capture-toggle-variable))
  "TODO: Document on OBJ."
  (not (oref obj value)))

(cl-defmethod transient-infix-set ((obj akirak-capture-toggle-variable) value)
  "TODO: Document function with OBJ and VALUE as args."
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj akirak-capture-toggle-variable))
  "TODO: Document on OBJ."
  (concat
   (propertize "(" 'face 'transient-inactive-value)
   (propertize (if (oref obj value) "t" "") 'face 'transient-value)
   (propertize ")" 'face 'transient-inactive-value)))

;;;;; Clock in

(defvar akirak-transient-clock-in nil)

(transient-define-infix akirak-capture--set-clock-in ()
  :class 'akirak-capture-toggle-variable
  :variable 'akirak-transient-clock-in
  :description "Clock in to the entry")

;;;;; Todo keywords

(defvar akirak-capture-todo-keyword nil)

(transient-define-infix akirak-capture--todo-option ()
  :class 'akirak-capture-toggle-variable
  :variable 'akirak-capture-todo-keyword
  :description "Make it a todo")

;;;; Inserted contents

;;;;; Blocks

(defclass akirak-capture-org-block-variable (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj akirak-capture-org-block-variable))
  "TODO: Document on OBJ."
  (let ((value (eval (oref obj variable))))
    (oset obj value value)
    (set (oref obj variable) value)))

(cl-defmethod transient-infix-read ((_ akirak-capture-org-block-variable))
  "TODO: Document."
  (pcase-let ((`(,_ ,type ,params) (org--insert-structure-template-mks)))
    (when type
      (list type params))))

(cl-defmethod transient-infix-set ((obj akirak-capture-org-block-variable) value)
  "TODO: Document function with OBJ and VALUE as args."
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj akirak-capture-org-block-variable))
  "TODO: Document on OBJ."
  (pcase-let ((`(,type ,params) (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (if type
         (propertize (concat type
                             (if (and params (not (string-empty-p params)))
                                 " "
                               "")
                             params)
                     'face 'transient-value)
       "")
     (propertize ")" 'face 'transient-inactive-value))))

(defvar akirak-capture-org-block nil)

(transient-define-infix akirak-capture--org-block-content ()
  :class 'akirak-capture-org-block-variable
  :if #'use-region-p
  :variable 'akirak-capture-org-block
  :description "Insert block")

;;;;; Link to the current buffer
(defclass akirak-capture-org-link-variable (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj akirak-capture-org-link-variable))
  "TODO: Document on OBJ."
  (let ((value (eval (oref obj variable))))
    (oset obj value value)
    (set (oref obj variable) value)))

(cl-defmethod transient-infix-read ((obj akirak-capture-org-link-variable))
  "TODO: Document on OBJ."
  (let ((value (oref obj value)))
    (if value
        nil
      (when-let (annotation (org-store-link nil))
        (or (save-match-data
              (when (string-match org-link-bracket-re annotation)
                (let* ((href (match-string 1 annotation))
                       (filename (buffer-file-name (buffer-base-buffer)))
                       (project (and filename (project-current)))
                       (context (and filename (which-function))))
                  (when filename
                    (org-link-make-string
                     href
                     (concat (if project
                                 (file-relative-name filename (project-root project))
                               (abbreviate-file-name filename))
                             (if context
                                 (concat "::" context)
                               "")))))))
            annotation)))))

(cl-defmethod transient-infix-set ((obj akirak-capture-org-link-variable) value)
  "TODO: Document function with OBJ and VALUE as args."
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj akirak-capture-org-link-variable))
  "TODO: Document on OBJ."
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (if value
         (propertize (save-match-data
                       (if (string-match org-link-bracket-re value)
                           (or (match-string 2 value)
                               (match-string 1 value))
                         value))
                     'face 'transient-value)
       "")
     (propertize ")" 'face 'transient-inactive-value))))

(defvar akirak-capture-org-link nil)

(transient-define-infix akirak-capture--link-content ()
  :class 'akirak-capture-org-link-variable
  :variable 'akirak-capture-org-link
  :description "Insert link to the current buffer")

;;;; Building a generic template

(defun akirak-capture--template-string ()
  "Build the template string according to the transient options."
  (pcase-let* ((`(url ,url) akirak-capture-org-headline)
               (`(,block-type ,block-params) akirak-capture-org-block)
               (point (if (or akirak-capture-org-headline
                              akirak-capture-org-link
                              block-type)
                          'body
                        'headline)))
    (akirak-org-capture-make-entry-body
      ;; Headline
      (cond
       (url
        (org-link-make-string url (akirak-readable-url-title url)))
       ((eq point 'headline)
        "%?")
       (t
        "%^{headline}"))
      :todo (pcase akirak-capture-todo-keyword
              ((pred stringp) akirak-capture-todo-keyword)
              ('t "TODO")
              (_ nil))
      ;; There is no :tags at present
      :body
      (thread-last (list (when (eq point 'body)
                           "%?")
                         (when block-type
                           (concat "\n#+begin_" block-type
                                   (if (string-empty-p block-params)
                                       ""
                                     " ")
                                   block-params
                                   "\n"
                                   (string-trim
                                    (buffer-substring-no-properties
                                     (region-beginning)
                                     (region-end)))
                                   "\n#+end_" block-type))
                         (when akirak-capture-org-link
                           (concat "\n" akirak-capture-org-link)))
        (delq nil)
        (apply #'concat)))))

(defun akirak-capture--template-params ()
  "Build parameters for the template according to the transient options."
  (append (when akirak-transient-clock-in
            '(:clock-in t :clock-resume t))
          ;; I have a rule on :no-save option: If you don't clock in to the new
          ;; entry, don't save the file, because you are likely to clocking in
          ;; to another task.
          ;;
          ;; I don't want to save files too frequently, because it slightly
          ;; slows down the finalization process.
          '(:no-save t)))

(defun akirak-capture--new-entry (destination)
  "Run `org-capture' to DESTINATION."
  (let ((org-capture-entry `("" ""
                             entry
                             ,(if (functionp destination)
                                  (list 'function destination)
                                destination)
                             ,(akirak-capture--template-string)
                             ,@(akirak-capture--template-params))))
    (org-capture)))

;;;; Targets

(transient-define-suffix akirak-capture--clock-target ()
  :description 'akirak-capture--clock-target-description
  :if (lambda () (org-clocking-p))
  (interactive)
  (akirak-capture--new-entry
   (lambda ()
     (org-goto-marker-or-bmk org-clock-marker)
     (org-back-to-heading))))

(defun akirak-capture--clock-target-description ()
  "Return the description of the current clock."
  (concat "Current clock: "
          (if (and org-clock-marker
                   (markerp org-clock-marker)
                   (buffer-live-p (marker-buffer org-clock-marker)))
              (org-with-point-at org-clock-marker
                (let ((filename (file-name-nondirectory
                                 (buffer-file-name
                                  (org-base-buffer (current-buffer))))))
                  (substring-no-properties
                   (org-format-outline-path
                    (org-get-outline-path t)
                    (window-width)
                    filename
                    " > "))))
            "N/A")))

(transient-define-suffix akirak-capture--info-target ()
  :description "Information (org-journal)"
  :if (lambda () (bound-and-true-p org-journal-dir))
  (interactive)
  (akirak-capture--new-entry
   (akirak-org-journal-group-target "Information")))

(transient-define-suffix akirak-capture--random-target ()
  :description "Random (org-journal)"
  :if (lambda () (bound-and-true-p org-journal-dir))
  (interactive)
  (akirak-capture--new-entry
   (akirak-org-journal-group-target "Random")))

(transient-define-suffix akirak-capture--journal-todo-target ()
  :class 'transient-suffix
  :if (lambda () (bound-and-true-p org-journal-dir))
  :description "Todo (org-journal)"
  (interactive)
  (setq akirak-capture-todo-keyword t)
  (akirak-capture--new-entry
   #'akirak-org-journal-find-location))

;;;; Specific actions

(transient-define-suffix akirak-capture-org-research ()
  :class 'transient-suffix
  :if (lambda () (bound-and-true-p org-journal-dir))
  :description "Start a research topic"
  (interactive)
  (let ((org-capture-entry `("" ""
                             entry
                             (function
                              ,(akirak-org-journal-group-target "Research"
                                 :tags "@research"))
                             ,(akirak-org-capture-make-entry-body
                                "%?"
                                :todo "STARTED"
                                :body nil)
                             :clock-in t
                             :clock-resume t)))
    (org-capture)))

(transient-define-suffix akirak-capture-news-session ()
  :class 'transient-suffix
  :if (lambda () (bound-and-true-p org-journal-dir))
  :description "Start a news session"
  (interactive)
  (let ((org-capture-entry `("" ""
                             entry
                             (function akirak-org-journal-find-location)
                             ,(akirak-org-capture-make-entry-body
                                "News"
                                :tags "@news"
                                :body t)
                             :clock-in t
                             :clock-resume t)))
    (org-capture)))

;;;; Prefix

(transient-define-prefix akirak-capture-dispatch (&rest plist)
  ["Generic template"
   ("u" akirak-capture--url-headline)
   ;; TODO: Allow more variants of headline
   ;; ("-h" )
   ;; ("-t" akirak-capture--todo-option)
   ;; ("-i" akirak-capture--set-clock-in)
   ("+" akirak-capture--org-block-content)
   ("-l" akirak-capture--link-content)]
  ["Targets for generic template"
   ("@" akirak-capture--clock-target)
   ("i" akirak-capture--info-target)
   ("r" akirak-capture--random-target)
   ("t" akirak-capture--journal-todo-target)]
  ["New session - Clock in"
   ("r" akirak-capture-org-research)
   ("n" akirak-capture-news-session)]
  ["Others"
   ;; ("s" "Screen" akirak-capture-screen)
   ("o" "Org Capture" org-capture)]
  (interactive)
  (setq akirak-capture-org-headline
        (cond
         ((plist-get plist :url)
          `(url ,(plist-get plist :url))))
        akirak-transient-clock-in nil
        akirak-capture-todo-keyword nil
        akirak-capture-org-block (when (use-region-p)
                                   (if (akirak-capture--prog-or-conf-mode-p)
                                       (list "src" (string-remove-suffix
                                                    "-mode" (symbol-name major-mode)))
                                     (list "example" "")))
        akirak-capture-org-link nil)
  (transient-setup 'akirak-capture-dispatch))

(defun akirak-capture--prog-or-conf-mode-p ()
  "Determine if the mode is some kind of code or config."
  (let ((mode major-mode)
        modes)
    (catch 'ok
      (while mode
        (push mode modes)
        (setq mode (get mode 'derived-mode-parent))))
    (or (memq 'prog-mode modes)
        (and (memq 'text-mode modes)
             (not (eq 'text-mode major-mode))
             (not (memq 'org-mode modes))
             (not (memq 'markdown-mode modes))))))

;;;###autoload
(defun akirak-capture (arg)
  "Capture something.

When two universal prefixes are given as ARG, it jumps to the
recently captured location, as in `org-capture'."
  (interactive "P")
  (pcase arg
    ('(16) (org-capture '(16)))
    (_ (akirak-capture-dispatch))))

(provide 'akirak-capture)
;;; akirak-capture.el ends here

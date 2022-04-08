;;; akirak-emacs-config.el ---  -*- lexical-binding: t -*-

(require 'akirak-org-capture)
(require 'org-starter)

(defvar akirak-emacs-config-capture-templates
  (cl-macrolet
      ((level2 (text)
         `(lambda ()
            (widen)
            (goto-char (point-min))
            (or (re-search-forward (rx bol "**" (+ space)
                                       (literal ,text))
                                   nil t)
                (error "Heading \"%s\" is not found in the file" ,text)))))
    (let ((file (org-starter-locate-file "emacs-config.org" nil t)))
      `(("Emacs Config" :keys ""
         :when ,(stringp file)
         :file ,file
         :jump-to-captured t
         :template
         ("* %^{Name}"
          ,akirak-org-capture-default-drawer
          "#+begin_src emacs-lisp"
          "%{src}"
          "#+end_src")
         :children
         (("Builtin" :keys "b"
           :function ,(level2 "Built-ins")
           :src "(setup %\\1\n  %?)")
          ("Org package" :keys "o"
           :function ,(level2 "Org")
           :src "(setup (:package %\\1)%?)")
          ("Package" :keys "p"
           :function ,(level2 "Packages")
           :src "(setup (:package %\\1)%?)")
          ("Macro package" :keys "M"
           :function ,(level2 "Macro packages")
           :src "(setup (:package %\\1)%?)")

          ("Define a setup macro" :keys "d"
           :function ,(level2 "Setup.el")
           :src "(eval-when-compile\n  (define-setup-macro %\\1 (%?)))")

          ("Note" :keys "n"
           :function ,(level2 "Notes")
           :template
           ("* %?"
            ,akirak-org-capture-default-drawer))

          ("Org-Ql dynamic block for a tag" :keys "q"
           :contexts (:in-file "emacs-config\\.org\\'")
           :type plain
           :function ignore
           :immediate-finish t
           :template
           ("#+BEGIN: org-ql :query \"tags:%^{tag}\" :columns (heading todo)"
            "#+END:"))))))))

;;;###autoload
(defun akirak-emacs-config-capture ()
  (interactive)
  (akirak-org-capture-with-doct akirak-emacs-config-capture-templates))

(provide 'akirak-emacs-config)
;;; akirak-emacs-config.el ends here

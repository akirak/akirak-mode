;;; akirak-beancount.el ---  -*- lexical-binding: t -*-

(require 'beancount)

(defgroup akirak-beancount nil
  ""
  :group 'beancount)

(defcustom akirak-beancount-journal-file nil
  "The master journal file."
  :type 'file)

(defcustom akirak-beancount-currency nil
  "Default currency."
  :type 'string)

(defmacro akirak-beancount--with-wide-buffer (&rest progn)
  `(with-current-buffer (or (find-buffer-visiting akirak-beancount-journal-file)
                            (find-file-noselect akirak-beancount-journal-file))
     (org-with-wide-buffer
      ,@progn)))

(defun akirak-beancount-complete-outline ()
  (goto-char (point-min))
  (let (candidates)
    (while (outline-next-heading)
      (push (buffer-substring (point) (line-end-position))
            candidates))
    (let ((input (completing-read "Heading: " candidates nil t)))
      (goto-char (point-min))
      (search-forward input nil nil))))

(cl-defun akirak-beancount-add-transaction (&key account date title quantity
                                                 price-num price-currency
                                                 payment)
  (akirak-beancount--with-wide-buffer
   (goto-char (point-min))
   (or (re-search-forward (concat (rx bol (* blank)) (regexp-quote account))
                          nil t)
       (akirak-beancount-complete-outline))
   (when (outline-next-heading)
     (forward-line -1))
   (insert "\n" date " * " (format "\"%s\"" title)
           "\n  " account "  "
           (if (= quantity 1)
               price-num
             (number-to-string (* quantity (string-to-number price-num))))
           (or price-currency
               (concat " " akirak-beancount-currency))
           "\n  " payment "\n")
   (beancount-indent-transaction)))

(defun akirak-beancount-account-completions ()
  "Return a completion table for accounts.

For now, it simply returns a list of accounts."
  (akirak-beancount--with-wide-buffer
   (beancount-collect beancount-account-regexp 0)))

(provide 'akirak-beancount)
;;; akirak-beancount.el ends here

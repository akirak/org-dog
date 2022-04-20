;;; org-dog-utils.el ---  -*- lexical-binding: t -*-

(require 'org)
(require 'subr-x)

(defvar org-keyword-regexp)

(defsubst org-dog-case-fold-equal (string1 string2)
  "Compare two strings, ignoring case."
  (equal (downcase string1)
         (downcase string2)))

(defun org-dog--file-title ()
  "Return the title of the current Org buffer, if any."
  (catch 'org-dog-file-title
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (<= (point) (point-max))
       (save-match-data
         (when (and (looking-at org-keyword-regexp)
                    (org-dog-case-fold-equal (match-string 1)
                                             "title"))
           (throw 'org-dog-file-title
                  (substring-no-properties (string-trim (match-string 2)))))
         (when (looking-at org-heading-regexp)
           (throw 'org-dog-file-title nil))
         (forward-line)))
     nil)))

(provide 'org-dog-utils)
;;; org-dog-utils.el ends here

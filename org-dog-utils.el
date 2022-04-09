;;; org-dog-utils.el ---  -*- lexical-binding: t -*-

(defsubst org-dog-case-fold-equal (string1 string2)
  "Compare two strings, ignoring case."
  (equal (downcase string1)
         (downcase string2)))

(provide 'org-dog-utils)
;;; org-dog-utils.el ends here

;;; org-dog-journal.el --- Org dog files with a reverse date tree -*- lexical-binding: t -*-

(require 'org-dog)
(require 'org)
(require 'doct)
(require 'org-reverse-datetree)

(defgroup org-dog-journal nil
  "Date tree file type for org-dog."
  :prefix "org-dog-journal-"
  :group 'org-reverse-datetree
  :group 'org)

(defvar org-dog-journal-refile-history nil)

(defclass org-dog-journal (org-dog-file)
  ((journal-capture-templates
    :initform nil
    :initarg :datetree-capture-templates)))

(cl-defmethod org-dog-file-refile ((file org-dog-journal))
  (org-reverse-datetree-refile-to-file (oref file absolute)))

(cl-defmethod org-dog-file-capture-templates ((file org-dog-journal))
  (doct (thread-last
          (oref file journal-capture-templates)
          (mapcar (pcase-lambda
                    (`(,key ,description ,template . ,options))
                    (append (list description
                                  :keys key
                                  :file absolute
                                  :function #'org-reverse-datetree-goto-date-in-file
                                  :template template)
                            options))))))

;;;###autoload
(defun org-dog-journal-refile (file)
  "Refile to the datetree in FILE."
  (interactive (list (completing-read
                      "Refile to datetree: "
                      (org-dog-file-completion :class 'org-dog-journal)
                      nil nil nil org-dog-journal-refile-history)))
  (org-reverse-datetree-refile-to-file file))

(defun org-dog-journal-refile-to-this-file ()
  "Refile to the datetree in the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (object-of-class-p (org-dog-file-object (abbreviate-file-name file))
                           'org-dog-journal)
        (org-reverse-datetree-refile-to-file file)
      (user-error "Not in `org-dog-journal'"))))

;; TODO: Use org-ql but without helm
;; (cl-defmethod org-dog-file-search ((file org-dog-journal)))

(provide 'org-dog-journal)
;;; org-dog-journal.el ends here

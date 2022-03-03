;;; org-dog-datetree.el --- Org dog files with a reverse date tree -*- lexical-binding: t -*-

(require 'org-dog)
(require 'doct)

(defcustom org-dog-datetree-default-template
  '("* %?")
  "Default template to be used in `org-capture-templates'.")

(defclass org-dog-datetree-file (org-dog-file)
  ((capture-templates :initform nil
                      :initarg :capture-templates)))

(cl-defmethod org-dog-file-refile ((file org-dog-datetree-file))
  (org-reverse-datetree-refile-to-file (oref file absolute)))

(cl-defmethod org-dog-file-capture ((file org-dog-datetree-file))
  (let* ((org-capture-templates (org-dog-datetree--templates
                                 (oref file absolute)
                                 (or (oref file capture-templates)
                                     (list org-dog-datetree-default-template))))
         (org-capture-entry (when (= 1 (length org-capture-templates))
                              (car org-capture-templates))))
    (org-capture)))

(defun org-dog-datetree--templates (absolute capture-templates)
  (doct (thread-last
          capture-templates
          (mapcar (pcase-lambda
                    (`(,key ,description ,template . ,options))
                    (append (list description
                                  :keys key
                                  :file absolute
                                  :function #'org-reverse-datetree-goto-date-in-file
                                  :template template)
                            options))))))

;; TODO: Use org-ql but without helm
;; (cl-defmethod org-dog-file-search ((file org-dog-datetree-file)))

(provide 'org-dog-datetree)
;;; org-dog-datetree.el ends here

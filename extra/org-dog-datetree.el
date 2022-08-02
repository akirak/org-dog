;;; org-dog-datetree.el --- Org dog files with a reverse date tree -*- lexical-binding: t -*-

(require 'org-dog)
(require 'org)
(require 'doct)
(require 'org-reverse-datetree)

(defvar org-capture-entry)
(declare-function org-reverse-datetree-num-levels "ext:org-reverse-datetree")
(declare-function org-reverse-datetree-guess-date "ext:org-reverse-datetree")

(defgroup org-dog-datetree nil
  "Date tree file type for org-dog."
  :prefix "org-dog-datetree-"
  :group 'org-reverse-datetree
  :group 'org-dog)

(defcustom org-dog-datetree-default-templates
  nil
  "List of default capture templates for `org-dog-datetree'."
  :type '(repeat (list (string :tag "Key")
                       (string :tag "Description")
                       (choice :tag "Doct template body"
                               string
                               (repeat string)
                               function)
                       (plist :inlint t))))

(defvar org-dog-datetree-refile-history nil)

(defclass org-dog-datetree-file (org-dog-file)
  ((journal-capture-templates
    :initform 'org-dog-datetree-default-templates
    :initarg :datetree-capture-templates)))

(cl-defmethod org-dog-file-refile ((file org-dog-datetree-file))
  (org-reverse-datetree-refile-to-file (oref file absolute)))

(cl-defmethod org-dog-file-capture-templates ((file org-dog-datetree-file))
  (doct (thread-last
          (org-dog-symbol-value (oref file journal-capture-templates))
          (mapcar (pcase-lambda
                    (`(,key ,description ,template . ,options))
                    (append (list description
                                  :keys key
                                  :file (oref file absolute)
                                  :function #'org-reverse-datetree-goto-date-in-file
                                  :template template)
                            options))))))

;;;###autoload
(defun org-dog-datetree-refile (file)
  "Refile to the datetree in FILE."
  (interactive (list (completing-read
                      "Refile to datetree: "
                      (org-dog-file-completion :class 'org-dog-datetree-file)
                      nil nil nil org-dog-datetree-refile-history)))
  (org-reverse-datetree-refile-to-file file))

(defun org-dog-datetree-refile-to-this-file ()
  "Refile to the datetree in the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (object-of-class-p (org-dog-file-object (abbreviate-file-name file))
                           'org-dog-datetree-file)
        (org-reverse-datetree-refile-to-file file)
      (user-error "Not in `org-dog-datetree-file'"))))

;;;###autoload
(defun org-dog-datetree-transclude-this-entry (file)
  "Add a link to the current entry in another datetree."
  (interactive (list (completing-read
                      "Link from datetree: "
                      (org-dog-file-completion :class 'org-dog-datetree-file))))
  (unless (derived-mode-p 'org-mode)
    (user-error "You have to run this command inside org-mode"))
  (let ((org-capture-entry `("" ""
                             plain
                             (file+function
                              ,file
                              (lambda ()
                                (org-reverse-datetree-goto-date-in-file
                                 ',(org-reverse-datetree-guess-date))))
                             "#+transclude: %a %?"
                             :unnarrowed t)))
    (org-capture)))

(cl-defmethod org-dog-meaningful-in-file-p ((_file org-dog-datetree-file))
  (let ((level (org-outline-level))
        (heading (org-get-heading t t t t)))
    (not (and (<= level (org-reverse-datetree-num-levels))
              (string-match-p "\\`[[:digit:]]\\{4\\}" heading)))))

(provide 'org-dog-datetree)
;;; org-dog-datetree.el ends here

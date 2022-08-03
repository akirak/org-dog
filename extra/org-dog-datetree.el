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

(defcustom org-dog-datetree-distribute-when-refile
  nil
  "Whether to generate transclusion links on refiling.

If this option is set to non-nil,
`org-dog-datetree-transclude-by-tag' is run when
`org-dog-datetree-refile' or
`org-dog-datetree-refile-to-this-file' is run.

This virtually means the information is distributed to its
relevant files when an entry is archived."
  :type 'boolean)

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
  (when org-dog-datetree-distribute-when-refile
    (org-dog-datetree-transclude-by-tag t))
  (org-reverse-datetree-refile-to-file file))

(defun org-dog-datetree-refile-to-this-file ()
  "Refile to the datetree in the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (object-of-class-p (org-dog-file-object (abbreviate-file-name file))
                           'org-dog-datetree-file)
        (progn
          (when org-dog-datetree-distribute-when-refile
            (org-dog-datetree-transclude-by-tag t))
          (org-reverse-datetree-refile-to-file file))
      (user-error "Not in `org-dog-datetree-file'"))))

;;;###autoload
(cl-defun org-dog-datetree-transclude-this-entry (file &key date)
  "Add a link to the current entry from another datetree."
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
                                 ',(or date (org-reverse-datetree-guess-date)))))
                             "#+transclude: %a"
                             :immediate-finish t)))
    (org-capture)))

;;;###autoload
(defun org-dog-datetree-transclude-by-tag (&optional allow-empty)
  "Transclude this entry from date trees containing one of the tags."
  (interactive)
  (let* ((this-file (when-let (obj (org-dog-buffer-object))
                      (oref obj absolute)))
         (tags (org-get-tags))
         (files (thread-last
                  (org-dog-select-files
                   (org-dog-make-file-pred
                    :buffer-pred
                    `(cl-intersection ',tags org-file-tags :test #'equal)))
                  (mapcar (lambda (obj)
                            (oref obj absolute)))
                  (cl-remove-if `(lambda (file)
                                   (and ,this-file (equal file ,this-file))))))
         (date (org-reverse-datetree-guess-date)))
    (if files
        (progn
          (dolist (file files)
            (org-dog-datetree-transclude-this-entry file :date date))
          (message "Linked to the entry from %s" (string-join files ", ")))
      (unless allow-empty
        (user-error "No files")))))

(cl-defmethod org-dog-meaningful-in-file-p ((_file org-dog-datetree-file))
  (let ((level (org-outline-level))
        (heading (org-get-heading t t t t)))
    (not (and (<= level (org-reverse-datetree-num-levels))
              (string-match-p "\\`[[:digit:]]\\{4\\}" heading)))))

(provide 'org-dog-datetree)
;;; org-dog-datetree.el ends here

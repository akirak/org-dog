;;; org-dog-datetree.el --- Org dog files with a reverse date tree -*- lexical-binding: t -*-

(require 'org-dog)
(require 'org)
(require 'doct)
(require 'org-reverse-datetree)

(defvar org-capture-entry)
(declare-function org-reverse-datetree-num-levels "ext:org-reverse-datetree")
(declare-function org-reverse-datetree-guess-date "ext:org-reverse-datetree")
(declare-function org-transclusion-add "ext:org-transclusion")

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

(defcustom org-dog-datetree-propagate-on-refile
  nil
  "Whether to generate transclusion links on refiling.

If this option is set to non-nil,
`org-dog-datetree-propagate-by-tag' is run when
`org-dog-datetree-refile' or
`org-dog-datetree-refile-to-this-file' is run.

This virtually means the information is distributed to its
relevant files when an entry is archived."
  :type 'boolean)

(defcustom org-dog-datetree-generate-id-on-refile
  t
  "Whether to generate an id on refiling."
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
  (when org-dog-datetree-generate-id-on-refile
    (org-id-get-create))
  (let ((date (org-reverse-datetree-refile-to-file file)))
    (when org-dog-datetree-propagate-on-refile
      (org-dog-datetree-propagate-by-tag nil :date date))))

(defun org-dog-datetree-refile-to-this-file ()
  "Refile to the datetree in the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (object-of-class-p (org-dog-file-object (abbreviate-file-name file))
                           'org-dog-datetree-file)
        (progn
          (when org-dog-datetree-generate-id-on-refile
            (org-id-get-create))
          (let ((date (org-reverse-datetree-refile-to-file file)))
            (when org-dog-datetree-propagate-on-refile
              (org-dog-datetree-propagate-by-tag nil :date date))))
      (user-error "Not in `org-dog-datetree-file'"))))

;;;###autoload
(cl-defun org-dog-datetree-transclude-this-entry (file &key date)
  "Add a link to the current entry from another datetree."
  (interactive (list (completing-read
                      "Link from datetree: "
                      (org-dog-file-completion :class 'org-dog-datetree-file))))
  (unless (derived-mode-p 'org-mode)
    (user-error "You have to run this command inside org-mode"))
  (let ((link (org-store-link nil)))
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (org-with-wide-buffer
       (org-reverse-datetree-goto-date-in-file
        (or date (org-reverse-datetree-guess-date)))
       (org-end-of-meta-data t)
       (insert "#+transclude: " link "\n")
       (org-end-of-line 0)
       (ignore-errors (org-transclusion-add))))))

;;;###autoload
(cl-defun org-dog-datetree-propagate-by-tag (&optional interactive
                                                       &key local
                                                       date subtree)
  "Transclude this entry from other date trees sharing tags."
  (interactive (list t :subtree (eql current-prefix-arg '(4))))
  (if-let (obj (org-dog-buffer-object))
      (if subtree
          (let ((end (save-excursion
                       (org-end-of-subtree))))
            (catch 'no-subtree
              (while t
                (when (org-get-tags nil local)
                  (org-dog-datetree-propagate-by-tag 'interactive
                                                     :local t
                                                     :date date))
                (unless (re-search-forward org-heading-regexp end t)
                  (throw 'no-subtree t)))))
        (when-let (tags (org-get-tags nil local))
          (let* ((this-file (oref obj absolute))
                 (root (oref obj root))
                 (date (or date (org-reverse-datetree-guess-date)))
                 (files (thread-last
                          (org-dog-select-files
                           (org-dog-make-file-pred
                            :header-pred
                            ;; The source entry must contain all of the file tags
                            ;; of the destination file.
                            `(and org-file-tags
                                  (seq-every-p (lambda (tag)
                                                 (member tag ',tags))
                                               org-file-tags))))
                          (cl-remove-if-not `(lambda (obj)
                                               (and (equal (oref obj root)
                                                           ,root)
                                                    (not (equal (oref obj absolute)
                                                                ,this-file)))))
                          (mapcar (lambda (obj)
                                    (oref obj absolute))))))
            (if files
                (progn
                  (dolist (file files)
                    (org-dog-datetree-transclude-this-entry file :date date))
                  (message "Linked to \"%s\" from %s"
                           (org-get-heading t t t t)
                           (string-join files ", ")))
              (when interactive
                (user-error "No files"))))))
    (when interactive
      (user-error "The source file needs to be in a repository"))))

(cl-defmethod org-dog-meaningful-in-file-p ((_file org-dog-datetree-file))
  (let ((level (org-outline-level))
        (heading (org-get-heading t t t t)))
    (not (and (<= level (org-reverse-datetree-num-levels))
              (string-match-p "\\`[[:digit:]]\\{4\\}" heading)))))

(provide 'org-dog-datetree)
;;; org-dog-datetree.el ends here

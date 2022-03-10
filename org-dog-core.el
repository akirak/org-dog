;;; org-dog-core.el --- Core type definitions -*- lexical-binding: t -*-

(require 'eieio)
(require 'seq)
(require 'org)
(require 'cl-lib)

(defgroup org-dog nil
  "A programmable workflow layer for Org mode."
  :prefix "org-dog-"
  :group 'convenience
  :group 'org)

;;;; Custom variables

(defcustom org-dog-default-file-class 'org-dog-file
  "Default class of instance used to track Org files.

This must be a symbol referring to a subclass of `org-dog-file'."
  :type 'symbol)

(defcustom org-dog-exclude-file-pattern
  (rx (or ".sync-conflict" "README"))
  "Pattern of Org files that should not be ignored."
  :type 'regexp)

(defcustom org-dog-repository-alist nil
  "Alist of Org repositories.

This is an alist of entries where the key is a directory and the
value is a plist which specifies how to treat Org files in the directory."
  :type '(alist :key-type directory
                :value-type plist))

(defcustom org-dog-file-registration-hook nil
  "Hook run when each file is found in a repository.

This hook is run when repositories are reloaded using
`org-dog-reload-files' and triggered on each file.

Each function in this hook takes an `org-dog-file' (or its
subclass/descendant) instance.

You can use this, for example, to add files to
`org-agenda-files'."
  :type 'hook)

;;;; Variables

(defvar org-dog--repository-table nil)

(defvar org-dog--file-table nil)

;;;;; Classes

(defclass org-dog-repository ()
  ((root :initarg :root)
   (sxhash :initarg :sxhash)
   (routes :initarg :routes)
   (directories :initarg :directories)))

(defclass org-dog-file ()
  ((absolute :initarg :absolute)
   (relative :initarg :relative)
   (root :initarg :root)))

;;;; File table

(defun org-dog--ensure-file-table ()
  "Ensure the file table is initialized.

This function should be called before `org-dog--file-table' is
accessed."
  (unless org-dog--file-table
    (org-dog-reload-files)))

(defun org-dog-file-object (file)
  "Find a `org-dog-file' object associated with a FILE."
  (org-dog--ensure-file-table)
  (or (gethash file org-dog--file-table)
      (let ((abbr (abbreviate-file-name file)))
        (or (unless (equal abbr file)
              (gethash abbr org-dog--file-table))
            (thread-last
              org-dog--repository-table
              (map-some `(lambda (repo)
                           (when (string-prefix-p (oref repo root)
                                                  ,abbr)
                             (org-dog--make-file-instance repo abbr)))))))
      (unless (file-readable-p file)
        (error "File %s is not readable" file))))

(defun org-dog-select-files (&optional pred)
  "Return a list of `org-dog-file' objects optionally matching PRED."
  (org-dog--ensure-file-table)
  (if pred
      (thread-last (map-values org-dog--file-table)
                   (seq-filter pred))
    (map-values org-dog--file-table)))

(defun org-dog-find-file-object (pred)
  "Find a file object where a slot satisfies PRED."
  (map-some (apply-partially (lambda (pred _key obj)
                               (when (funcall pred obj)
                                 obj))
                             pred)
            org-dog--file-table))

;;;; Repositories

(defun org-dog-reload-files (&optional arg)
  "Reload the file table.

When a universal prefix is given, the repositories are reloaded
as well."
  (interactive "P")
  (if (or (not org-dog--repository-table)
          arg)
      (org-dog--init-repositories)
    (thread-last
      org-dog-repository-alist
      (mapcar #'car)
      (mapc #'org-dog--maybe-update-repository)))
  (if (and org-dog--file-table (hash-table-p org-dog--file-table))
      (clrhash org-dog--file-table)
    (setq org-dog--file-table (make-hash-table :test #'equal)))
  (let ((error-count 0))
    (thread-last
      org-dog--repository-table
      (map-do (lambda (_root repo)
                (dolist (absolute (org-dog--repo-files repo))
                  (unless (gethash absolute org-dog--file-table)
                    (unless (with-demoted-errors
                                "Error while instantiating an object: %s"
                              (org-dog--make-file-instance repo absolute))
                      (cl-incf error-count)))))))
    (message "Registered %d Org files%s" (map-length org-dog--file-table)
             (if (> error-count 0)
                 (format " (%d errors)" error-count)
               ""))
    org-dog--file-table))

(defun org-dog--init-repositories ()
  "Inititialize the list of directories."
  (if org-dog--repository-table
      (clrhash org-dog--repository-table)
    (setq org-dog--repository-table (make-hash-table :test #'equal :size 10)))
  (pcase-dolist (`(,root . ,plist) org-dog-repository-alist)
    (when (file-directory-p root)
      (puthash root (apply #'org-dog--make-repository root
                           :sxhash (sxhash plist) plist)
               org-dog--repository-table))))

(cl-defun org-dog--make-repository (root &rest plist &key subdirs
                                         &allow-other-keys)
  "Make an instance of `org-dog-repository'."
  (cl-flet
      ((normalize-dir (dir)
         (file-name-as-directory (abbreviate-file-name dir))))
    (let ((abbr-root (normalize-dir root))
          (real-subdirs (thread-last
                          (cl-etypecase subdirs
                            (function
                             (funcall subdirs root))
                            (list
                             (thread-last
                               subdirs
                               (mapcar (lambda (str) (expand-file-name str root)))
                               (cl-remove-if-not #'file-directory-p))))
                          (mapcar #'normalize-dir))))
      (apply #'make-instance 'org-dog-repository
             :root abbr-root
             :directories (cons abbr-root real-subdirs)
             (org-dog--remprop plist :subdirs)))))

(defun org-dog--maybe-update-repository (root)
  "If the configuration for ROOT has been changed, update the instance.

This function checks a corresponding entry in
`org-dog-repository-alist' for the repository, and updates its
instance if any of the properties has been changed.
"
  (let* ((repo (gethash root org-dog--repository-table))
         (plist (cdr (assoc root org-dog-repository-alist)))
         (sxhash (sxhash plist)))
    (cond
     ;; Remove from the table?
     ((not plist))
     ;; The same hash: No update is needed
     ((and repo (equal sxhash (oref repo sxhash))))
     (t
      (puthash root (apply #'org-dog--make-repository root :sxhash sxhash plist)
               org-dog--repository-table)))))

(defun org-dog--repo-files (repo)
  "Return an list of files in REPO."
  (let (result)
    (dolist (dir (oref repo directories))
      (thread-last
        (directory-files dir t "^[a-zA-Z].*\\.org\\(?:\\.gpg\\)?\\'" 'nosort)
        (cl-remove-if (lambda (name)
                        (when org-dog-exclude-file-pattern
                          (string-match-p org-dog-exclude-file-pattern name))))
        (mapcar #'abbreviate-file-name)
        (append result)
        (setq result)))
    result))

(defun org-dog--make-file-instance (repo absolute)
  "Create an instance of `org-dog-file' or its subclass from a path.

Both ROOT and ABSOLUTE are required and should be passed from
inside the caller function.

RELATIVE is optional, and it can save little computation if
explicitly given. Maybe unnecessary."
  (let* ((relative (string-remove-prefix (oref repo root) absolute))
         (route (catch 'route
                  (pcase-dolist (`(,pattern . ,ent) (oref repo routes))
                    (when (or (and (stringp pattern)
                                   (string-prefix-p pattern relative))
                              (and (not pattern)
                                   (not (string-match-p "/" relative)))
                              (eq pattern t))
                      (throw 'route ent))))))
    (when-let (instance
               (apply #'make-instance (or (car route)
                                          org-dog-default-file-class)
                      :absolute absolute
                      :relative relative
                      :root (oref repo root)
                      (cdr route)))
      (puthash absolute instance org-dog--file-table)
      (run-hook-with-args 'org-dog-file-registration-hook instance)
      instance)))

;;;; Utilities

(defun org-dog--remprop (plist prop)
  (let (result key)
    (while (setq key (pop plist))
      (if (eq key prop)
          (pop plist)
        (push key result)
        (push (pop plist) result)))
    (nreverse result)))

;;;; Other public APIs

(defun org-dog-file-equal (x y)
  "Return non-nil if X and Y are the same object of `org-dog-file'."
  (and (object-of-class-p x 'org-dog-file)
       (eq (eieio-object-class x)
           (eieio-object-class y))
       (equal (oref x absolute)
              (oref y absolute))))

(defun org-dog-repositories ()
  "Return a list of the current repository objects."
  (map-values org-dog--repository-table))

(provide 'org-dog-core)
;;; org-dog-core.el ends here

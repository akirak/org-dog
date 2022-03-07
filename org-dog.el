;;; org-dog.el --- A programmable workflow layer for Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.5") (project "0.6"))
;; Keywords: org convenience
;; URL: https://github.com/akirak/org-dog

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides the core of a configuration mechanism for Org files.

;;; Code:

(require 'eieio)
(require 'seq)
(require 'org)
(require 'cl-lib)

(declare-function org-link-set-parameters "ext:ol")
(declare-function project-root "ext:project")

(defgroup org-dog nil
  "A programmable workflow layer for Org mode."
  :prefix "org-dog-"
  :group 'convenience
  :group 'org)

;;;; Custom variables

(defcustom org-dog-default-file-class 'org-dog-file
  ""
  :type 'symbol)

(defcustom org-dog-exclude-file-pattern
  (regexp-quote ".sync-conflict")
  "Pattern of Org files that should not be ignored."
  :type 'regexp)

(defcustom org-dog-repository-alist nil
  "Alist of Org repositories.

This is an alist of entries where the key is a directory and the
value is a plist which specifies how to treat Org files in the directory."
  :type '(alist :key-type directory
                :value-type plist))

(defcustom org-dog-context-alist
  '((project
     :value-fn project-current
     :test equal
     :callback org-dog-project-context-1)
    (major-mode
     :callback org-dog-major-mode-context-1)
    (current-language-environment
     :callback org-dog-language-context-1))
  ""
  :type '(alist :key-type symbol
                :value-type '(plist
                              :options ((list (const :value-fn)
                                              function)
                                        (list (const :test)
                                              function)
                                        (list (const :callback)
                                              function)))))

(defcustom org-dog-complete-contextually 'clocking
  ""
  :type '(choice (const t)
                 (const clocking)
                 (const nil)))

;;;; Faces

(defface org-dog-file-directory-face
  '((t :foreground "#aaaadd"))
  "Face for sub-directory components in completion of `org-dog-file' category.")

(defface org-dog-file-class-face
  '((t :inherit font-lock-constant-face))
  "Face for class names in completion of `org-dog-file' category.")

(defface org-dog-repository-face
  '((t :inherit font-lock-comment-face))
  "Face for repository directories in completion of `org-dog-file' category.")

;;;; Variables

(defvar org-dog-repository-instances nil
  "A list of `org-dog-repository' instances.

The user should not manually set this variable.")

(defvar org-dog-file-table nil
  "A hash table.")

(defvar org-dog-context-cache nil
  "A hash table.")

(defvar org-dog-file-completion-history nil)

(defvar org-dog-file-mode-map (make-sparse-keymap)
  "Keymap for `org-dog-file-mode'.")

(defvar org-dog-id-files nil
  "List used to track Org files in `org-dog-id-mode'.")

;;;; Files

;;;;; Class

(defclass org-dog-file ()
  ((absolute :initarg :absolute)
   (relative :initarg :relative)
   (root :initarg :root)
   (agenda :initarg :agenda :initform nil)
   (title :initform nil)))

;;;;; Instances

(defun org-dog--ensure-file-table ()
  "Ensure the file table is initialized.

This function should be called before `org-dog-file-table' is
accessed."
  (unless org-dog-file-table
    (org-dog-reload-files)))

(cl-defun org-dog-make-file-pred (&key class
                                       relative
                                       relative-prefix
                                       basename-regexp
                                       negate-basename-regexp)
  (if-let (conds (thread-last
                   (list (when class
                           `(object-of-class-p obj ',class))
                         (when relative
                           `(equal ,relative (oref obj relative)))
                         (when relative-prefix
                           `(string-prefix-p ,relative-prefix (oref obj relative)))
                         (when basename-regexp
                           `(string-match-p ,(concat "^" basename-regexp "$")
                                            (file-name-base (oref obj relative))))
                         (when negate-basename-regexp
                           `(not (string-match-p ,(concat "^" negate-basename-regexp "$")
                                                 (file-name-base (oref obj relative))))))
                   (delq nil)))
      `(lambda (obj)
         (and ,@conds))
    #'identity))

(defun org-dog-select-files (&optional pred)
  "Return a list of `org-dog-file' objects optionally matching PRED."
  (org-dog--ensure-file-table)
  (if pred
      (thread-last (map-values org-dog-file-table)
                   (seq-filter pred))
    (map-values org-dog-file-table)))

(defun org-dog-file-object (file)
  "Find a `org-dog-file' object associated with a FILE."
  (org-dog--ensure-file-table)
  (or (gethash file org-dog-file-table)
      (let ((abbr (abbreviate-file-name file)))
        (or (unless (equal abbr file)
              (gethash abbr org-dog-file-table))
            (when-let* ((repo (cl-find-if `(lambda (x)
                                             (string-prefix-p (oref x root)
                                                              ,abbr))
                                          org-dog-repository-instances))
                        (root (oref repo root))
                        (instance (org-dog--make-file-instance
                                   :root root :absolute abbr)))
              (when (bound-and-true-p org-dog-id-mode)
                (add-to-list 'org-dog-id-files abbr))
              instance)))
      (unless (file-readable-p file)
        (error "File %s is not readable" file))))

(cl-defun org-dog-find-file-object (pred)
  "Find a file object where a slot satisfies PRED."
  (map-some (apply-partially (lambda (pred _key obj)
                               (when (funcall pred obj)
                                 obj))
                             pred)
            org-dog-file-table))

(defun org-dog-current-buffer-object ()
  "Return the `org-dog-file' object for the current buffer, if any."
  (when-let (filename (buffer-file-name))
    (org-dog-file-object (abbreviate-file-name filename))))

;;;;; Methods

(defun org-dog-file-equal (x y)
  "Return non-nil if X and Y are the same object of `org-dog-file'."
  (and (object-of-class-p x 'org-dog-file)
       (eq (eieio-object-class x)
           (eieio-object-class y))
       (equal (oref x absolute)
              (oref y absolute))))

(cl-defgeneric org-dog-annotate-file (x)
  "Annotation function for completion of `org-dog-file' category.")
(cl-defmethod org-dog-annotate-file ((x org-dog-file))
  "Annotation function for completion of `org-dog-file' category."
  (let ((class (eieio-object-class x)))
    (concat (if (eq class org-dog-default-file-class)
                ""
              (concat " "
                      (propertize (thread-last
                                    (symbol-name class)
                                    (string-remove-prefix "org-dog-")
                                    (string-remove-suffix "-file"))
                                  'face 'org-dog-file-class-face)))
            " "
            (propertize (oref x root) 'face 'org-dog-repository-face))))

(cl-defgeneric org-dog-file-in-agenda-p (x)
  "Return non-nil if X should be added to `org-agenda-files'.")
(cl-defmethod org-dog-file-in-agenda-p ((x org-dog-file))
  (pcase (oref x agenda)
    (`nil nil)
    (`t t)
    ((and pattern (guard (stringp pattern)))
     (string-match-p pattern (oref x relative)))))

(cl-defgeneric org-dog-file-refile (file)
  "Refile the current Org entry to FILE.")
(cl-defgeneric org-dog-file-capture (file)
  "Capture an entry or text to FILE.")
(cl-defgeneric org-dog-file-search (file)
  "Search in FILE.")

;;;;; Utilities

(defun org-dog-maybe-file-buffer (file-obj)
  "Return a file buffer visiting X if any."
  (let ((file (oref file-obj absolute)))
    (find-buffer-visiting file)))

(defun org-dog-file-buffer (file-obj)
  "Return a file buffer visiting X."
  (let ((file (oref file-obj absolute)))
    (or (find-buffer-visiting file)
        (find-file-noselect file))))

(defun org-dog-file-title (file-obj &optional force)
  "Return the title of a file in its header."
  (or (oref file-obj title)
      (when-let (buffer (if force
                            (org-dog-file-buffer file-obj)
                          (org-dog-maybe-file-buffer file-obj)))
        (with-current-buffer buffer
          (org-with-wide-buffer
           (when-let (title (org-dog-file-header "title"))
             (oset file-obj title title)))))))

(defun org-dog-file-header (keyword &optional noprops)
  "Return the file headers of the Org current buffer.

Only interesting items are returned."
  ;; If you want to retrieve multiple keywords, it will be necessary to tweak
  ;; this function.
  (goto-char (point-min))
  (catch 'finish
    (while (or (looking-at (rx (* space) eol))
               (looking-at org-comment-regexp)
               (looking-at org-keyword-regexp))
      (when-let (key (match-string 1))
        (when (equal key keyword)
          (throw 'finish (if noprops
                             (substring-no-properties (match-string 2))
                           (match-string 2)))))
      (forward-line))
    nil))

;;;;; File operations

;;;###autoload
(defun org-dog-find-file (file)
  "Open an Org FILE."
  (interactive (list (org-dog-complete-file current-prefix-arg)))
  (find-file file))

;;;###autoload
(defun org-dog-search-in-file (file)
  "Open an Org FILE."
  (interactive (list (org-dog-complete-file current-prefix-arg)))
  (cl-etypecase file
    (string (org-dog-file-search (org-dog-file-object file)))
    (org-dog-file (org-dog-file-search file))))

;;;###autoload
(defun org-dog-refile-to-file (file)
  "Refile the current entry to FILE."
  (interactive (list (org-dog-complete-file current-prefix-arg)))
  (cl-etypecase file
    (string (org-dog-file-refile (org-dog-file-object file)))
    (org-dog-file (org-dog-file-refile file))))

;;;###autoload
(defun org-dog-capture-to-file (file)
  "Capture an entry to FILE."
  (interactive (list (org-dog-complete-file current-prefix-arg)))
  (cl-etypecase file
    (string (org-dog-file-capture (org-dog-file-object file)))
    (org-dog-file (org-dog-file-capture file))))

;;;###autoload
(defun org-dog-capture-to-this-file ()
  "Capture an entry to the current buffer."
  (interactive)
  (when-let (obj (org-dog-current-buffer-object))
    (org-dog-capture-to-file obj)))

;;;; Directories

;;;;; Class

(defclass org-dog-repository ()
  ((root :initarg :root)
   (directories :initarg :directories)))

(defun org-dog--repo-file-alist (repo)
  "Return an alist of repositories in REPO."
  (let ((root (oref repo root)))
    (cl-flet
        ((scan-subdir (dir)
           (let ((rel-dir (string-remove-prefix root dir)))
             (thread-last
               (directory-files dir nil "^[a-zA-Z].*\\.org\\(?:\\.gpg\\)?\\'" 'nosort)
               (cl-remove-if (lambda (name)
                               (when org-dog-exclude-file-pattern
                                 (string-match-p org-dog-exclude-file-pattern name))))
               (mapcar (lambda (file)
                         (list (concat dir file)
                               :root root
                               :relative (concat rel-dir file))))))))
      (thread-last
        (oref repo directories)
        (mapcar #'scan-subdir)
        (apply #'append)))))

;;;;; Instance management

(defun org-dog--init-repositories ()
  "Inititialize the list of directories."
  (thread-last org-dog-repository-alist
               (mapcar (pcase-lambda (`(,root . ,plist))
                         (when (file-directory-p root)
                           (apply #'org-dog--make-repository
                                  root plist))))
               (delq nil)
               (setq org-dog-repository-instances)))

(cl-defun org-dog--make-repository (root &key subdirs &allow-other-keys)
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
      (make-instance 'org-dog-repository
                     :root abbr-root
                     :directories (cons abbr-root real-subdirs)))))

(defun org-dog-reload-files (&optional arg)
  "Reload the file table.

When a universal prefix is given, the repositories are reloaded
as well."
  (interactive "P")
  (when (or (not org-dog-repository-instances)
            arg)
    (org-dog--init-repositories))
  (if (and org-dog-file-table (hash-table-p org-dog-file-table))
      (clrhash org-dog-file-table)
    (setq org-dog-file-table (make-hash-table :test #'equal)))
  (let ((error-count 0))
    (pcase-dolist (`(,absolute . ,plist) (thread-last
                                           org-dog-repository-instances
                                           (mapcar #'org-dog--repo-file-alist)
                                           (apply #'append)))
      (unless (gethash absolute org-dog-file-table)
        (let ((relative (plist-get plist :relative))
              (root (plist-get plist :root)))
          (unless (with-demoted-errors "Error while instantiating an object: %s"
                    (org-dog--make-file-instance :root root
                                                 :absolute absolute
                                                 :relative relative))
            (cl-incf error-count)))))
    (message "Registered %d Org files%s" (map-length org-dog-file-table)
             (if (> error-count 0)
                 (format " (%d errors)" error-count)
               ""))
    (when (bound-and-true-p org-dog-id-mode)
      (setq org-dog-id-files (map-keys org-dog-file-table)))
    org-dog-file-table))

(cl-defun org-dog--make-file-instance (&key root absolute relative)
  "Create an instance of `org-dog-file' or its subclass from a path.

Both ROOT and ABSOLUTE are required and should be passed from
inside the caller function.

RELATIVE is optional, and it can save little computation if
explicitly given. Maybe unnecessary."
  (cl-assert (and root absolute))
  (let* ((relative (or relative
                       (string-remove-prefix root absolute)))
         (route (org-dog--file-route root relative))
         (instance (apply #'make-instance (or (car route)
                                              org-dog-default-file-class)
                          :absolute absolute
                          :relative relative
                          :root root
                          (cdr route))))
    (puthash absolute instance org-dog-file-table)
    (if (org-dog-file-in-agenda-p instance)
        (add-to-list 'org-agenda-files absolute)
      (delq absolute org-agenda-files))
    instance))

(defun org-dog--file-route (root relative)
  "Return a route for a file in a repository, if any."
  (if-let (repo-entry (assoc root org-dog-repository-alist))
      (catch 'result
        (let ((rules (plist-get (cdr repo-entry) :routes)))
          (while rules
            (pcase-let ((`(,pattern . ,ent) (pop rules)))
              (when (or (and (stringp pattern)
                             (string-prefix-p pattern relative))
                        (and (not pattern)
                             (not (string-match-p "/" relative)))
                        (eq pattern t))
                (throw 'result ent))))))
    (error "Did not match root %s" root)))

;;;;; Utilities

(defun org-dog-subdirs-with-predicate (predicate root)
  "Return a list of subdirectories matching a predicate."
  (let ((predicate-fn (pcase predicate
                        ((pred functionp)
                         predicate)
                        (`(exclude ,pattern)
                         `(lambda (name)
                            (not (string-match-p ,pattern name)))))))
    (thread-last (directory-files-and-attributes root nil "^[a-z]")
                 (seq-filter (pcase-lambda (`(,_ ,dir . ,_))
                               dir))
                 (mapcar #'car)
                 (seq-filter predicate-fn)
                 (mapcar (lambda (name) (expand-file-name name root))))))

;;;; Contexts

(cl-defstruct org-dog-context file-whitelisted-p file-masked-p)

(defun org-dog-context (&optional force)
  (thread-last
    org-dog-context-alist
    (mapcar (pcase-lambda (`(,type . ,_))
              (org-dog-context-edge type force)))
    (delq nil)))

(defun org-dog-context-make-file-filter ()
  (apply-partially
   (lambda (predicates operand)
     (seq-reduce (lambda (cur p)
                   (when cur
                     (not (funcall p operand))))
                 predicates
                 t))
   (thread-last
     (org-dog-context)
     (mapcar (pcase-lambda (`(,_ . ,context))
               (apply-partially
                (lambda (context x)
                  (when context
                    (and (funcall (org-dog-context-file-masked-p context)
                                  x)
                         (not
                          (funcall (org-dog-context-file-whitelisted-p context)
                                   x)))))
                context)))
     (delq nil))))

(defun org-dog-context-edge (type &optional force arg)
  (let* ((plist (cdr (or (assq type org-dog-context-alist)
                         (error "No entry for %s in org-dog-context-alist" type))))
         (callback (or (plist-get plist :callback)
                       (message "Missing :callback for %s" type)))
         (arg (cond
               (arg arg)
               ((plist-get plist :value-fn)
                (funcall (plist-get plist :value-fn)))
               (t
                (or (buffer-local-value type (current-buffer))
                    (symbol-value type))))))
    (when arg
      (cons (cons type arg)
            (if force
                (let ((tbl (or (cdr (assq type org-dog-context-cache))
                               (org-dog-context--make-table type))))
                  (pcase (gethash arg tbl :dflt)
                    (:dflt
                     (when-let (context (funcall callback arg))
                       (let ((files (org-dog-select-files
                                     (org-dog-context-file-selected-p context))))
                         (puthash arg files tbl)
                         files)))
                    (`nil
                     nil)
                    (files
                     files)))
              (funcall callback arg))))))

(defun org-dog-context--make-table (type &optional test)
  (let ((tbl (make-hash-table :test (or test #'eq))))
    (push (cons type tbl) org-dog-context-cache)
    tbl))

(defun org-dog-context--flatten-to-alist (&optional context)
  (let (result)
    (cl-labels
        ((go (path triples)
           (pcase-dolist (`(,key ,value . ,children) triples)
             (let ((newpath (cons (cons key value) (reverse path))))
               (dolist (x children)
                 (if (object-of-class-p x 'org-dog-file)
                     (push (cons x (reverse newpath)) result)
                   (go newpath x)))))))
      (go nil (or context (org-dog-context))))
    (nreverse result)))

;;;;; Example context functions

(defun org-dog-project-context-1 (project)
  (require 'project)
  (pcase (file-name-split (abbreviate-file-name (project-root project)))
    (`("~" "work" ,_ ,group ,name "")
     (let ((regexp (rx-to-string `(and (or ,name ,group) (?  "-devel")))))
       (make-org-dog-context
        :file-whitelisted-p
        (org-dog-make-file-pred :relative-prefix "projects/"
                                :basename-regexp regexp)
        :file-masked-p
        (org-dog-make-file-pred :relative-prefix "projects/"))))))

(defun org-dog-major-mode-context-1 (mode)
  (let ((mode mode)
        filenames)
    (while mode
      (push (string-remove-suffix "-mode" (symbol-name mode))
            filenames)
      (setq mode (get mode 'derived-mode-parent)))
    (let ((regexp (rx-to-string `(or ,@filenames))))
      (make-org-dog-context
       :file-whitelisted-p
       (org-dog-make-file-pred :relative-prefix "programming/"
                               :basename-regexp regexp)
       :file-masked-p
       (org-dog-make-file-pred :relative-prefix "programming/")))))

(defun org-dog-language-context-1 (language)
  (let ((language (save-match-data
                    (pcase language
                      ((rx bol "UTF-")
                       "English")
                      ("ASCII"
                       "English")
                      ((rx bol (group (+ (not (any "-")))))
                       (match-string 1 language))))))
    (make-org-dog-context
     :file-whitelisted-p
     (org-dog-make-file-pred :relative-prefix "languages/"
                             :basename-regexp (regexp-quote language))
     :file-masked-p
     (org-dog-make-file-pred :relative-prefix "languages/"))))

;;;; Completion

(cl-defun org-dog-file-completion (&key class pred)
  "A completion function for `org-dog-file' matching a criteria.

If CLASS is specified, only files associated with an instance of
the class or its descendant are suggested as candidates.

The category will be CLASS is specified, or `org-dog-file' if omitted.

To customize the annotation, override `org-dog-annotate-file' method.

For a usage example, see the implementation of
`org-dog-complete-file'."
  (let* ((objs (org-dog-select-files (or pred
                                         (org-dog-make-file-pred :class class))))
         (files (mapcar (lambda (obj)
                          (let ((absolute (substring (oref obj absolute))))
                            (when-let (dir (file-name-directory absolute))
                              (put-text-property 0 (length dir)
                                                 'face 'org-dog-file-directory-face
                                                 absolute))
                            (put-text-property 0 (length (oref obj root))
                                               'invisible t
                                               absolute)
                            absolute))
                        objs)))
    ;; (mapc #'org-dog-get-annotation-info objs)
    `(lambda (string pred action)
       (if (eq action 'metadata)
           '(metadata . ((category . ,(or class 'org-dog-file))
                         (annotation-function . org-dog--annotate-file)))
         (complete-with-action action ',files string pred)))))

(defun org-dog--annotate-file (file)
  "Annotation function for `org-dog-file'."
  (when-let (entry (gethash file org-dog-file-table))
    (org-dog-annotate-file entry)))

(cl-defun org-dog-complete-file (no-context &optional prompt initial-input _history)
  "Complete an Org file."
  (completing-read (or prompt "Org file: ")
                   (org-dog-file-completion
                    :pred (when (and (not no-context)
                                     (cl-case org-dog-complete-contextually
                                       (t t)
                                       (clocking (org-clocking-p))))
                            (org-dog-context-make-file-filter)))
                   nil nil
                   initial-input org-dog-file-completion-history))

;;;; Links

(defun org-dog-follow-link (ref _arg)
  "Follow a link to an Org Dog file."
  (when-let (obj (org-dog--linked-object ref))
    (cl-etypecase obj
      (org-dog-file (find-file (oref obj absolute))))))

(defun org-dog--linked-object (ref)
  "Return an object referred to by REF."
  (org-dog-find-file-object (org-dog-make-file-pred :relative ref)))

(defun org-dog-complete-link (&optional _arg)
  "Complete a link to an Org Dog file."
  (let ((absolute (org-dog-complete-file t)))
    (concat "org-dog:" (oref (org-dog-file-object absolute) relative))))

(org-link-set-parameters "org-dog"
                         :follow #'org-dog-follow-link
                         :complete #'org-dog-complete-link)

;;;; Minor mode

;;;###autoload
(define-minor-mode org-dog-file-mode
  "Minor mode which can be activated in an Org Dog file.

For now, this is only used for enabling `org-dog-file-mode-map'."
  :lighter "Dog"
  (when org-dog-file-mode
    (cond
     ((org-dog-current-buffer-object)
      t)
     ((derived-mode-p 'org-mode)
      (let* ((filename (buffer-file-name (org-base-buffer (current-buffer))))
             (obj (org-dog-file-object filename))
             (message-log-max nil))
        (if obj
            (message "Generated a new object typed %s for %s"
                     (eieio-object-class-name obj)
                     filename)
          (org-dog-file-mode -1)
          (error "There is no route for %s, or the file is not in an repository."
                 filename))))
     (t
      (error "This mode must be turned on in an `org-mode' buffer.")))))

;;;###autoload
(defun org-dog-file-mode-maybe ()
  "Turn on `org-dog-file-mode' if possible.

You can add this function "
  (interactive)
  (let ((inhibit-message t))
    (ignore-errors
      (org-dog-file-mode t))))

;;;###autoload
(define-minor-mode org-dog-id-mode
  "Use Org Dog as the source of `org-id-extra-files'.

When this mode is on, `org-id-extra-files' is set to
`org-dog-id-files' and all files loaded by Org Dog are added to
the variable.

Alternatively, you can use `org-dog-update-id-locations' for
scanning dog files for IDs without modifying
`org-id-extra-files'."
  :global t
  :lighter "DogId"
  ;; TODO: More comprehensive cleanup (e.g. restoring and saving the original
  ;; value)
  (require 'org-id)
  (if org-dog-id-mode
      (progn
        (setq org-id-extra-files 'org-dog-id-files)
        (unless org-id-track-globally
          (setq org-id-track-globally t))
        (when (and (not org-dog-id-files)
                   org-dog-file-table)
          (setq org-dog-id-files (map-keys org-dog-file-table))))
    (setq org-id-extra-files
          (eval (car (get 'org-id-extra-files 'standard-value))))))

(defun org-dog-update-id-locations ()
  "Scan dog files for IDs.

This function calls `org-id-update-id-locations' for scanning IDs
with dog files as extra files. `org-id-extra-files' are scanned
as well, so you should call this when `org-dog-id-mode' is off.
If `org-dog-id-mode' is on, you should use
`org-id-update-id-locations' instead."
  (interactive)
  (unless org-dog-file-table
    (user-error "The files are not loaded yet"))
  (org-id-update-id-locations (map-keys org-dog-file-table)))

(provide 'org-dog)
;;; org-dog.el ends here

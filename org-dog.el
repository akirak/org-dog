;;; org-dog.el --- A programmable workflow layer for Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.5"))
;; Keywords: org outlines
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

;; FIXME

;;; Code:

(require 'eieio)
(require 'seq)
(require 'org)

;;;; Faces

(defface org-dog-file-directory-face
  '((t :foreground "#aaaadd"))
  "Face for directory components in filename completion.")

;;;; Files

;;;;; Class

(defclass org-dog-file ()
  ((absolute :initarg :absolute)
   (relative :initarg :relative)
   (root :initarg :root)
   (title :initform nil)))

(defcustom org-dog-default-file-class 'org-dog-file
  "")

;;;;; Instances

(defvar org-dog-file-table nil)

(cl-defun org-dog-files-matching (&key class)
  (let ((pred (if class
                  `(lambda (obj)
                     (object-of-class-p obj ',class))
                (lambda (_) t))))
    (thread-last (map-values org-dog-file-table)
                 (seq-filter pred))))

(defun org-dog-file-object (file)
  "Find a `org-dog-file' object associated with a FILE."
  (or (gethash file org-dog-file-table)
      (let ((abbr (abbreviate-file-name file)))
        (unless (equal abbr file)
          (gethash abbr org-dog-file-table))
        ;; TODO: Ensure loaded
        (when-let* ((repo (cl-find-if `(lambda (x)
                                         (string-prefix-p (oref x root)
                                                          ,abbr))
                                      org-dog-repository-instances))
                    (root (oref file root)))
          (org-dog--file-route root (string-remove-prefix root abbr))))
      (unless (file-readable-p file)
        (error "File %s is not readable" file))))

(cl-defun org-dog-find-file-object (slot value &key (test #'equal))
  "Find a file object where a slot satisfies a certain condition."
  (map-some (apply-partially
             (lambda (test slot value _key obj)
               (when (funcall test value (slot-value obj slot))
                 obj))
             test slot value)
            org-dog-file-table))

(defun org-dog-current-buffer-object ()
  (let* ((filename (abbreviate-file-name (buffer-file-name)))
         (obj (when filename
                (org-dog-file-object filename))))
    (when (and obj (org-dog-facade-datetree-file-p obj))
      obj)))

;;;;; Methods

(cl-defgeneric org-dog-annotate-file (x))
(cl-defmethod org-dog-annotate-file ((x org-dog-file))
  (let ((class (eieio-object-class x)))
    (concat (if (eq class org-dog-default-file-class)
                ""
              (concat " "
                      (propertize (thread-last
                                    (symbol-name class)
                                    (string-remove-prefix "org-dog-")
                                    (string-remove-suffix "-file"))
                                  'face 'font-lock-constant-face)))
            " "
            (propertize (oref x root) 'face 'font-lock-comment-face))))

(cl-defgeneric org-dog-file-refile (file))
(cl-defgeneric org-dog-file-capture (file))
(cl-defgeneric org-dog-file-search (file))

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
  (or (oref file-obj title)
      (when-let (buffer (org-dog-maybe-file-buffer file-obj))
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
  (interactive (list (org-dog-complete-file)))
  (find-file file))

;;;###autoload
(defun org-dog-search-in-file (file)
  "Open an Org FILE."
  (interactive (list (org-dog-complete-file)))
  (cl-etypecase file
    (string (org-dog-file-search (org-dog-file-object file)))
    (org-dog-file (org-dog-file-search file))))

;;;###autoload
(defun org-dog-refile-to-file (file)
  "Refile the current entry to FILE."
  (interactive (list (org-dog-complete-file)))
  (cl-etypecase file
    (string (org-dog-file-refile (org-dog-file-object file)))
    (org-dog-file (org-dog-file-refile file))))

;;;###autoload
(defun org-dog-capture-to-file (file)
  "Capture an entry to FILE."
  (interactive (list (org-dog-complete-file)))
  (cl-etypecase file
    (string (org-dog-file-capture (org-dog-file-object file)))
    (org-dog-file (org-dog-file-capture file))))

;;;; Directories

;;;;; Class

(defclass org-dog-repository ()
  ((root :initarg :root)
   (directories :initarg :directories)))

(defcustom org-dog-exclude-file-pattern
  (regexp-quote ".sync-conflict")
  ""
  :type 'string)

(defun org-dog--repo-file-alist (x)
  (let ((root (oref x root)))
    (cl-flet
        ((scan-subdir (dir)
           (let ((rel-dir (string-remove-prefix root dir)))
             (thread-last
               (directory-files dir nil "^[a-z].*\\.org\\(?:\\.gpg\\)?\\'" 'nosort)
               (cl-remove-if (lambda (name)
                               (when org-dog-exclude-file-pattern
                                 (string-match-p org-dog-exclude-file-pattern name))))
               (mapcar (lambda (file)
                         (list (concat dir file)
                               :root root
                               :relative (concat rel-dir file))))))))
      (thread-last
        (oref x directories)
        (mapcar #'scan-subdir)
        (apply #'append)))))

;;;;; Instance management

(defcustom org-dog-repository-alist nil
  ""
  :type '(alist :key-type directory
                :value-type plist))

(defvar org-dog-repository-instances nil)

(defun org-dog-load-repositories ()
  "Inititialize the list of directories."
  (interactive)
  (thread-last org-dog-repository-alist
               (mapcar (pcase-lambda (`(,root . ,plist))
                         (when (file-directory-p root)
                           (apply #'org-dog--make-repository
                                  root plist))))
               (delq nil)
               (setq org-dog-repository-instances)))

(cl-defun org-dog--make-repository (root &key subdirs &allow-other-keys)
  (cl-flet
      ((normalize-dir (dir)
         (file-name-as-directory (abbreviate-file-name dir))))
    (let ((abbr-root (normalize-dir root))
          (real-subdirs (thread-last
                          (cl-etypecase subdirs
                            (function
                             (funcall subdirs root))
                            (list
                             (thread-last subdirs
                                          (mapcar (lambda (str) (expand-file-name str root)))
                                          (cl-remove-if-not #'file-directory-p))))
                          (mapcar #'normalize-dir))))
      (make-instance 'org-dog-repository
                     :root abbr-root
                     :directories (cons abbr-root real-subdirs)))))

(defun org-dog-reload-files ()
  (interactive)
  (unless org-dog-repository-instances
    (org-dog-load-repositories))
  (if (and org-dog-file-table (hash-table-p org-dog-file-table))
      (clrhash org-dog-file-table)
    (setq org-dog-file-table (make-hash-table :test #'equal)))
  (pcase-dolist (`(,absolute . ,plist) (thread-last
                                         org-dog-repository-instances
                                         (mapcar #'org-dog--repo-file-alist)
                                         (apply #'append)))
    (unless (gethash absolute org-dog-file-table)
      (let* ((relative (plist-get plist :relative))
             (root (plist-get plist :root))
             (route (org-dog--file-route root relative)))
        (puthash absolute
                 (apply #'make-instance (or (car route)
                                            org-dog-default-file-class)
                        :absolute absolute
                        :relative relative
                        :root root
                        (cdr route))
                 org-dog-file-table))))
  org-dog-file-table)

(defun org-dog--file-route (root relative)
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

;;;; Completion

(cl-defun org-dog-file-completion (&key class)
  (unless org-dog-file-table
    (org-dog-reload-files))
  (let* ((objs (org-dog-files-matching :class class))
         (files (mapcar (lambda (obj)
                          (let ((absolute (oref obj absolute)))
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
  (when-let (entry (gethash file org-dog-file-table))
    (org-dog-annotate-file entry)))

(defvar org-dog-file-completion-history nil)

(defun org-dog-complete-file (&optional prompt initial-input _history)
  (completing-read (or prompt "Org file: ")
                   (org-dog-file-completion)
                   nil nil
                   initial-input org-dog-file-completion-history))

(provide 'org-dog)
;;; org-dog.el ends here

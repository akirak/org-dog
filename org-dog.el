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

(require 'org-dog-core)

(declare-function org-link-set-parameters "ext:ol")
(declare-function org-element-property "ext:org-element")
(declare-function org-element-type "ext:org-element")
(declare-function project-root "ext:project")
(defvar org-id-extra-files)
(defvar org-id-track-globally)

;;;; Custom variables

(defcustom org-dog-file-mode-hook nil
  "Hook called when `org-dog-file-mode' is turned on.

When this hook is called, `org-dog-buffer-file-object' should
have been already set to the object of the buffer file."
  :group 'org-dog
  :type 'hook)

;;;; Faces

(defface org-dog-file-directory-face
  '((t :foreground "#aaaadd"))
  "Face for sub-directory components in completion of `org-dog-file' category."
  :group 'org-dog)

(defface org-dog-file-class-face
  '((t :inherit font-lock-constant-face))
  "Face for class names in completion of `org-dog-file' category."
  :group 'org-dog)

(defface org-dog-repository-face
  '((t :inherit font-lock-comment-face))
  "Face for repository directories in completion of `org-dog-file' category."
  :group 'org-dog)

;;;; Variables

(defvar org-dog-file-completion-history nil)

(defvar org-dog-file-mode-map (make-sparse-keymap)
  "Keymap for `org-dog-file-mode'.")

(defvar org-dog-id-files nil
  "List used to track Org files in `org-dog-id-mode'.")

(defvar org-dog--indirect-buffers nil
  "Hash table for tracking Org indirect buffers.

The key should be an id property of the first Org entry in each buffer.")

(defvar org-dog-buffer-file-object nil
  "Object for the file of the buffer.

This variable is intended for use in the mode-line constructs,
and it is only set in `org-dog-file-mode'. If the file is not
tracked by dog, it is nil.")

(defvar org-dog-indirect-buffer-p nil
  "Whether the current buffer is an indirect buffer.

This variable is intended for use in the mode-line constructs,
and it is only set in `org-dog-file-mode'.")

;;;; Associating the file object with a buffer

(defun org-dog-buffer-object ()
  "Return the `org-dog-file' object for the current buffer, if any."
  (when-let (filename (buffer-file-name (org-base-buffer (current-buffer))))
    (org-dog-file-object (abbreviate-file-name filename))))

(defun org-dog-maybe-file-buffer (file-obj)
  "Return a file buffer visiting X if any."
  (let ((file (oref file-obj absolute)))
    (find-buffer-visiting file)))

(defun org-dog-file-buffer (file-obj)
  "Return a file buffer visiting X."
  (let ((file (oref file-obj absolute)))
    (or (find-buffer-visiting file)
        (find-file-noselect file))))

;;;###autoload
(define-minor-mode org-dog-file-mode
  "Minor mode which can be activated in an Org Dog file.

For now, this is only used for enabling `org-dog-file-mode-map'."
  :lighter " Dog"
  (when org-dog-file-mode
    (let ((obj (or (org-dog-buffer-object)
                   (org-dog--new-object))))
      (unless obj
        (org-dog-file-mode -1)
        (error "There is no route for %s, or the file is not in an repository."
               (buffer-file-name (org-base-buffer (current-buffer)))))
      (org-dog--set-file-identity obj)
      (add-hook 'clone-indirect-buffer-hook #'org-dog--set-file-identity
                nil t)
      (add-hook 'after-set-visited-file-name-hook #'org-dog--handle-file-rename
                nil t)
      (run-hooks 'org-dog-file-mode-hook))))

(defun org-dog--set-file-identity (&optional obj)
  (setq-local org-dog-buffer-file-object (or obj (org-dog-buffer-object))
              org-dog-indirect-buffer-p (when (buffer-base-buffer)
                                          t)))

(defun org-dog--handle-file-rename ()
  "Update the file table after the file has been renamed."
  (when org-dog-buffer-file-object
    (let ((old-path (oref org-dog-buffer-file-object absolute)))
      (unwind-protect
          ;; Regenerate a new object and update the identity
          (org-dog-file-mode t)
        (unless (equal old-path (oref org-dog-buffer-file-object absolute))
          (map-delete org-dog--file-table old-path))))))

(defun org-dog--new-object ()
  "Return a new object for the buffer."
  (unless (derived-mode-p 'org-mode)
    (error "This mode must be turned on in an `org-mode' buffer."))
  (when-let (obj (thread-last
                   (org-base-buffer (current-buffer))
                   (buffer-file-name)
                   (org-dog-file-object)))
    (when obj
      (let ((message-log-max nil))
        (message "Generated a new object typed %s for %s"
                 (eieio-object-class-name obj)
                 (oref obj-generated absolute)))
      obj)))

;;;###autoload
(defun org-dog-file-mode-maybe ()
  "Turn on `org-dog-file-mode' if possible.

You can add this function "
  (interactive)
  (let ((inhibit-message t))
    (ignore-errors
      (org-dog-file-mode t))))

;;;; File operations

;;;;; Generic methods

;; For every subclass of `org-dog-file', the user needs to implement these
;; methods.

(cl-defgeneric org-dog-file-refile (file)
  "Refile the current Org entry to FILE.")

(cl-defgeneric org-dog-file-capture-templates (file)
  "Return `org-capture-templates' to the file.")

(cl-defgeneric org-dog-file-capture-template-names (file)
  "Return an list of keys and names of the capture templates.

This is mostly for optimization."
  (mapcar (lambda (template) (seq-take templates 2))
          (org-dog-file-capture-templates)))

(cl-defgeneric org-dog-file-capture-entry (file key)
  (assoc key (org-dog-file-capture-templates file)))

(cl-defgeneric org-dog-file-search (file)
  "Search in FILE.")

;;;;; Interactive functions

;;;###autoload
(defun org-dog-find-file (file)
  "Open an Org FILE."
  (interactive (list (org-dog-complete-file)))
  (if (file-name-absolute-p file)
      (find-file file)
    (let ((repo-root (completing-read (format "Choose a repository for %s: " file)
                                      (map-keys org-dog--repository-table)
                                      nil t)))
      (find-file (expand-file-name file repo-root)))))

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
    (string (org-dog-file-capture-templates (org-dog-file-object file)))
    (org-dog-file
     (let ((org-capture-templates (org-dog-file-capture-templates file))
           org-capture-templates-contexts)
       (org-capture)))))

;;;###autoload
(defun org-dog-capture-to-this-file ()
  "Capture an entry to the current buffer."
  (interactive)
  (when-let (obj (org-dog-buffer-object))
    (org-dog-capture-to-file obj)))

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

(cl-defun org-dog-complete-file (&optional prompt initial-input _history)
  "Complete an Org file.

It returns an absolute path if the user selects a candidate. It
is also possible for the user to enter a relative path that does
not exist in the candidate, so the caller of this function should
properly handle it."
  (completing-read (or prompt "Org file: ")
                   (org-dog-file-completion)
                   nil nil
                   initial-input org-dog-file-completion-history))

(defun org-dog--annotate-file (file)
  "Annotation function for `org-dog-file'."
  (when-let (obj (gethash file org-dog--file-table))
    (let ((class (eieio-object-class obj)))
      (concat (if (eq class org-dog-default-file-class)
                  ""
                (concat " "
                        (propertize (thread-last
                                      (symbol-name class)
                                      (string-remove-prefix "org-dog-")
                                      (string-remove-suffix "-file"))
                                    'face 'org-dog-file-class-face)))
              " "
              (propertize (oref obj root) 'face 'org-dog-repository-face)))))

;;;; Miscellaneous utilities for convenience of users

(defun org-dog-symbol-value (x)
  "If X is a symbol, return its value."
  (cl-typecase x
    (null nil)
    (symbol (symbol-value x))
    (otherwise x)))

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

(defun org-dog-file-title (file-obj &optional force)
  "Return the title of a file in its header."
  (when-let (buffer (if force
                        (org-dog-file-buffer file-obj)
                      (org-dog-maybe-file-buffer file-obj)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (when-let (title (org-dog-file-header "title"))
         (oset file-obj title title)
         title)))))

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

;;;; Links

(defun org-dog-follow-link (ref _arg)
  "Follow a link to an Org Dog file."
  (let ((obj (org-dog--linked-object ref)))
    (if obj
        (cl-etypecase obj
          (org-dog-file
           (let ((file (oref obj absolute)))
             (find-file file)
             (widen)
             (goto-char (point-min))
             (while (or (org-at-comment-p)
                        (org-at-keyword-p)
                        (looking-at (rx eol)))
               (forward-line)))))
      (when noninteractive
        (error "Dead link: %s" ref))
      (when (yes-or-no-p (format-message "%s is a dead link. Create the file? " ref))
        (org-dog-find-file ref)))))

(defun org-dog--linked-object (ref)
  "Return an object referred to by REF."
  (org-dog-find-file-object (org-dog-make-file-pred :relative ref)))

(defun org-dog-complete-link (&optional _arg)
  "Complete a link to an Org Dog file."
  (let* ((path (org-dog-complete-file))
         (object (org-dog-file-object path :allow-missing t))
         (relative (if object
                       (oref object relative)
                     path)))
    (concat "org-dog:" relative)))

(org-link-set-parameters "org-dog"
                         :follow #'org-dog-follow-link
                         :complete #'org-dog-complete-link)

;;;; Integrating with org-id.el

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
  :lighter " DogId"
  :group 'org-dog
  ;; TODO: More comprehensive cleanup (e.g. restoring and saving the original
  ;; value)
  (require 'org-id)
  (if org-dog-id-mode
      (progn
        (setq org-id-extra-files 'org-dog-id-files)
        (unless org-id-track-globally
          (setq org-id-track-globally t))
        (when (and (not org-dog-id-files)
                   org-dog--file-table)
          (setq org-dog-id-files (map-keys org-dog--file-table)))
        (add-hook 'org-dog-file-registration-hook #'org-dog-id--add-file))
    (setq org-id-extra-files
          (eval (car (get 'org-id-extra-files 'standard-value))))
    (remove-hook 'org-dog-file-registration-hook #'org-dog-id--add-file)))

(defun org-dog-id--add-file (file-obj)
  "Add the path to FILE-OBJ to `org-dog-id-files'.

This is used in `org-dog-id-mode'."
  (add-to-list 'org-dog-id-files (oref file-obj absolute)))

(defun org-dog-update-id-locations ()
  "Scan dog files for IDs.

This function calls `org-id-update-id-locations' for scanning IDs
with dog files as extra files. `org-id-extra-files' are scanned
as well, so you should call this when `org-dog-id-mode' is off.
If `org-dog-id-mode' is on, you should use
`org-id-update-id-locations' instead."
  (interactive)
  (unless org-dog--file-table
    (user-error "The files are not loaded yet"))
  (org-id-update-id-locations (map-keys org-dog--file-table)))

;;;; Manage indirect buffers for Org subtrees

(defun org-dog-indirect-buffer (&optional entry no-reuse)
  "Return an indirect buffer for the subtree.

This function gets or creates an indirect buffer narrowed to the
subtree of an entry. If there is an existing indirect buffer, it
will be reused. It is useful for preventing from creating
duplicate indirect buffers for the same entry.

ENTRY can be a marker or an org-element with a
marker (:org-hd-marker or :org-marker). If it is omitted, it will
be the current Org entry.

If NO-REUSE is non-nil, a new buffer is created unconditionally,
even if there is an existing one. This prevents motion of the
point, so it is useful for narrowing to the current Org entry.

The comparison is made by the existing ID property, so it can
create multiple buffers if the entry has no ID."
  (unless org-dog--indirect-buffers
    (setq org-dog--indirect-buffers (make-hash-table :test #'equal)))
  (pcase-let*
      ((element-marker (and entry
                            (sequencep entry)
                            (eq 'headline (org-element-type entry))
                            (or (org-element-property :org-hd-marker entry)
                                (org-element-property :org-marker entry))))
       (marker (or element-marker
                   (cond
                    ((markerp entry)
                     entry)
                    ((numberp entry)
                     (copy-marker entry))
                    (t
                     (point-marker)))))
       (`(,id ,headline) (if element-marker
                             (list (org-element-property :ID entry)
                                   (org-element-property :raw-value entry))
                           (org-with-point-at marker
                             (list (org-id-get)
                                   (org-get-heading t t t t)))))
       (buffer (when (and id (not no-reuse))
                 (gethash id org-dog--indirect-buffers))))
    (unless (and buffer
                 (bufferp buffer)
                 (buffer-live-p buffer))
      ;; Create a new indirect buffer
      (setq buffer (org-with-point-at marker
                     (with-current-buffer
                         (org-get-indirect-buffer
                          nil headline)
                       (org-narrow-to-subtree)
                       (current-buffer))))
      (when id
        (puthash id buffer org-dog--indirect-buffers)))
    buffer))

(defun org-dog-indirect-buffers ()
  "Return a list of live indirect buffers."
  (when org-dog--indirect-buffers
    (thread-last
      (map-filter (lambda (_id buffer)
                    (buffer-live-p buffer))
                  org-dog--indirect-buffers)
      (mapcar #'cdr))))

(provide 'org-dog)
;;; org-dog.el ends here

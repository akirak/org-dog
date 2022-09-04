;;; octopus.el --- Transient commands for Org Dog -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org-dog "0.1") (transient "0.3"))
;; Keywords: convenience
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

;;

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'transient)
(require 'org-dog)
(require 'org-dog-context)

(defgroup octopus nil
  "Transient commands for Org Dog."
  :group 'org-dog
  :group 'transient)

(defcustom octopus-static-file-list nil
  ""
  :type '(repeat (list (string :tag "Key")
                       (string :tag "Relative path or absolute path"))))

;;;; Generic

;;;;; Generic functions

(cl-defgeneric octopus--dispatch (command target))

;;;;; octopus-boolean-variable

(defclass octopus-boolean-variable (transient-variable)
  ((variable :initarg :variable)
   (default :initarg :default :initform nil)
   (format :initform " %k %v")))

(cl-defmethod transient-init-value ((obj octopus-boolean-variable))
  (let ((value (eval (oref obj default))))
    (oset obj value value)
    (set (oref obj variable) value)))

(cl-defmethod transient-infix-read ((obj octopus-boolean-variable))
  (not (oref obj value)))

(cl-defmethod transient-infix-set ((obj octopus-boolean-variable) value)
  (set (oref obj variable) (oset obj value value)))

(cl-defmethod transient-format ((obj octopus-boolean-variable))
  (format-spec (oref obj format)
               `((?k . ,(transient-format-key obj))
                 (?v . ,(transient-format-value obj)))))

(cl-defmethod transient-format-value ((obj octopus-boolean-variable))
  (propertize (format "%s" (oref obj description))
              'face (if (oref obj value)
                        'transient-value
                      'transient-inactive-value)))

;;;; Suffixes and infixes

;;;;; Display options

(defvar octopus-other-window nil)

(transient-define-infix octopus-infix-other-window ()
  :class 'octopus-boolean-variable
  :variable 'octopus-other-window
  :default nil
  :description "Other window")

;;;;; Static files

(defun octopus-setup-static-targets (_children)
  "Return a list of static file children as suffixes."
  (mapcar (pcase-lambda (`(,key ,filename))
            (let ((symbol (intern (format "octopus--static-file-%s" key))))
              (fset symbol
                    `(lambda ()
                       (interactive)
                       (octopus--run-file-suffix ,filename)))
              (put symbol 'interactive-only t)
              `(,transient--default-child-level
                transient-suffix
                ,(list :key key
                       :description (file-name-nondirectory filename)
                       :command symbol))))
          octopus-static-file-list))

(defun octopus--run-file-suffix (filename)
  (octopus--dispatch (oref transient-current-prefix command)
                     (if (file-name-absolute-p filename)
                         filename
                       (org-dog-resolve-relative-file filename))))

;;;;; This file

(transient-define-suffix octopus-in-file-suffix ()
  :if (lambda () (derived-mode-p 'org-mode))
  :description "This file"
  (interactive)
  (octopus--dispatch (oref transient-current-prefix command)
                     (buffer-file-name)))

;;;;; Contexts

(cl-defmacro octopus-define-context (name &key context-key
                                          description-label
                                          description-body
                                          files-suffix
                                          setup-suffix
                                          initial-key)
  (declare (indent 1))
  (let ((var-sym (intern (format "octopus--%s-context" name)))
        (predicate-sym (intern (format "octopus--%s-p" name)))
        (description-sym (intern (format "octopus--%s-description" name))))
    `(progn
       (defvar ,var-sym nil)

       (defun ,predicate-sym ()
         (let ((ctx (org-dog-context-edge ',context-key)))
           (setq ,var-sym ctx)
           (cdr ctx)))

       (defun ,description-sym ()
         (format "%s: %s" ,description-label ,description-body))

       (defun ,setup-suffix (children)
         (let* ((files (thread-last
                         (cdr ,var-sym)
                         (org-dog-context-file-objects)
                         (mapcar (lambda (obj) (oref obj absolute)))))
                (files (thread-last
                         (org-dog-overview-scan files :fast t)
                         (mapcar #'car)
                         (reverse)))
                (singletonp (= (length files) 1))
                (i 0)
                result)
           (dolist (file files)
             (let ((symbol (intern (format "octopus--context-file-suffix-%s-%d"
                                           ,initial-key i)))
                   ;; (absolute (oref obj absolute))
                   ;; (relative (oref obj relative))
                   (key (concat ,initial-key (cond
                                              (singletonp "")
                                              ((= i 0) ,initial-key)
                                              (t (number-to-string i))))))
               (fset symbol
                     `(lambda ()
                        (interactive)
                        (octopus--run-file-suffix ,file)))
               (put symbol 'interactive-only t)
               (push `(,transient--default-child-level
                       transient-suffix
                       ,(list :key key
                              :description file
                              :command symbol))
                     result)
               (cl-incf i)))
           (nreverse result)))

       (transient-define-suffix ,files-suffix ()
         :description ,(format "%s Files" description-label)
         :if #',predicate-sym
         (interactive)
         (if-let (files (thread-last
                          (org-dog-overview-scan
                           (thread-last
                             (cdr ,var-sym)
                             (org-dog-context-file-objects)
                             (mapcar (lambda (obj) (oref obj absolute))))
                           :fast t)
                          (mapcar #'car)
                          (reverse)))
             (octopus--dispatch (oref transient-current-prefix command)
                                files)
           (user-error "No file in the context"))))))

(octopus-define-context "project"
  :context-key project
  :initial-key "p"
  :description-label "Project"
  :description-body (thread-last
                      (cdar octopus--project-context)
                      (project-root)
                      (octopus--abbr-file-name))
  :files-suffix octopus-project-files-suffix
  :setup-suffix octopus-setup-project-file-targets)

(octopus-define-context "major-mode"
  :context-key major-mode
  :initial-key "m"
  :description-label "Major Mode"
  :description-body (cdar octopus--major-mode-context)
  :files-suffix octopus-major-mode-files-suffix
  :setup-suffix octopus-setup-major-mode-file-targets)

(octopus-define-context "path"
  :context-key path
  :initial-key "f"
  :description-label "File Path"
  :description-body (thread-last
                      (cdar octopus--path-context)
                      (octopus--abbr-file-name))
  :files-suffix octopus-path-files-suffix
  :setup-suffix octopus-setup-path-file-targets)

(octopus-define-context "language"
  :context-key language
  :initial-key "l"
  :description-label "Language"
  :description-body (cdar octopus--language-context)
  :files-suffix octopus-language-files-suffix
  :setup-suffix octopus-setup-language-file-targets)

(defun octopus-setup-context-file-subgroups (_children)
  (mapcar (lambda (spec)
            (vector 1 transient-column spec))
          '((:description
             octopus--project-description
             :if octopus--project-p
             :setup-children octopus-setup-project-file-targets)
            (:description
             octopus--major-mode-description
             :if octopus--major-mode-p
             :setup-children octopus-setup-major-mode-file-targets)
            (:description
             octopus--path-description
             :if octopus--path-p
             :setup-children octopus-setup-path-file-targets)
            (:description
             octopus--language-description
             :if octopus--language-p
             :setup-children octopus-setup-language-file-targets))))

(defun octopus-setup-context-files-targets (_children)
  (mapcar (pcase-lambda (`(,key ,suffix))
            `(,transient--default-child-level
              transient-suffix
              ,(list :key key
                     :command suffix)))
          '(("p" octopus-project-files-suffix)
            ("m" octopus-major-mode-files-suffix)
            ("f" octopus-path-files-suffix)
            ("l" octopus-language-files-suffix))))

;;;;; Refile

(defvar octopus-refile-to-datetree nil)

(transient-define-infix octopus-infix-refile-to-datetree ()
  :class 'octopus-boolean-variable
  :variable 'octopus-refile-to-datetree
  :description "To datetree")

;;;;; Prompt

(transient-define-suffix octopus-read-dog-file-suffix ()
  :description "Prompt"
  (interactive)
  (octopus--dispatch (oref transient-current-prefix command)
                     (org-dog-complete-file)))

;;;;; Current file

(transient-define-suffix octopus-this-file-suffix ()
  :description "This file"
  :if (lambda () (and (derived-mode-p 'org-mode)
                      (octopus--base-buffer-file)))
  (interactive)
  (octopus--dispatch (oref transient-current-prefix command)
                     (octopus--base-buffer-file)))

(defun octopus--base-buffer-file ()
  "Return the file name for the base buffer."
  (buffer-file-name (org-base-buffer (current-buffer))))

;;;;; Clock

(transient-define-suffix octopus-clock-marker-suffix ()
  :description "Clock"
  :if #'org-clocking-p
  (interactive)
  (octopus--dispatch (oref transient-current-prefix command)
                     org-clock-marker))

;;;; Prefix commands

;;;###autoload (autoload 'octopus-find-file "octopus" nil t)
(transient-define-prefix octopus-find-file ()
  ["Options"
   ("-o" octopus-infix-other-window)]
  ["Context"
   :class transient-columns
   :setup-children octopus-setup-context-file-subgroups]
  ["Static targets"
   :class transient-row
   :setup-children octopus-setup-static-targets]
  ["Other targets"
   :class transient-row
   ("/" octopus-read-dog-file-suffix)]
  (interactive)
  (transient-setup 'octopus-find-file))

(cl-defmethod octopus--dispatch ((_cmd (eql 'octopus-find-file))
                                 target)
  (funcall (if octopus-other-window
               #'find-file-other-window
             #'find-file)
           (cl-etypecase target
             (org-dog-file (oref target absolute))
             (string target))))

;;;###autoload (autoload 'octopus-find-node "octopus" nil 'interactive)
(transient-define-prefix octopus-find-node ()
  ["Context"
   :class transient-row
   :setup-children octopus-setup-context-files-targets]
  ["Static targets"
   :class transient-row
   :setup-children octopus-setup-static-targets]
  ["Other targets"
   :class transient-row
   ("/" octopus-read-dog-file-suffix)]
  (interactive)
  (transient-setup 'octopus-find-node))

(cl-defmethod octopus--dispatch ((_cmd (eql 'octopus-find-node))
                                 files)
  (org-ql-find files))

;;;###autoload (autoload 'octopus-refile "octopus" nil 'interactive)
(transient-define-prefix octopus-refile ()
  ["Options"
   ("-d" octopus-infix-refile-to-datetree)]
  ["This file"
   ("." octopus-in-file-suffix)]
  ["Other targets"
   :class transient-row
   ("'" "Avy" avy-org-refile-as-child)
   ("/" octopus-read-dog-file-suffix)
   ("@" octopus-clock-marker-suffix)]
  (interactive)
  (unless (derived-mode-p 'org-mode 'org-agenda-mode)
    (user-error "Cannot run in this mode"))
  (transient-setup 'octopus-refile))

(cl-defmethod octopus--dispatch ((_cmd (eql 'octopus-refile))
                                 target)
  (if (markerp target)
      (org-refile nil nil
                  (org-with-point-at target
                    (list (org-get-heading t t t t)
                          (buffer-file-name (marker-buffer target))
                          nil
                          (marker-position target))))
    (if octopus-refile-to-datetree
        (org-dog-datetree-refile target)
      (org-dog-refile-to-file target))))

;;;; Other utilities

(defvar octopus--path-separator nil)

(defun octopus--path-separator ()
  (or octopus--path-separator
      (setq octopus--path-separator
            (string-remove-prefix "a" (file-name-as-directory "a")))))

(defun octopus--abbr-file-name (file)
  "Return an aggressively abbreviated path to FILE."
  (let* ((sep (octopus--path-separator))
         (endsep (string-suffix-p sep file))
         (segs (cl-remove-if #'string-empty-p (split-string file sep))))
    (thread-first
      (mapcar (lambda (s)
                (if (string-match (rx (* (not (any alnum))) (any alnum)) s)
                    (match-string 0 s)
                  s))
              (butlast segs))
      (append (last segs))
      (string-join sep)
      (concat (if endsep sep "")))))

(provide 'octopus)
;;; octopus.el ends here

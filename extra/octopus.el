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
                       :description filename
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
         (let* ((objects (thread-last
                           (cdr ,var-sym)
                           (org-dog-context-file-objects)))
                (singletonp (= (length objects) 1))
                (i 0)
                result)
           (dolist (obj objects)
             (let ((symbol (intern (format "octopus--context-file-suffix-%s-%d" ,initial-key i)))
                   (absolute (oref obj absolute))
                   (relative (oref obj relative))
                   (key (concat ,initial-key (cond
                                              (singletonp "")
                                              ((= i 0) ,initial-key)
                                              (t (number-to-string i))))))
               (fset symbol
                     `(lambda ()
                        (interactive)
                        (octopus--run-file-suffix ,absolute)))
               (put symbol 'interactive-only t)
               (push `(,transient--default-child-level
                       transient-suffix
                       ,(list :key key
                              :description relative
                              :command symbol))
                     result)
               (cl-incf i)))
           (nreverse result))))))

(octopus-define-context "project"
  :context-key project
  :initial-key "p"
  :description-label "Project"
  :description-body (project-root (cdar octopus--project-context))
  :setup-suffix octopus-setup-project-file-targets)

(octopus-define-context "major-mode"
  :context-key major-mode
  :initial-key "m"
  :description-label "Major Mode"
  :description-body (cdar octopus--major-mode-context)
  :setup-suffix octopus-setup-major-mode-file-targets)

(octopus-define-context "path"
  :context-key path
  :initial-key "f"
  :description-label "File Path"
  :description-body (cdar octopus--path-context)
  :setup-suffix octopus-setup-path-file-targets)

(octopus-define-context "language"
  :context-key language
  :initial-key "l"
  :description-label "Language"
  :description-body (cdar octopus--language-context)
  :setup-suffix octopus-setup-language-file-targets)

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

;;;; Prefix commands

;;;###autoload (autoload 'octopus-find-file "octopus" nil t)
(transient-define-prefix octopus-find-file ()
  ["Options"
   ("-o" octopus-infix-other-window)]
  ["Context"
   :class transient-columns
   [:description
    octopus--project-description
    :if octopus--project-p
    :setup-children octopus-setup-project-file-targets]
   [:description
    octopus--major-mode-description
    :if octopus--major-mode-p
    :setup-children octopus-setup-major-mode-file-targets]
   [:description
    octopus--path-description
    :if octopus--path-p
    :setup-children octopus-setup-path-file-targets]
   [:description
    octopus--language-description
    :if octopus--language-p
    :setup-children octopus-setup-language-file-targets]]
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

;;;###autoload (autoload 'octopus-refile "octopus" nil 'interactive)
(transient-define-prefix octopus-refile ()
  ["Options"
   ("-d" octopus-infix-refile-to-datetree)]
  ["This file"
   ("." octopus-in-file-suffix)]
  ["Other targets"
   :class transient-row
   ("'" "Avy" avy-org-refile-as-child)
   ("/" octopus-read-dog-file-suffix)]
  (interactive)
  (unless (derived-mode-p 'org-mode 'org-agenda-mode)
    (user-error "Cannot run in this mode"))
  (transient-setup 'octopus-refile))

(cl-defmethod octopus--dispatch ((_cmd (eql 'octopus-refile))
                                 target)
  (if octopus-refile-to-datetree
      (org-dog-datetree-refile target)
    (org-dog-refile-to-file target)))

(provide 'octopus)
;;; octopus.el ends here

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
(require 'org-clock)
(require 'org-dog)
(require 'org-dog-context)
(require 'org-dog-overview)
(require 'project)

(declare-function org-dog-datetree-refile "ext:org-dog-datetree")
(declare-function org-ql-find "ext:org-ql-find")
(declare-function org-ql-completing-read "ext:org-ql")
(declare-function org-super-links-store-link "ext:org-super-links")
(declare-function org-super-links-insert-link "ext:org-super-links")
(declare-function org-dog-clock-in "org-dog-clock")
(declare-function avy-action-goto "ext:avy")
(declare-function avy-jump "ext:avy")
(declare-function avy-with "ext:avy")
(declare-function avy-org-refile-as-child "ext:avy")
(declare-function org-agenda-get-any-marker "org-agenda")
(declare-function org-ql-search "ext:org-ql-search")
(declare-function thing-at-point-looking-at "thingatpt")
(defvar org-capture-last-stored-marker)
(defvar avy-goto-line)

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

(cl-defmethod octopus--dispatch ((_cmd (eql nil))
                                 _target)
  (message "octopus--dispatch: nil command is passed. transient-current-command: %s"
           transient-current-command)
  (transient--exit-and-debug))

(defun octopus-current-command ()
  "Return the current transient command."
  (oref (or transient--prefix
            transient-current-prefix
            (error "No transient-current-prefix"))
        command))

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

;;;;; octopus-completion

(defclass octopus-completion (transient-variable)
  ((variable :initarg :variable)
   (prompt :initarg :prompt)
   (table :initarg :table)))

(cl-defmethod transient-init-value ((obj octopus-completion))
  (let ((value (symbol-value (oref obj variable))))
    (oset obj value value)
    (set (oref obj variable) value)))

(cl-defmethod transient-infix-read ((obj octopus-completion))
  (if-let (value (oref obj value))
      nil
    (let ((table (oref obj table)))
      (completing-read (oref obj prompt)
                       (if (and (not (functionp table))
                                (symbolp table))
                           (symbol-value table)
                         table)))))

(cl-defmethod transient-infix-set ((obj octopus-completion) value)
  (set (oref obj variable) (oset obj value value)))

(cl-defmethod transient-format-value ((obj octopus-completion))
  (let ((value (oref obj value)))
    (concat (propertize "(" 'face 'transient-inactive-value)
            (if value
                (propertize value 'face 'transient-value)
              "")
            (propertize ")" 'face 'transient-inactive-value))))

;;;;; octopus-multiple-choice

(defclass octopus-multiple-choice (transient-variable)
  ((variable :initarg :variable)
   (choices-variable :initarg :choices-variable)))

(cl-defmethod transient-init-value ((obj octopus-multiple-choice))
  (let ((value (car (symbol-value (oref obj choices-variable)))))
    (oset obj value value)
    (set (oref obj variable) value)))

(cl-defmethod transient-infix-read ((obj octopus-multiple-choice))
  (read-multiple-choice (oref obj prompt)
                        (symbol-value (oref obj choices-variable))))

(cl-defmethod transient-infix-set ((obj octopus-multiple-choice) value)
  (set (oref obj variable) (oset obj value value)))

(cl-defmethod transient-format-value ((obj octopus-multiple-choice))
  (if-let (value (oref obj value))
      (concat
       (propertize "(" 'face 'transient-inactive-value)
       (propertize (format "%s" (nth 1 value))
                   'face 'transient-value)
       (propertize ")" 'face 'transient-inactive-value))
    ""))

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
  (octopus--dispatch (octopus-current-command)
                     (if (file-name-absolute-p filename)
                         filename
                       (org-dog-resolve-relative-file filename))))

;;;;; This file

(transient-define-suffix octopus-in-file-suffix ()
  :if (lambda () (derived-mode-p 'org-mode))
  :description "This file"
  (interactive)
  (octopus--dispatch (octopus-current-command)
                     (buffer-file-name (org-base-buffer (current-buffer)))))

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

       (defun ,setup-suffix (_children)
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
           (when (catch 'toomany
                   (dolist (file files)
                     (when (> i 2)
                       (throw 'toomany t))
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
                                      :description (file-name-nondirectory file)
                                      :command symbol))
                             result)
                       (cl-incf i)
                       nil)))
             (let ((symbol (intern (format "octopus--context-file-suffix-%s-rest"
                                           ,initial-key)))
                   (key (concat ,initial-key "/")))
               (fset symbol
                     `(lambda ()
                        (interactive)
                        (octopus--run-file-suffix
                         (completing-read "Select a file: "
                                          (org-dog-file-completion :files ',files)))))
               (put symbol 'interactive-only t)
               (push `(,transient--default-child-level
                       transient-suffix
                       ,(list :key key
                              :description "more"
                              :command symbol))
                     result)))
           (nreverse result)))

       (transient-define-suffix ,files-suffix ()
         :description ',description-sym
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
             (octopus--dispatch (octopus-current-command)
                                files)
           (user-error "No file in the context"))))))

(octopus-define-context "project"
  :context-key project
  :initial-key "p"
  :description-label "Project"
  :description-body (thread-last
                      (cdar octopus--project-context)
                      (project-root)
                      (octopus--directory-name))
  :files-suffix octopus-project-files-suffix
  :setup-suffix octopus-setup-project-file-targets)

(octopus-define-context "major-mode"
  :context-key major-mode
  :initial-key "m"
  :description-label "Major Mode"
  :description-body (thread-last
                      (cdar octopus--major-mode-context)
                      (symbol-name)
                      (string-remove-suffix "-mode"))
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

(octopus-define-context "machine"
  :context-key machine
  :initial-key "M"
  :description-label "Machine"
  :description-body (cdar octopus--machine-context)
  :files-suffix octopus-machine-files-suffix
  :setup-suffix octopus-setup-machine-file-targets)

(defcustom octopus-context-file-subgroups
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
     :setup-children octopus-setup-language-file-targets)
    (:description
     octopus--machine-description
     :if octopus--machine-p
     :setup-children octopus-setup-machine-file-targets))
  "List of context file subgroups displayed in transient."
  :type '(repeat plist))

(defun octopus-setup-context-file-subgroups (_children)
  (mapcar (lambda (spec)
            (vector 1 'transient-column spec))
          octopus-context-file-subgroups))

(defcustom octopus-context-files-targets
  '(("p" octopus-project-files-suffix)
    ("m" octopus-major-mode-files-suffix)
    ("f" octopus-path-files-suffix)
    ("l" octopus-language-files-suffix)
    ("M" octopus-machine-files-suffix))
  "List of context file targets."
  :type '(repeat (list (string :tag "Key")
                       (symbol :tag "Transient suffix"))))

(defun octopus-setup-context-files-targets (_children)
  (mapcar (pcase-lambda (`(,key ,suffix))
            `(,transient--default-child-level
              transient-suffix
              ,(list :key key
                     :command suffix)))
          octopus-context-files-targets))

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
  (octopus--dispatch (octopus-current-command)
                     (org-dog-complete-file)))

;;;;; Current file

(transient-define-suffix octopus-this-file-suffix ()
  :description #'octopus--this-file-description
  :if (lambda () (and (derived-mode-p 'org-mode)
                      (octopus--base-buffer-file)))
  (interactive)
  (octopus--dispatch (octopus-current-command)
                     (octopus--base-buffer-file)))

(defun octopus--this-file-description ()
  (format "This file: %s" (file-name-nondirectory (octopus--base-buffer-file))))

(defun octopus--base-buffer-file ()
  "Return the file name for the base buffer."
  (buffer-file-name (org-base-buffer (current-buffer))))

;;;;; Clock

(defun octopus-clocked-entry-description ()
  (format "Clock: \"%s\"" (octopus--marker-heading org-clock-hd-marker)))

(transient-define-suffix octopus-clock-marker-suffix ()
  :description 'octopus-clocked-entry-description
  :if #'org-clocking-p
  (interactive)
  (octopus--dispatch (octopus-current-command)
                     org-clock-marker))

(defun octopus-clocked-file-description ()
  (format "Clocked file: \"%s\""
          (thread-last
            (marker-buffer org-clock-marker)
            (org-base-buffer)
            (buffer-file-name)
            (file-name-nondirectory))))

(transient-define-suffix octopus-clocked-file-suffix ()
  :description 'octopus-clocked-file-description
  :if #'org-clocking-p
  (interactive)
  (octopus--dispatch (octopus-current-command)
                     (buffer-file-name
                      (org-base-buffer (marker-buffer org-clock-marker)))))

;;;;; Capture locations

(transient-define-suffix octopus-last-capture-marker-suffix ()
  :description (lambda ()
                 (format "Last capture: \"%s\""
                         (octopus--marker-heading org-capture-last-stored-marker)))
  :if (lambda ()
        (and (bound-and-true-p org-capture-last-stored-marker)
             (buffer-live-p (marker-buffer org-capture-last-stored-marker))
             (org-match-line org-heading-regexp)))
  (interactive)
  (octopus--dispatch (octopus-current-command)
                     org-capture-last-stored-marker))

;;;;; Avy

(transient-define-suffix octopus-avy-org-heading-suffix ()
  :description "Avy Org heading"
  :if (lambda () (require 'avy nil t))
  (interactive)
  (when-let (marker (save-window-excursion
                      (save-excursion
                        (and (avy-jump (rx bol (+ "*") space))
                             (point-marker)))))
    (octopus--dispatch (octopus-current-command)
                       marker)))

;;;;; Link description

(defvar octopus-edit-link-description nil)

(transient-define-infix octopus-infix-edit-link-description ()
  :description "Edit the link description"
  :class 'octopus-boolean-variable
  ;; If there is an active region, it will be used as the description, so this
  ;; suffix will be disabled.
  :if (lambda () (not (use-region-p)))
  :variable 'octopus-edit-link-description)

;;;;; org-super-link

(defvar octopus-enable-super-link nil)

(transient-define-infix octopus-infix-super-link ()
  :description "Insert a backlink into the target"
  :class 'octopus-boolean-variable
  :variable 'octopus-enable-super-link)

(defcustom octopus-super-link-drawer-list nil
  "List of drawer names suggested in completion."
  :type '(choice string))

(transient-define-infix octopus-infix-super-link-drawer ()
  :description "org-super-links drawer"
  :class 'octopus-completion
  :variable 'org-super-links-related-into-drawer
  :prompt "Drawer name: "
  :table 'octopus-super-link-drawer-list)

;;;;; org-transclusion

(defvar octopus-enable-transclusion-link nil)

(transient-define-infix octopus-infix-transclusion-link ()
  :description "Insert transclusion"
  :class 'octopus-boolean-variable
  :variable 'octopus-enable-transclusion-link)

;;;;; octopus-find-node

(defcustom octopus-find-node-verbs
  '((?f "Display a node" nil org-ql-find))
  "List of choices of search functions used in `octopus-find-node'.

This must be a list that can be passed to `read-multiple-choice',
and its fourth argument must be a function that takes a list of
function as the argument."
  :type '(repeat (list (character :tag "Key")
                       (string :tag "Name of the choice")
                       (choice (string :tag "Description")
                               (const nil))
                       (function :tag "Function that takes a list of files"))))

(defvar octopus-find-node-verb nil)

(transient-define-infix octopus-infix-find-node-verb ()
  :class 'octopus-multiple-choice
  :description "Verb"
  :variable 'octopus-find-node-verb
  :prompt "Choose a search function"
  :choices-variable 'octopus-find-node-verbs)

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
   ;; Select the base buffer of an indirect bufer
   ("\\" octopus-this-file-suffix :if buffer-base-buffer)
   ("/" octopus-read-dog-file-suffix)]
  (interactive)
  (transient-setup 'octopus-find-file))

(cl-defmethod octopus--dispatch ((_cmd (eql 'octopus-find-file))
                                 target)
  (funcall (if octopus-other-window
               #'org-dog-find-file-other-window
             #'org-dog-find-file)
           (cl-etypecase target
             (org-dog-file (oref target absolute))
             (string target))))

;;;###autoload (autoload 'octopus-find-node "octopus" nil 'interactive)
(transient-define-prefix octopus-find-node ()
  ["Options"
   ("-" octopus-infix-find-node-verb)]
  ["Context"
   :class transient-row
   :setup-children octopus-setup-context-files-targets]
  ["Static targets"
   :class transient-row
   :setup-children octopus-setup-static-targets]
  ["Other targets"
   :class transient-row
   ("\\" octopus-in-file-suffix)
   ("/" octopus-read-dog-file-suffix)
   ("#" octopus-clocked-file-suffix)]
  (interactive)
  (transient-setup 'octopus-find-node))

(cl-defmethod octopus--dispatch ((_cmd (eql 'octopus-find-node))
                                 files)
  (dolist (file (cl-etypecase files
                  (list files)
                  (string (list files))))
    (with-current-buffer (or (org-find-base-buffer-visiting file)
                             (find-file-noselect file))
      (run-hooks 'org-dog-before-search-hook)))
  (funcall (or (nth 3 (or octopus-find-node-verb
                          (error "octopus-find-node-verb is nil")))
               (error "Missing the fourth argument: %s" octopus-find-node-verb))
           files))

(defcustom octopus-refiled-entry-functions
  ;; `org-agenda-get-any-marker' is undocumented now
  '(octopus-org-agenda-marker)
  "Alternative functions to return an Org marker in non `org-mode'.

Each function should have no side-effect and return either a
marker to an Org entry or nil."
  :type 'hook)

(defun octopus-org-agenda-marker ()
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-get-any-marker)))

(defun octopus--refiled-entry ()
  "Return a marker to an Org entry to be refiled."
  (if (derived-mode-p 'org-mode)
      (point-marker)
    (run-hook-with-args-until-success 'octopus-refiled-entry-functions)))

;;;###autoload (autoload 'octopus-refile "octopus" nil 'interactive)
(transient-define-prefix octopus-refile ()
  ["Options"
   ("-d" octopus-infix-refile-to-datetree)]
  ["Other targets"
   :class transient-row
   ("D" "Date tree in this file" org-dog-datetree-refile-to-this-file)
   ("'" "Avy" octopus-refile-to-avy-as-child
    :if (lambda () (fboundp 'avy-org-refile-as-child)))
   ("/" octopus-read-dog-file-suffix)
   ("@" octopus-clock-marker-suffix)
   ("\\" octopus-in-file-suffix)]
  (interactive)
  (unless (octopus--refiled-entry)
    (user-error "Support `octopus-refiled-entry-functions'"))
  ;; Load avy-org-refile-as-child
  (require 'avy nil t)
  (transient-setup 'octopus-refile))

(cl-defmethod octopus--dispatch ((_cmd (eql 'octopus-refile))
                                 target)
  (org-with-point-at (octopus--refiled-entry)
    (if (markerp target)
        (org-refile nil nil
                    (with-current-buffer (marker-buffer target)
                      (org-with-wide-buffer
                       (goto-char target)
                       (list (org-get-heading t t t t)
                             (buffer-file-name (org-base-buffer (marker-buffer target)))
                             nil
                             (marker-position target)))))
      (if octopus-refile-to-datetree
          (progn
            (require 'org-dog-datetree)
            (org-dog-datetree-refile target))
        (org-dog-refile-1 (cl-etypecase target
                            (org-dog-file (oref target absolute))
                            (string target)))))))

(defun octopus-refile-to-avy-as-child ()
  (interactive)
  (require 'avy)
  (org-with-point-at (octopus--refiled-entry)
    (avy-org-refile-as-child)))

;;;###autoload (autoload 'octopus-insert-link "octopus" nil 'interactive)
(transient-define-prefix octopus-insert-link ()
  "Insert a link to a heading in a file."
  ["Options"
   ("-s" octopus-infix-super-link)
   ("-S" octopus-infix-super-link-drawer)
   ("-t" octopus-infix-transclusion-link)
   ("-d" octopus-infix-edit-link-description)]
  ["Context"
   :class transient-row
   :setup-children octopus-setup-context-files-targets]
  ["Static targets"
   :class transient-row
   :setup-children octopus-setup-static-targets]
  ["Other targets"
   :class transient-row
   ("'" octopus-avy-org-heading-suffix)
   ("\\" octopus-in-file-suffix)
   ("/" octopus-read-dog-file-suffix)
   ("@" octopus-clock-marker-suffix)
   ("#" octopus-clocked-file-suffix)
   ("$" octopus-last-capture-marker-suffix)]
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "You must run this command inside org-mode"))
  (transient-setup 'octopus-insert-link))

(cl-defmethod octopus--dispatch ((_cmd (eql 'octopus-insert-link))
                                 target)
  (let* ((region (when (use-region-p)
                   (car (region-bounds))))
         (description (when region
                        (buffer-substring-no-properties
                         (car region) (cdr region))))
         (marker (if (markerp target)
                     target
                   (org-ql-completing-read target :prompt "Insert a link: "))))
    (atomic-change-group
      (when octopus-enable-transclusion-link
        (unless (bolp)
          (insert "\n"))
        (insert "#+transclude: "))
      (when region
        (delete-region (car region) (cdr region)))
      (if octopus-enable-super-link
          (progn
            ;; save-window-excursion is only necessary to targets that selects a
            ;; window, e.g. avy
            (with-current-buffer (marker-buffer marker)
              (org-with-wide-buffer
               (goto-char marker)
               (if (fboundp 'org-super-links-store-link)
                   (org-super-links-store-link)
                 (error "org-super-links-store-link is not bound"))))
            (org-super-links-insert-link)
            ;; Update the description of the inserted link.
            (when description
              (if (thing-at-point-looking-at org-link-bracket-re)
                  (let ((link (match-string-no-properties 1)))
                    (delete-region (match-beginning 0) (match-end 0))
                    (insert (org-link-make-string link description))))))
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (let ((inhibit-message))
             (call-interactively #'org-store-link))))
        (if-let (link (pop org-stored-links))
            (insert (org-link-make-string (car link)
                                          (or description
                                              (if octopus-edit-link-description
                                                  (read-from-minibuffer
                                                   "Link description: "
                                                   (cadr link))
                                                (cadr link)))))
          (error "No stored link"))))))

;; Maybe I'll drop `octopus-clock-in'. I think there should be a better
;; interface to the use case that is supposed to be covered by the command.

;;;###autoload (autoload 'octopus-clock-in "octopus" nil 'interactive)
(transient-define-prefix octopus-clock-in ()
  "Clock in to an existing heading or create a new heading."
  ["Context"
   :class transient-columns
   :setup-children octopus-setup-context-file-subgroups]
  ["Static targets"
   :class transient-row
   :setup-children octopus-setup-static-targets]
  ["Other targets"
   :class transient-row
   ;; Select the base buffer of an indirect bufer
   ("\\" octopus-this-file-suffix :if buffer-base-buffer)
   ("/" octopus-read-dog-file-suffix)]
  (interactive)
  (transient-setup 'octopus-clock-in))

(cl-defmethod octopus--dispatch ((_cmd (eql 'octopus-clock-in))
                                 file)
  (org-dog-clock-in file))

;;;; Other utilities

(defun octopus--marker-heading (marker)
  (save-current-buffer
    (org-with-point-at marker
      (if (org-match-line org-complex-heading-regexp)
          (org-link-display-format (match-string-no-properties 4))
        (error "Not on a heading")))))

(defun octopus--org-mode-p ()
  "Return non-nil if the current buffer is in `org-mode'."
  (derived-mode-p 'org-mode))

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

(defun octopus--directory-name (file)
  (thread-last
    file
    (string-remove-suffix (octopus--path-separator))
    (file-name-nondirectory)))

(provide 'octopus)
;;; octopus.el ends here

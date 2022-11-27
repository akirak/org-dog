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

(defcustom octopus-ql-find-options nil
  "Options passed from `octopus-find-node' to `org-ql-find'.

It is a plist that contains options of `org-ql-find', e.g.
:query-prefix, :query-filter, etc."
  :type 'plist)

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

(transient-define-suffix octopus-clocked-file-suffix ()
  :description "Clocked file"
  :if #'org-clocking-p
  (interactive)
  (octopus--dispatch (oref transient-current-prefix command)
                     (buffer-file-name
                      (org-base-buffer (marker-buffer org-clock-marker)))))

;;;;; Capture locations

(transient-define-suffix octopus-last-capture-marker-suffix ()
  :description (lambda ()
                 (concat "Last capture "
                         (save-current-buffer
                           (org-with-point-at org-capture-last-stored-marker
                             (format "\"%s\""
                                     (substring-no-properties
                                      (org-get-heading t t t t)))))))
  :if (lambda ()
        (and (bound-and-true-p org-capture-last-stored-marker)
             (buffer-live-p (marker-buffer org-capture-last-stored-marker))))
  (interactive)
  (octopus--dispatch (oref transient-current-prefix command)
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
    (octopus--dispatch (oref transient-current-prefix command)
                       marker)))

;;;;; org-super-link

(defvar octopus-enable-super-link nil)

(transient-define-infix octopus-infix-super-link ()
  :description "Insert a backlink into the target"
  :class 'octopus-boolean-variable
  :variable 'octopus-enable-super-link)

;;;;; org-transclusion

(defvar octopus-enable-transclusion-link nil)

(transient-define-infix octopus-infix-transclusion-link ()
  :description "Insert transclusion"
  :class 'octopus-boolean-variable
  :variable 'octopus-enable-transclusion-link)

;;;;; Org-ql

(defcustom octopus-view-function #'org-ql-search
  "Function used to view Org files in `octopus-find-node'.

It should be a function that takes a list of Org files as the
argument."
  :type 'function)

(defvar octopus-view-list nil)

(transient-define-infix octopus-infix-view-list ()
  :description "View list"
  :class 'octopus-boolean-variable
  :variable 'octopus-view-list)

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
               #'find-file-other-window
             #'find-file)
           (cl-etypecase target
             (org-dog-file (oref target absolute))
             (string target))))

;;;###autoload (autoload 'octopus-find-node "octopus" nil 'interactive)
(transient-define-prefix octopus-find-node ()
  ["Options"
   ("-v" octopus-infix-view-list)]
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
  (setq octopus-view-list nil)
  (transient-setup 'octopus-find-node))

(cl-defmethod octopus--dispatch ((_cmd (eql 'octopus-find-node))
                                 files)
  (require 'org-ql-find)
  (if octopus-view-list
      (funcall octopus-view-function files)
    (apply #'org-ql-find files octopus-ql-find-options)))

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
   ("-t" octopus-infix-transclusion-link)]
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
                                              (cadr link))))
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

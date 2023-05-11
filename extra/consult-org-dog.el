;;; consult-org-dog.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org "9.5") (org-dog "0.1") (consult "0.19"))
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

(require 'consult)
(require 'consult-org)
(require 'org-dog)

;; Silence the byte compiler
(declare-function org-ql-search "ext:org-ql-search")
(defvar org-capture-initial)

(defgroup consult-org-dog nil
  ""
  :group 'consult
  :group 'org-dog)

;;;; Custom variables

(defcustom consult-org-dog-sources
  '(consult-org-dog--file-source
    consult-org-dog--persistent-tag-source
    consult-org-dog--target-source)
  "List of consult sources used in `consult-org-dog'."
  :type '(repeat symbol))

(defcustom consult-org-dog-view-tag-fn #'consult-org-dog-view-tag-default
  "Function used to view tag.

The function is called with an Org tag."
  :type 'function)

(defcustom consult-org-dog-view-target-fn #'consult-org-dog-view-target-default
  "Function used to view a link target.

The function is called with two arguments: the target as a
string, and a list of files containing the target."
  :type 'function)

(defcustom consult-org-dog-fallback-fn
  #'consult-org-dog-fallback-default
  "Function called when the input matches no candidate.

The function is called with one argument, the string entered by
the user to the minibuffer."
  :type 'function)

(defcustom consult-org-dog-tag-super-groups nil
  "Alist of mappings from a tag to a list of `org-super-agenda' groups."
  :type '(alist :key-type (choice (string :tag "Org tag")
                                  (const t))
                :value-type '(sexp :tag "Org Super Agenda groups")))

;;;; Variables

(defvar consult-org-dog--file-source
  `(:name "Registered Org File"
          :narrow ?\f
          :category org-dog-file
          :face file
          :action ,#'org-dog-find-file
          :annotate ,#'org-dog--annotate-file
          :items ,#'org-dog--file-candidates))

(defvar consult-org-dog--persistent-tag-source
  `(:name "Persistent Org Tag"
          :narrow ?\g
          :category org-tag
          :face org-tag
          :action ,(lambda (tag)
                     (funcall consult-org-dog-view-tag-fn tag))
          :annotate ,#'ignore
          :items ,(lambda ()
                    (thread-last
                      (mapcar #'car org-tag-persistent-alist)
                      (cl-remove-if-not #'stringp)))))

(defvar consult-org-dog--target-alist nil)

(defvar consult-org-dog--target-source
  `(:name "Org Link Target"
          :narrow ?\t
          :category org-target
          :face default
          :action ,(lambda (target)
                     (funcall consult-org-dog-view-target-fn
                              target
                              (cdr (assoc target consult-org-dog--target-alist))))
          :annotate ,#'consult-org-dog--annotate-target
          :items
          ,(lambda ()
             (org-dog--build-link-target-cache)
             (setq consult-org-dog--target-alist (org-dog--link-target-alist))
             (mapcar #'car consult-org-dog--target-alist))))

(defvar consult-org-dog-history nil)

;;;; Overrides

(cl-defmethod org-dog-file-search ((file org-dog-file))
  (consult-org-heading nil (list (oref file absolute))))

;;;; Commands

;;;###autoload
(defun consult-org-dog-all-file-headings ()
  "Run `consult-org-heading' on all Org Dog files."
  (interactive)
  (consult-org-heading nil (map-keys org-dog--file-table)))

;;;###autoload
(defun consult-org-dog (&optional sources)
  ;; FIXME: Better explain the command
  "One command to rule them all.

SOURCES default to `consult-org-dog-sources'."
  (interactive)
  (let ((selected (consult--multi (or sources consult-org-dog-sources)
                                  :prompt "Display Org: "
                                  :history 'consult-org-dog-history
                                  :sort nil)))
    (unless (plist-get (cdr selected) :match)
      (funcall consult-org-dog-fallback-fn (car selected)))))

(defun consult-org-dog--annotate-target (target)
  (when-let (cell (assoc target consult-org-dog--target-alist))
    (concat " " (string-join (cdr cell) ", "))))

(defun consult-org-dog-fallback-default (string)
  "Default implementation of `consult-org-dog-fallback-fn'."
  (let ((string (string-trim string)))
    (if (string-match org-agenda-file-regexp string)
        (org-dog-find-file string)
      (let ((org-capture-initial string))
        (org-capture)))))

(defun consult-org-dog-view-tag-default (tag)
  (require 'org-ql-search)
  (org-ql-search (org-dog-select 'absolute
                   `(regexp ,(consult-org-dog--tag-regexp tag)))
    `(tags ,tag)
    :super-groups
    (cdr (or (assoc tag consult-org-dog-tag-super-groups)
             (assq t consult-org-dog-tag-super-groups)))
    :buffer (get-buffer-create (format "*Org Tag<%s>*" tag))))

(defun consult-org-dog--tag-regexp (tag)
  (rx-to-string `(and bol (+ "*") blank (+ nonl) blank
                      ,(org-make-tag-string (list tag))
                      (* blank) eol)))

(defun consult-org-dog-view-target-default (target files)
  (org-dog-link-target-occur
   target
   (if (= 1 (length files))
       (car files)
     (completing-read "File: " files nil t))))

(defun consult-org-dog--make-target-regexp (target)
  (rx-to-string `(or ,(format "<<%s>>" target)
                     (and "[[" ,target "]"
                          "[" (* nonl) "]]"))))

(provide 'consult-org-dog)
;;; consult-org-dog.el ends here

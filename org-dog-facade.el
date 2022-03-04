;;; org-dog-facade.el --- Facade Org files -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.5") (org-dog "0.1") (doct "3.1"))
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

(require 'org-dog-datetree)

(defcustom org-dog-facade-default-sections
  '(("Backlog" "b")
    ("Keywords" "k")
    ("Projects" "p")
    ("Activities" "a")
    ("Resources" "r"))
  "")

(defcustom org-dog-facade-datetree-key
  "d"
  "")

(defclass org-dog-facade-datetree-file (org-dog-datetree-file)
  ((sections :initarg :sections
             :initform 'org-dog-facade-default-sections)
   (datetree-capture-templates :initarg :datetree-capture-templates
                               :initform nil)))

(cl-defmethod org-dog-file-refile ((file org-dog-facade-datetree-file)))

(cl-defmethod org-dog-file-capture ((file org-dog-facade-datetree-file)))

(cl-defmethod org-dog-file-search ((file org-dog-facade-datetree-file)))

(defun org-dog-facade-goto-section (file)
  (interactive (list (or (and (not current-prefix-arg)
                              (org-dog-current-buffer-object))
                         (org-dog-complete-file))))
  (let ((file (cl-etypecase file
                (string (org-dog--file-object file))
                (org-dog-facade-datetree-file file))))
    (pcase (org-dog-facade-read-section file)
      (`(,_name ,_key ,marker)
       (switch-to-buffer (org-dog-file-buffer file))
       (org-goto-marker-or-bmk marker)))))

(defun org-dog-facade-read-section (file)
  (let ((available-sections (org-dog-facade--sections file)))
    (pcase (read-char-choice (concat (mapconcat (pcase-lambda (`(,name ,key . ,_))
                                                  (format "[%s] %s" key name))
                                                available-sections
                                                " ")
                                     ": ")
                             (mapcar (lambda (key)
                                       (car (string-to-list key)))
                                     available-sections))
      (`nil nil)
      (char (assoc (char-to-string char) available-sections)))))

(defun org-dog-facade--sections (file)
  (with-current-buffer (org-dog-file-buffer file)
    (org-with-wide-buffer
     (let ((sections (oref file sections))
           result)
       (when (symbolp sections)
         (setq sections (symbol-value sections)))
       (goto-char (point-min))
       (pcase-dolist (`(,olp ,key . ,_) sections)
         (when-let (marker (org-find-olp (if (stringp olp) (list olp) olp) t))
           (push (list key olp marker) result)))
       (nreverse result)))))

(provide 'org-dog-facade)
;;; org-dog-facade.el ends here

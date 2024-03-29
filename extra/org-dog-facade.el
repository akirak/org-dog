;;; org-dog-facade.el --- Facade Org files -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org "9.5") (org-dog "0.1") (doct "3.1") (org-reverse-datetree "0.4.0"))
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

;; FIXME

;;; Code:

(require 'org-dog-datetree)
(declare-function org-refile "ext:org-refile")
(declare-function org-agenda-refile "ext:org-agenda")
(declare-function org-reverse-datetree-num-levels "ext:org-reverse-datetree")
(declare-function org-with-base-buffer "ext:org-macs")
(declare-function org-clocking-p "ext:org-clock")

(defgroup org-dog-facade nil
  "Support for facade Org files."
  :prefix "org-dog-facade-"
  :group 'org-dog
  :group 'org-dog-datetree)

(defcustom org-dog-facade-default-sections
  '((?b "Backlog")
    (?k "Keywords")
    (?i "Index")
    (?l "Libraries")
    (?p "Projects")
    (?t "Patterns")
    (?a "Activities")
    (?r "Resources"))
  "The default set of sections for facade files."
  :type '(repeat (list (character :tag "Key")
                       (choice :tag "Heading or olp"
                               string (repeat string)))))

(defcustom org-dog-facade-datetree-key
  ?d
  "Key used to select the datetree of a facade file."
  :type 'character)

(defclass org-dog-facade-datetree-file (org-dog-datetree-file)
  ((sections :initarg :sections
             :initform 'org-dog-facade-default-sections)))

(cl-defmethod org-dog-file-refile ((file org-dog-facade-datetree-file))
  (pcase (org-dog-facade-read-section file t)
    (`nil nil)
    ((and `(,key . ,_) (guard (equal key org-dog-facade-datetree-key)))
     (org-reverse-datetree-refile-to-file (oref file absolute)))
    (`(,_ ,olp ,marker)
     ;; TODO: Allow customization
     (let ((rfloc (list (cl-etypecase olp
                          (string olp)
                          (list (org-format-outline-path olp)))
                        (oref file absolute)
                        nil
                        marker)))
       (cond
        ((derived-mode-p 'org-agenda-mode)
         (require 'org-agenda)
         (org-agenda-refile nil rfloc))
        ((derived-mode-p 'org-mode)
         (org-refile nil nil rfloc)))))))

(cl-defmethod org-dog-file-capture-templates ((file org-dog-facade-datetree-file))
  ;; TODO: Append templates for the facade sections
  (mapcar (pcase-lambda (`(,key . ,rest))
            (cons (concat (char-to-string org-dog-facade-datetree-key) key)
                  rest))
          (cl-call-next-method file)))

;; (cl-defmethod org-dog-file-search ((file org-dog-facade-datetree-file)))

(defun org-dog-facade-goto-section (file)
  (interactive (list (or (and (not current-prefix-arg)
                              (org-dog-buffer-object))
                         (org-dog-complete-file))))
  (let ((file (cl-etypecase file
                (string (org-dog-file-object file))
                (org-dog-facade-datetree-file file))))
    (pcase (org-dog-facade-read-section file)
      (`(,_name ,_key ,marker)
       (switch-to-buffer (org-dog-file-buffer file))
       (org-goto-marker-or-bmk marker)))))

(defun org-dog-facade-read-section (file &optional include-datetree)
  (let ((available-sections (org-dog-facade--sections file)))
    (when include-datetree
      (setq available-sections (cons (list org-dog-facade-datetree-key "Datetree")
                                     available-sections)))
    (pcase (read-char-choice (concat (mapconcat (pcase-lambda (`(,key ,name . ,_))
                                                  (format "[%s] %s"
                                                          (char-to-string key)
                                                          name))
                                                available-sections
                                                " ")
                                     ": ")
                             (mapcar #'car available-sections))
      (`nil nil)
      (char (assq char available-sections)))))

(defun org-dog-facade--sections (file)
  (with-current-buffer (org-dog-file-buffer file)
    (org-with-wide-buffer
     (let ((sections (org-dog-symbol-value (oref file sections)))
           result)
       (goto-char (point-min))
       (pcase-dolist (`(,key ,olp . ,_) sections)
         ;; An error is shown when no olp is found, so suppress the error
         (let ((inhibit-message t))
           (when-let (marker (ignore-errors
                               (org-find-olp (if (stringp olp) (list olp) olp) t)))
             (push (list key olp marker) result))))
       (nreverse result)))))

(cl-defmethod org-dog-meaningful-in-file-p ((file org-dog-facade-datetree-file))
  (let ((level (org-outline-level))
        (heading (org-get-heading t t t t)))
    (and (not (and (= level 1)
                   (member heading
                           (mapcar (lambda (x) (nth 1 x))
                                   (org-dog-symbol-value (oref file sections))))))
         (not (and (<= level (org-reverse-datetree-num-levels))
                   (string-match-p "\\`[[:digit:]]\\{4\\}" heading))))))

(defun org-dog-facade-move (file)
  "Move the subtree to another file according to the outline path."
  (interactive (list (completing-read "File: "
                                      (org-dog-file-completion
                                       :class 'org-dog-facade-datetree-file)
                                      nil t)))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (when (org-before-first-heading-p)
    (user-error "Before first heading"))
  (unless (object-of-class-p (org-dog-buffer-object)
                             'org-dog-facade-datetree-file)
    (user-error "Wrong source class"))
  (let* ((pos (point))
         (buffer (org-base-buffer (current-buffer)))
         (start (org-entry-beginning-position))
         (end (save-excursion
                (org-end-of-subtree)))
         (clock-pos (and (org-clocking-p)
                         (eq buffer (marker-buffer org-clock-marker))
                         (>= (marker-position org-clock-marker)
                             start)
                         (< (marker-position org-clock-marker)
                            end)
                         (- (marker-position org-clock-marker)
                            start)))
         (heading (org-get-heading t t t t))
         (olp (org-get-outline-path))
         (diff (- pos start))
         (marker (org-with-base-buffer (or (find-buffer-visiting file)
                                           (find-file-noselect file))
                   (or (org-find-olp olp t)
                       ;; TODO: Create the parent
                       (error "The parent does not exist"))))
         (rfloc (list (cl-etypecase olp
                        (string olp)
                        (list (org-format-outline-path olp)))
                      file
                      nil
                      marker)))
    (when clock-pos
      (org-clock-out))
    (org-refile nil nil rfloc)
    (message "Moved the subtree to %s" file)
    (org-goto-marker-or-bmk marker)
    (re-search-forward (format org-complex-heading-regexp-format heading))
    (org-back-to-heading)
    (when clock-pos
      (save-excursion
        (forward-char clock-pos)
        (org-clock-in)))
    (recenter-top-bottom)
    (forward-char diff)))

(provide 'org-dog-facade)
;;; org-dog-facade.el ends here

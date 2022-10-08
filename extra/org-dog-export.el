;;; org-dog-export.el --- Export Org contents to various formats -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org "9.5") (org-dog "0.1"))
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

(require 'org-dog)

(declare-function emacsql "ext:emacsql")
(declare-function emacsql-sqlite "ext:emacsql-sqlite")
(declare-function org-reverse-datetree-map-entries "ext:org-reverse-datetree")
(defvar org-dog-datetree-file)

(defconst org-dog-export-date-regexp
  "[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}[[:digit:]]\\{2\\}")

;;;###autoload
(cl-defun org-dog-export-datetrees-to-sqlite (outfile
                                              files
                                              &key (date-regexp org-dog-export-date-regexp))
  "Export headings in date trees to a SQLite database."
  (interactive (list (read-file-name "Database file to export to: ")
                     (progn
                       (require 'org-dog-datetree)
                       (org-dog-complete-multiple-files nil nil nil
                                                        :class 'org-dog-datetree-file))
                     :date-regexp
                     (let* ((options (org-dog-export--date-regexp-alist))
                            (input (completing-read "Select a date range or enter a regexp: "
                                                    options)))
                       (or (cdr (assoc input options))
                           input))))
  (require 'emacsql-sqlite)
  (require 'org-reverse-datetree)
  (let ((i 0)
        (j 0))
    (emacsql-with-connection (conn (emacsql-sqlite outfile))
      (emacsql conn [:create-table headings
                                   ([(id integer :primary-key)
                                     title
                                     file
                                     basename
                                     date
                                     uuid
                                     link])])
      (emacsql conn [:create-table heading_tags
                                   ([(id integer :primary-key)
                                     (heading_id integer)
                                     tag])])
      (emacsql-with-transaction conn
        (dolist (file files)
          (with-current-buffer (or (find-buffer-visiting file)
                                   (find-file-noselect file))
            (org-with-wide-buffer
             (goto-char (point-min))
             (org-reverse-datetree-map-entries
              (lambda (date)
                (cl-incf i)
                (let ((element (org-element-headline-parser (org-entry-end-position))))
                  (emacsql conn
                           (vconcat [:insert-into headings :values]
                                    (vector
                                     (list
                                      (vector
                                       i
                                       ;; title
                                       (org-link-display-format
                                        (org-element-property :raw-value element))
                                       ;; file
                                       (expand-file-name file)
                                       ;; basename
                                       (file-name-base file)
                                       ;; date
                                       date
                                       ;; uuid
                                       (org-element-property :ID element)
                                       ;; link
                                       (when-let (title (car (org-element-property :title element)))
                                         (when (eq 'link (org-element-type title))
                                           (org-element-property :raw-link title))))))))
                  (dolist (tag (org-element-property :tags element))
                    (cl-incf j)
                    (emacsql conn
                             (vconcat [:insert-into heading_tags :values]
                                      (vector
                                       (list
                                        (vector
                                         j
                                         i
                                         (substring-no-properties tag)))))))))
              :date-regexp date-regexp)))))))
  (when (called-interactively-p 'any)
    (kill-new outfile)
    (message "Pushed the database file to the kill ring")))

(defun org-dog-export--date-regexp-alist ()
  (pcase (decode-time)
    (`(,_ ,_ ,_ ,day ,month ,year . ,_)
     (list (cons "Today" (format "%4d-%02d-%02d" year month day))
           (cons "This month" (format "%4d-%02d-%s" year month (rx (repeat 2 digit))))
           (cons "Last month" (format "%4d-%02d-%s"
                                      (if (> month 1) year (1- year))
                                      (if (> month 1) (1- month) 12)
                                      (rx (repeat 2 digit))))
           (cons "This year" (format "%4d-%s-%s" year
                                     (rx (repeat 2 digit))
                                     (rx (repeat 2 digit))))
           (cons "All" org-dog-export-date-regexp)))))

(provide 'org-dog-export)
;;; org-dog-export.el ends here

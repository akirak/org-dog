;;; org-dog-file-graph.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.5") (org-dog "0.1"))
;; Keywords: org outlines convenience
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
(require 'eieio)
(require 'subr-x)

(defgroup org-dog-file-graph nil
  ""
  :prefix "org-dog-file-graph-"
  :group 'org-dog)

(defcustom org-dog-file-graph-program "dot"
  "GraphViz program used to render the graph."
  :type 'file)

(defvar org-dog-file-graph-revs nil)

(defun org-dog-file-graph-scan (files)
  "Scan file links in FILES."
  (let* ((queue (mapcar #'abbreviate-file-name files))
         file)
    (dolist (x queue)
      (unless (assoc x org-dog-file-graph-revs)
        (push (list x) org-dog-file-graph-revs)))
    (while (setq file (pop queue))
      (dolist (dest (with-current-buffer (or (find-buffer-visiting file)
                                             (find-file-noselect file))
                      (org-with-wide-buffer
                       (goto-char (point-min))
                       (let (result
                             (bound (save-excursion
                                      (re-search-forward (rx bol (+ "*") space) nil t))))
                         (save-match-data
                           (while (re-search-forward org-link-any-re bound t)
                             (let* ((href (match-string 2))
                                    (obj (save-match-data
                                           (when (string-match (rx bol "org-dog:"
                                                                   (group (+ anything)))
                                                               href)
                                             (org-dog-find-file-object
                                              `(lambda (obj)
                                                 (equal (oref obj relative)
                                                        ,(match-string 1 href))))))))
                               (when obj
                                 (push (oref obj absolute)
                                       result)))))
                         result))))
        (if-let (cell (assoc dest org-dog-file-graph-revs))
            (unless (member file (cdr cell))
              (setcdr cell (cons file (cdr (copy-sequence cell)))))
          (push dest queue)
          (push (list dest file) org-dog-file-graph-revs))))
    org-dog-file-graph-revs))

;;;###autoload
(defun org-dog-file-graph-viz (files)
  "Visualize links between FILES using graphviz."
  (interactive (list org-agenda-files))
  (when files
    (setq org-dog-file-graph-revs nil)
    (org-dog-file-graph-scan files))
  (with-temp-buffer
    (insert "digraph {")
    (dolist (node (mapcar #'car org-dog-file-graph-revs))
      (insert (format "\"%s\" [label=\"%s\"]\n"
                      node (file-name-nondirectory node))))
    (pcase-dolist (`(,dest . ,sources) org-dog-file-graph-revs)
      (dolist (source sources)
        (insert (format "\"%s\" -> \"%s\"\n" source dest))))
    (insert "}")
    (let ((err-file (make-temp-file "org-dog-file-graph"))
          (out-buf (get-buffer-create "*Org-Dog-File-Graph*")))
      (unwind-protect
          (progn
            (with-current-buffer out-buf
              (read-only-mode -1)
              (erase-buffer))
            (unless (zerop (call-process-region (point-min) (point-max)
                                                org-dog-file-graph-program
                                                nil (list out-buf err-file) nil
                                                "-Tsvg"))
              (error "Program %s failed with non-zero exit code: %s"
                     org-dog-file-graph-program
                     (with-temp-buffer
                       (insert-file-contents err-file)))))
        (delete-file err-file))
      (with-current-buffer out-buf
        (let ((image (create-image
                      (buffer-string)
                      'svg
                      t)))
          (erase-buffer)
          (insert-image image))
        (setq-local org-dog-file-graph-files files)
        (local-set-key (kbd "g") #'org-dog-file-graph-revert-image)
        (read-only-mode t)
        (pop-to-buffer (current-buffer))))))

(defun org-dog-file-graph-revert-image ()
  "Revert the image."
  (interactive)
  (org-dog-file-graph-viz (org-agenda-files)))

(provide 'org-dog-file-graph)
;;; org-dog-file-graph.el ends here

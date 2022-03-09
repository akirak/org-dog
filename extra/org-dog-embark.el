;;; org-dog-embark.el --- An example embark configuration for org-dog -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.5") (org-dog "0.1") (embark "0.15"))
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

(require 'embark)
(require 'org-dog)

(defvar aw-dispatch-always)
(declare-function aw-select "ext:avy")
(declare-function aw-switch-to-window "ext:avy")

;; Based on https://karthinks.com/software/fifteen-ways-to-use-embark/
(eval-when-compile
  (defmacro org-dog-embark-aw-command (fn &optional name)
    (declare (indent 1))
    `(defun ,(intern (format "org-dog-%s-ace-window" (or name fn))) ()
       (interactive)
       (require 'ace-window)
       (with-demoted-errors "%s"
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(embark-define-keymap org-dog-embark-file-map
  "An embark keymap on Org files."
  :parent nil
  ("o" find-file-other-window)
  ("a" org-dog-find-file-ace-window)
  ("s" org-dog-search-in-file)
  ("/" org-dog-search-file-ace-window)
  ("r" org-dog-refile-to-file)
  ("c" org-dog-capture-to-file))

(add-to-list 'embark-keymap-alist '(org-dog-file . org-dog-embark-file-map))

;;;; Commands

(org-dog-embark-aw-command find-file)
(org-dog-embark-aw-command org-dog-search-in-file "search-file")

(provide 'org-dog-embark)
;;; org-dog-embark.el ends here

;;; org-dog-tablist.el --- Tabulated-list interface -*- lexical-binding: t -*-

(require 'org-dog)
(require 'tabulated-list)

(defvar org-dog-tablist-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'tabulated-list-revert)
    map))

(defcustom org-dog-tablist-columns
  '((relative
     ("Relative" 36 t))
    (org-dog-file-title
     ("Title" 25 t))
    (root
     ("Repo" 12 t))
    (org-dog-tablist--format-class
     ("Class" 16 t))
    (org-dog-tablist--format-agenda-p
     ("Agenda" 3 t))
    (org-dog-tablist--format-file-tags
     ("File tags" 14 t))
    (org-dog-tablist--format-inactive-ts
     ("Activity" 10 t)))
  "List of columns in the tabulated list."
  :type '(repeat (list (choice (symbol :tag "Name of the slot in the object")
                               (function :tag "Function to return a value"))
                       (list :tag "Format" string number boolean))))

(defcustom org-dog-tablist-sort-key '("Relative" . nil)
  "Sort key."
  :type '(cons string boolean))

(defvar org-dog-tablist--agenda-files nil)

(defvar-local org-dog-tablist-query nil)

;;;###autoload
(defun org-dog-tablist-files ()
  "Browse org-dog files in a tabulated list interface."
  (interactive)
  (with-current-buffer (get-buffer-create "*Org Dog Tablist Files*")
    (let ((inhibit-read-only t))
      (erase-buffer))
    (org-dog-tablist-files-mode)
    (tabulated-list-revert)
    (pop-to-buffer (current-buffer))))

(define-derived-mode org-dog-tablist-files-mode tabulated-list-mode
  "DogTablistFiles"
  "Major mode for displaying a list of files."
  (setq tabulated-list-format (apply #'vector
                                     (mapcar #'cadr org-dog-tablist-columns)))
  (setq tabulated-list-padding 2)
  ;; I am not sure what would be the best here, so I may change this later.
  (setq tabulated-list-sort-key org-dog-tablist-sort-key)
  (add-hook 'tabulated-list-revert-hook #'org-dog-tablist-files-refresh nil t)
  (tabulated-list-init-header))

(defun org-dog-tablist-files-refresh ()
  (setq org-dog-tablist--agenda-files
        (thread-last
          (org-agenda-files)
          (mapcar #'abbreviate-file-name)))
  (setq tabulated-list-entries
        (thread-last
          (org-dog-select nil org-dog-tablist-query)
          (mapcar (lambda (obj)
                    (list (oref obj absolute)
                          (thread-last
                            org-dog-tablist-columns
                            (mapcar #'car)
                            (mapcar `(lambda (key)
                                       (or (if (functionp key)
                                               (funcall key ,obj)
                                             (slot-value ,obj key))
                                           "")))
                            (apply #'vector))))))))

;;;; Tweak the view

(defun org-dog-tablist-revert-with-query ()
  "Revert the current buffer with a new query."
  (interactive)
  (let ((query (minibuffer-with-setup-hook
                   (lambda ()
                     (lisp-data-mode))
                 (read-from-minibuffer "Query: "
                                       (when org-dog-tablist-query
                                         (prin1-to-string org-dog-tablist-query))
                                       nil
                                       #'read))))
    (setq org-dog-tablist-query query)
    (tabulated-list-revert)))

(defun org-dog-tablist-sort ()
  "Revert the current buffer with a new sort."
  (interactive)
  (setq-local tabulated-list-sort-key
              (cons (completing-read "Sort key: "
                                     (mapcar #'caadr org-dog-tablist-columns)
                                     nil t
                                     nil (car tabulated-list-sort-key))
                    (cdr tabulated-list-sort-key)))
  (tabulated-list-revert))

(defun org-dog-tablist-reverse ()
  "Reverse items in the current buffer."
  (interactive)
  (setq-local tabulated-list-sort-key
              (cons (car tabulated-list-sort-key)
                    (not (cdr tabulated-list-sort-key))))
  (tabulated-list-revert))

;;;; Functions for formatting a column

(defun org-dog-tablist--format-class (obj)
  (org-dog--format-class
   (eieio-object-class obj)))

(defun org-dog-tablist--format-agenda-p (obj)
  (if (member (oref obj absolute)
              org-dog-tablist--agenda-files)
      "Yes"
    "No"))

(defun org-dog-tablist--format-file-tags (obj)
  (string-join (org-dog-file-tags obj) ","))

(defun org-dog-tablist--format-inactive-ts (obj)
  (with-current-buffer (org-dog-file-buffer obj)
    (org-with-wide-buffer
     (goto-char (point-min))
     (if-let (ts (org-dog--latest-inactive-ts))
         (ts-format "%F" ts)
       ""))))

(provide 'org-dog-tablist)
;;; org-dog-tablist.el ends here

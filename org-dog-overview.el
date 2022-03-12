;;; org-dog-overview.el ---  -*- lexical-binding: t -*-

(require 'org-dog)
(require 'eieio)
(require 'subr-x)

(defgroup org-dog-overview nil
  ""
  :prefix "org-dog-overview-"
  :group 'org-dog)

(defconst org-dog-overview-viz-buffer "*Org-Dog-Overview-Graph*"
  "Name of the buffer that visualizes file links.")

(defconst org-dog-overview-sidebar-buffer "*Org-Dog-Overview-Sidebar*"
  "Name of the buffer that visualizes file links.")

(defcustom org-dog-overview-program "dot"
  "GraphViz program used to render the graph."
  :type 'file)

(defvar org-dog-overview-backlinks nil
  "Alist that stores information from file links.

Each items is a cons cell of (FILE . ALIST) where the car is the
absolute path of a file and the cdr is an alist of links linking
to it.

The cdr alist if a cons cell of (FILE . MARKER) where the file is
the absolute path of a file containing the link and the marker
points to the start of the link.

This entire variable represents the reverse dependencies of Org
files. An entry where its cdr is nil has no file linking to it.")

(defvar org-dog-overview-saved-wconf nil)

(defun org-dog-overview-scan (files &optional clear)
  "Scan file links in FILES."
  (when clear
    (setq org-dog-overview-backlinks nil))
  (let* ((queue (mapcar #'abbreviate-file-name files))
         file)
    (dolist (x queue)
      (unless (assoc x org-dog-overview-backlinks)
        (push (list (substring x)) org-dog-overview-backlinks)))
    (while (setq file (pop queue))
      (pcase-dolist
          (`(,dest . ,marker)
           (with-current-buffer (or (find-buffer-visiting file)
                                    (find-file-noselect file))
             (org-with-wide-buffer
              (goto-char (point-min))
              (let (result
                    (bound (save-excursion
                             (re-search-forward (rx bol (+ "*") space) nil t))))
                (save-match-data
                  (while (re-search-forward org-link-any-re bound t)
                    (let* ((pos (car (match-data)))
                           (href (match-string 2))
                           (obj (save-match-data
                                  (when (string-match (rx bol "org-dog:"
                                                          (group (+ anything)))
                                                      href)
                                    (org-dog-find-file-object
                                     `(lambda (obj)
                                        (equal (oref obj relative)
                                               ,(match-string 1 href))))))))
                      (when obj
                        (push (cons (substring (oref obj absolute))
                                    (copy-marker pos))
                              result)))))
                result))))
        (if-let (cell (assoc dest org-dog-overview-backlinks))
            (unless (member file (cdr cell))
              (setcdr cell (cons (cons (substring file) marker)
                                 (cdr (copy-sequence cell)))))
          (push dest queue)
          (push (list dest
                      (cons file marker))
                org-dog-overview-backlinks))))
    org-dog-overview-backlinks))

;;;###autoload
(defun org-dog-overview (files)
  "Visualize links between FILES using graphviz."
  (interactive (list (if current-prefix-arg
                         (completing-read-multiple "Files: " org-agenda-files)
                       org-agenda-files)))
  (when files
    (org-dog-overview-scan files t))
  (unless org-dog-overview-saved-wconf
    (setq org-dog-overview-saved-wconf
          (current-window-configuration)))
  (let ((viz-buffer (org-dog-overview-viz-buffer))
        (sidebar-buffer (org-dog-overview-sidebar-buffer)))
    (delete-other-windows)
    (switch-to-buffer sidebar-buffer)
    (split-window-right (- (/ (frame-inner-width) (frame-char-width)) 80))
    (switch-to-buffer viz-buffer)
    (select-window (window-in-direction 'right))))

(defun org-dog-overview-viz-buffer ()
  (with-temp-buffer
    (insert "digraph {")
    (dolist (node (mapcar #'car org-dog-overview-backlinks))
      (insert (format "\"%s\" [label=\"%s\"]\n"
                      node (file-name-nondirectory node))))
    (pcase-dolist (`(,dest . ,links) org-dog-overview-backlinks)
      (dolist (link links)
        (insert (format "\"%s\" -> \"%s\"\n"
                        (car link)
                        dest))))
    (insert "}")
    (let ((err-file (make-temp-file "org-dog-overview-errors"))
          (out-buf (get-buffer-create org-dog-overview-viz-buffer)))
      (unwind-protect
          (progn
            (with-current-buffer out-buf
              (read-only-mode -1)
              (erase-buffer))
            (unless (zerop (call-process-region (point-min) (point-max)
                                                org-dog-overview-program
                                                nil (list out-buf err-file) nil
                                                "-Tsvg"))
              (error "Program %s failed with non-zero exit code: %s"
                     org-dog-overview-program
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
        ;; (read-only-mode t)
        (local-set-key (kbd "g") #'org-dog-overview-revert)
        (local-set-key (kbd "q") #'org-dog-overview-quit))
      out-buf)))

(defun org-dog-overview-sidebar-buffer ()
  (with-current-buffer (get-buffer-create org-dog-overview-sidebar-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pcase-dolist (`(,dest . ,links) (reverse org-dog-overview-backlinks))
        (insert "* "
                (org-link-make-string (concat "file:" dest)
                                      (file-name-nondirectory dest))
                "\n")
        (pcase-dolist (`(,src . ,marker) (reverse links))
          (insert (concat (org-with-point-at marker
                            (cond
                             ((org-in-item-p)
                              (buffer-substring (org-in-item-p)
                                                (line-end-position)))
                             (t
                              (thing-at-point 'paragraph))))
                          " — "
                          (org-link-make-string (concat "file:" src)
                                                (file-name-nondirectory src)))
                  "\n"))))
    (goto-char (point-min))
    (org-mode)
    (setq org-startup-folded 'showeverything)
    (org-set-startup-visibility)
    ;; (read-only-mode t)
    (local-set-key (kbd "g") #'org-dog-overview-revert)
    (local-set-key (kbd "q") #'org-dog-overview-quit)
    (current-buffer)))

(defun org-dog-overview-revert ()
  "Revert the image."
  (interactive)
  (org-dog-overview-scan org-agenda-files t)
  (org-dog-overview-viz-buffer)
  (org-dog-overview-sidebar-buffer))

(defun org-dog-overview-quit ()
  (interactive)
  (dolist (buf (list org-dog-overview-viz-buffer
                     org-dog-overview-sidebar-buffer))
    (bury-buffer buf))
  (when org-dog-overview-saved-wconf
    (set-window-configuration org-dog-overview-saved-wconf)
    (setq org-dog-overview-saved-wconf nil)))

(provide 'org-dog-overview)
;;; org-dog-overview.el ends here

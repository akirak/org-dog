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

(defcustom org-dog-overview-sort-fn
  #'org-dog-overview-sort-backlinks-1
  "Function used to sort contents in the sidebar buffer.

This function takes `org-dog-overview-backlinks' as an argument."
  :type 'function)

(defcustom org-dog-overview-sidebar-width 80
  "Width of the sidebar window."
  :type 'number)

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
    (switch-to-buffer viz-buffer)
    (display-buffer-in-side-window sidebar-buffer
                                   `((side . left)
                                     (dedicated . side)
                                     (preserve-size . (t . nil))
                                     (window-width . ,org-dog-overview-sidebar-width)))
    (select-window (window-in-direction 'left))))

(defun org-dog-overview-viz-buffer ()
  (with-temp-buffer
    (insert "digraph {")
    (dolist (node (mapcar #'car org-dog-overview-backlinks))
      (insert (format "\"%s\" [label=\"%s\"]\n"
                      node (file-name-base node))))
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
              (setq-local buffer-read-only nil)
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
        (setq-local buffer-read-only t)
        (org-dog-overview-mode t))
      out-buf)))

(defun org-dog-overview-sidebar-buffer ()
  (with-current-buffer (get-buffer-create org-dog-overview-sidebar-buffer)
    (let ((initial-loc (when (> (point) (point-min))
                         (list (progn
                                 (beginning-of-line)
                                 (when (re-search-forward (rx " — " (+ nonl) eol)
                                                          (save-excursion
                                                            (org-end-of-subtree))
                                                          t)
                                   (match-string 0)))
                               (progn
                                 (org-back-to-heading)
                                 (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))))))
          (inhibit-read-only t))
      (erase-buffer)
      (pcase-dolist (`(,dest . ,links)
                     (funcall org-dog-overview-sort-fn org-dog-overview-backlinks))
        (insert "* "
                (org-link-make-string
                 (concat "org-dog:" (oref (org-dog-file-object dest) relative))
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
                          (org-link-make-string
                           (concat "org-dog:" (oref (org-dog-file-object src) relative))
                           (file-name-nondirectory src)))
                  "\n")))
      (goto-char (point-min))
      (org-mode)
      (setq-local org-startup-folded 'showeverything)
      (org-set-startup-visibility)
      (when (and initial-loc
                 (search-forward (nth 1 initial-loc) nil t)
                 (car initial-loc)
                 (search-forward (car initial-loc) (save-excursion
                                                     (org-end-of-subtree))
                                 t)
                 (looking-back org-link-any-re (line-beginning-position)))
        (goto-char (car (match-data))))
      (setq-local buffer-read-only t)
      (org-dog-overview-mode t)
      (current-buffer))))

(defun org-dog-overview-sort-backlinks-1 (backlinks)
  (let ((group1 (seq-filter (lambda (x) (= 1 (length x))) backlinks))
        (group2 (seq-filter (lambda (x) (< 1 (length x))) backlinks)))
    (append (seq-sort-by #'car #'string< group1)
            (seq-sort-by #'car #'string< group2))))

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

(defvar org-dog-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'org-dog-overview-revert)
    (define-key map (kbd "q") #'org-dog-overview-quit)
    map))

(define-minor-mode org-dog-overview-mode
  "Minor mode which should be activated in overview buffers.")

(provide 'org-dog-overview)
;;; org-dog-overview.el ends here

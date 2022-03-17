;;; org-dog-overview.el ---  -*- lexical-binding: t -*-

(require 'org-dog)
(require 'eieio)
(require 'subr-x)

(defgroup org-dog-overview nil
  "Visualize links between Org files."
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

(defcustom org-dog-overview-dot-type "digraph"
  ""
  :type 'string)

(defcustom org-dog-overview-dot-stmts
  '("rankdir=LR")
  "Dot statements inserted into the graph."
  :type '(choice string
                 (repeat string)))

(defcustom org-dog-overview-node-format-fn
  #'org-dog-overview-node-format-1
  ""
  :type 'function)

(defcustom org-dog-overview-edge-format-fn
  #'org-dog-overview-edge-format-1
  ""
  :type 'function)

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

(defvar org-dog-overview-non-default-files nil
  "List of files that used as the starting point of scanning.

This is non-nil if and only if the initial file list is not
`org-agenda-files'.")

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
              (org-dog-overview--file-links
               (save-excursion
                 (re-search-forward (rx bol (+ "*") space) nil t))))))
        (if-let (cell (assoc dest org-dog-overview-backlinks))
            (unless (member file (cdr cell))
              (setcdr cell (cons (cons (substring file) marker)
                                 (cdr (copy-sequence cell)))))
          (push dest queue)
          (push (list dest
                      (cons file marker))
                org-dog-overview-backlinks))))
    org-dog-overview-backlinks))

(defun org-dog-overview--file-links (bound)
  "Return an alist of file-link markers till BOUND."
  (let (result)
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
          (if obj
              (push (cons (substring (oref obj absolute))
                          (copy-marker pos))
                    result)
            (message "Dead link: %s" href)))))
    result))

;;;###autoload
(defun org-dog-overview (files)
  "Visualize links between FILES using graphviz.

This command collects `org-dog' links from a certain set of
files.

Not the entire Org buffer is scanned. Only the region before the
first heading is scanned in each Org file.

By default, the target files are `org-agenda-files'. However,
when a prefix argument is given, the user is asked to select
files using `completing-read-multiple' interface.

The initial selection depends on the prefix argument.

If a single universal prefix argument is given and the current
point is on an `org-mode' entry, it collects links from the
subtree. This feature allows you to define a context in an Org
subtree which you can visualize its dependencies later.

If two universal prefixes are given, the last selection is used
as the initial input."
  (interactive (list (if current-prefix-arg
                         (setq org-dog-overview-non-default-files
                               (completing-read-multiple
                                "Files: "
                                (map-keys org-dog--file-table)
                                nil nil
                                (thread-first
                                  (pcase current-prefix-arg
                                    ('(4)
                                     (org-dog-overview--subtree-links))
                                    ('(16)
                                     ;; Reuse the last selection
                                     org-dog-overview-non-default-files))
                                  ;; Join with one of the crm separators
                                  (string-join ","))))
                       (setq org-dog-overview-non-default-files nil)
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

(defun org-dog-overview--subtree-links ()
  "Return a list of linked files in the subtree."
  (when (and (derived-mode-p 'org-mode)
             (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading)
      (mapcar #'car (org-dog-overview--file-links
                     (save-excursion
                       (org-end-of-subtree)))))))

(defun org-dog-overview-viz-buffer ()
  "Return `org-dog-overview-viz-buffer' after initializing it."
  (with-temp-buffer
    (org-dog-overview-dot-1 org-dog-overview-backlinks)
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

(defun org-dog-overview-dot-1 (graph)
  "Insert dot format for the GRAPH."
  (insert org-dog-overview-dot-type " {"
          (cl-etypecase org-dog-overview-dot-stmts
            (null "")
            (string org-dog-overview-dot-stmts)
            (list (string-join org-dog-overview-dot-stmts "\n")))
          "\n")
  (dolist (node (mapcar #'car graph))
    (insert (format "\"%s\"" node)
            (funcall org-dog-overview-node-format-fn node)
            "\n"))
  (pcase-dolist (`(,dest . ,links) graph)
    (dolist (link links)
      (insert (format "\"%s\" -> \"%s\"" (car link) dest)
              (funcall org-dog-overview-edge-format-fn (car link) dest)
              "\n")))
  (insert "}"))

(defun org-dog-overview-node-format-1 (filename)
  (format "[label=\"%s\"]" (file-name-base filename)))

(defun org-dog-overview-edge-format-1 (_origin _dest)
  "")

(defun org-dog-overview-sidebar-buffer ()
  "Return `org-dog-overview-sidebar-buffer' after initializing it."
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
  "Sort an alist of BACKLINKS according to a certain rule."
  (let ((group1 (seq-filter (lambda (x) (= 1 (length x))) backlinks))
        (group2 (seq-filter (lambda (x) (< 1 (length x))) backlinks)))
    (append (seq-sort-by #'car #'string< group1)
            (seq-sort-by #'car #'string< group2))))

(defun org-dog-overview-revert ()
  "Revert the image."
  (interactive)
  (org-dog-overview-scan (or org-dog-overview-non-default-files
                             org-agenda-files)
                         t)
  (org-dog-overview-viz-buffer)
  (org-dog-overview-sidebar-buffer))

(defun org-dog-overview-quit ()
  "Quit the view and restore the window configuration."
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

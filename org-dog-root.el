;;; org-dog-root.el --- Manage root contexts -*- lexical-binding: t -*-

(defgroup org-dog-root nil
  "Manage root contexts."
  :prefix "org-dog-root"
  :group 'org-dog)

(defcustom org-dog-root-files nil
  "List of Org files that define context roots.

Each file should be relative path from the root."
  :type '(repeat file))

(defcustom org-dog-root-default-desktop-dir
  (locate-user-emacs-file ".org-dog-desktop")
  "Path to the desktop file of the default context."
  :type 'directory)

(defcustom org-dog-root-enter-hook nil
  "Hook run when switching to a new root context.

Each function should take the file object as an argument."
  :type 'hook)

(defcustom org-dog-root-leave-hook nil
  "Hook run when leaving to a new root context.

Each function should take the file object as an argument."
  :type 'hook)

(defcustom org-dog-root-enter-default-hook nil
  "Hook run when switching to the default context.

Each function takes no argument."
  :type 'hook)

(defcustom org-dog-root-leave-default-hook nil
  "Hook run when leaving the default context.

Each function takes no argument."
  :type 'hook)

(defcustom org-dog-root-span 7
  "Number of days `org-dog-root-add-active-files' should check for."
  :type 'number)

(defvar org-dog-root-file nil)

;;;; Basics

;;;###autoload
(defun org-dog-root-switch (file)
  "Switch the context to a root FILE."
  (interactive (list (unless (equal current-prefix-arg '(16))
                       (completing-read (format "Switch context [current %s]: "
                                                org-dog-root-file)
                                        (thread-last
                                          org-dog-root-files
                                          (mapcar #'org-dog-resolve-relative-file)
                                          (delq nil)
                                          (remove org-dog-root-file))))))
  (when (equal org-dog-root-file file)
    (user-error "Same context"))
  (if file
      (org-dog-root--switch file)
    (org-dog-root--switch-to-default)))

(defun org-dog-root--switch (file)
  (let ((obj (org-dog-file-object file)))
    (if org-dog-root-file
        (run-hook-with-args 'org-dog-root-leave-hook
                            (org-dog-file-object org-dog-root-file))
      (run-hooks 'org-dog-root-leave-default-hook))
    (setq org-dog-root-file file
          org-agenda-files (thread-last
                             (org-dog-overview-scan (list file) :clear t :fast t)
                             (mapcar #'car)))
    (run-hook-with-args 'org-dog-root-enter-hook
                        obj)))

(defun org-dog-root--switch-to-default ()
  (when org-dog-root-file
    (run-hook-with-args 'org-dog-root-leave-hook
                        (org-dog-file-object org-dog-root-file)))
  (setq org-dog-root-file nil)
  (setq org-agenda-files nil)
  (org-dog-reload-files)
  (run-hooks 'org-dog-root-enter-default-hook))

;;;; Mode line construct

(defconst org-dog-root-mode-item
  '(org-dog-root-file
    (org-dog-root-format " ")))

(defface org-dog-root-mode-item-face '((t :inherit default))
  "Face for active root context name.")

(defcustom org-dog-root-format
  '("dog-root"
    "["
    (:eval (propertize (file-name-base org-dog-root-file)
                       'face 'org-dog-root-mode-item-face))
    "]")
  "Format of the lighter."
  :type 'sexp)

;;;; Desktop integration

(defun org-dog-root-setup-desktop ()
  "Set up hooks for desktop integration."
  (add-hook 'org-dog-root-enter-hook #'org-dog-root-enter-desktop)
  (add-hook 'org-dog-root-leave-hook #'org-dog-root-leave-desktop)
  (add-hook 'org-dog-root-enter-default-hook #'org-dog-root-enter-default-desktop)
  (add-hook 'org-dog-root-leave-default-hook #'org-dog-root-leave-default-desktop))

(defun org-dog-root-enter-desktop (obj)
  (let ((file (org-dog-root--desktop-file obj)))
    (when (file-directory-p file)
      (desktop-read file))))

(defun org-dog-root-enter-default-desktop ()
  (when (file-directory-p org-dog-root-default-desktop-dir)
    (desktop-read org-dog-root-default-desktop-dir)))

(defun org-dog-root-leave-desktop (obj)
  (let ((dir (org-dog-root--desktop-file obj)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (desktop-save dir 'release 'only-if-changed)
    (desktop-clear)))

(defun org-dog-root-leave-default-desktop ()
  (unless (file-directory-p org-dog-root-default-desktop-dir)
    (make-directory org-dog-root-default-desktop-dir t))
  (desktop-save org-dog-root-default-desktop-dir 'release 'only-if-changed))

(defun org-dog-root--desktop-file (obj)
  (let ((root (oref obj root))
        (relative (oref obj relative)))
    (expand-file-name (concat ".org-dog-desktops/" (file-name-base relative))
                      root)))

;;;; Scanning

(defun org-dog-root-add-active-files (&optional arg)
  "Add files that contain latest activities to `org-agenda-files'."
  (interactive "P")
  (let ((org-dog-root-span (if (numberp arg)
                               arg
                             org-dog-root-span)))
    (dolist (file (thread-last
                    (org-dog-select-files)
                    (mapcar (lambda (obj) (oref obj absolute)))))
      (unless (member file org-agenda-files)
        (when (org-dog-root--active-file-p file)
          (push file org-agenda-files))))))

(defun org-dog-root--active-file-p (file)
  "Return non-nil if FILE contains latest activities."
  (cl-flet
      ((contain-clock-p
         ()
         (catch 'found-clock
           (while (re-search-forward org-clock-line-re nil t)
             (let ((time (thread-last
                           (org-element-clock-parser (line-end-position))
                           (org-element-property :value)
                           (org-timestamp-to-time))))
               (when (< (- (float-time) (float-time time))
                        (* 3600 24 org-dog-root-span))
                 (throw 'found-clock t))))
           nil)))
    (if-let (buffer (find-buffer-visiting file))
        (with-current-buffer buffer
          (org-with-wide-buffer
           (goto-char (point-min))
           (contain-clock-p)))
      (with-temp-buffer
        (insert-file-contents file)
        (org-set-regexps-and-options)
        (goto-char (point-min))
        (contain-clock-p)))))

(provide 'org-dog-root)
;;; org-dog-root.el ends here

;;; org-dog-context.el --- Context support -*- lexical-binding: t -*-

(require 'org-dog)

(declare-function with-electric-help "ehelp")
(declare-function project-root "ext:project")

(defgroup org-dog-context nil
  "Foundation for contextual Org mode."
  :prefix "org-dog-context-"
  :group 'org-dog)

;;;; Generic methods

(cl-defgeneric org-dog-context-file-objects (_context))

;;;; Context types

(cl-defstruct org-dog-context
  "A skeleton type to provide `org-dog-context-p' predicate.")

(cl-defstruct (org-dog-context-in-directory (:include org-dog-context))
  directory filenames)

(cl-defmethod org-dog-context-file-objects ((context org-dog-context-in-directory))
  (let* ((prefix (org-dog-context-in-directory-directory context))
         (files (org-dog-select-files
                 `(lambda (obj)
                    (string-prefix-p ,prefix (oref obj relative))))))
    (thread-last
      (org-dog-context-in-directory-filenames context)
      (seq-some (apply-partially
                 (lambda (files basename)
                   (seq-some `(lambda (file)
                                (when (equal (file-name-base (oref file relative))
                                             ,basename)
                                  file))
                             files))
                 files))
      (list)
      (delq nil))))

;;;;

(defcustom org-dog-context-alist
  '((project
     :key ?p
     :value-fn project-current
     :test equal
     :callback org-dog-context-project-1)
    (major-mode
     :key ?m
     :callback org-dog-context-major-mode-1)
    (language
     :key ?l
     :value-fn org-dog-context-language-value
     :callback org-dog-context-language-1)
    (path
     :key ?f
     :value-fn org-dog-context-path-value
     :callback org-dog-context-path-1))
  ""
  :type '(alist :key-type symbol
                :value-type (plist
                             :options ((list (const :key)
                                             character)
                                       (list (const :value-fn)
                                             function)
                                       (list (const :test)
                                             function)
                                       (list (const :callback)
                                             function)))))

(defvar org-dog-context-cache nil
  "A hash table.")

(defun org-dog-context ()
  (thread-last
    org-dog-context-alist
    (mapcar (pcase-lambda (`(,type . ,_))
              (org-dog-context-edge type)))
    (delq nil)))

(defun org-dog-context-edge (type &optional arg)
  (let* ((plist (cdr (or (assq type org-dog-context-alist)
                         (error "No entry for %s in org-dog-context-alist" type))))
         (callback (or (plist-get plist :callback)
                       (message "Missing :callback for %s" type)))
         (arg (cond
               (arg arg)
               ((plist-get plist :value-fn)
                (funcall (plist-get plist :value-fn)))
               (t
                (or (buffer-local-value type (current-buffer))
                    (symbol-value type))))))
    (when arg
      (cons (cons type arg)
            (funcall callback arg)))))

(defun org-dog-context--make-table (type &optional test)
  (let ((tbl (make-hash-table :test (or test #'eq))))
    (push (cons type tbl) org-dog-context-cache)
    tbl))

(defun org-dog-context--flatten-to-alist (&optional context)
  (let (result)
    (cl-labels
        ((go (path triples)
           (pcase-dolist (`(,key ,value . ,children) triples)
             (let ((newpath (cons (cons key value) (reverse path))))
               (dolist (x children)
                 (if (object-of-class-p x 'org-dog-file)
                     (push (cons x (reverse newpath)) result)
                   (go newpath x)))))))
      (go nil (or context (org-dog-context))))
    (nreverse result)))

;;;###autoload
(defun org-dog-context-describe ()
  "Describe the context."
  (interactive)
  (let ((contexts (thread-last
                    (org-dog-context)
                    (mapcar (pcase-lambda (`(,key . ,ctx))
                              (cons key
                                    (when ctx
                                      (org-dog-context-file-objects ctx))))))))
    (with-electric-help
     `(lambda ()
        (read-only-mode t)
        (let ((inhibit-read-only t))
          (pcase-dolist (`(,ctx . ,files) ',contexts)
            (insert (propertize (format "%s: %s" (car ctx) (cdr ctx))
                                'face 'bold)
                    "\n")
            (if files
                (dolist (file files)
                  (insert (make-string 2 ?\s)
                          (oref file absolute)
                          "\n"))
              (insert (make-string 2 ?\s) "No file\n"))
            (insert ?\n))))
     "*Org-Dog-Context*")))

;;;;; Example context functions

(defun org-dog-context-project-1 (project)
  (require 'project)
  (pcase (file-name-split (abbreviate-file-name (project-root project)))
    (`("~" "work" ,_ ,group ,name "")
     (make-org-dog-context-in-directory
      :directory "projects/"
      :filenames (list (concat name "-dev")
                       (concat group "-dev")
                       name
                       group)))))

(defun org-dog-context-major-mode-1 (mode)
  (catch 'mode-context
    (let ((mode mode)
          filenames)
      (while mode
        ;; The mode context is unapplicable
        (when (memq mode '(fundamental-mode special-mode))
          (throw 'mode-context nil))
        (push (string-remove-suffix "-mode" (symbol-name mode))
              filenames)
        (setq mode (get mode 'derived-mode-parent)))
      (make-org-dog-context-in-directory
       :directory "programming/"
       :filenames (nreverse filenames)))))

(defun org-dog-context-language-value ()
  (save-match-data
    (pcase current-language-environment
      ((rx bol "UTF-")
       "English")
      ("ASCII"
       "English")
      ((rx bol (group (+ (not (any "-")))))
       (match-string 1 current-language-environment)))))

(defun org-dog-context-language-1 (language)
  (make-org-dog-context-in-directory
   :directory "languages/"
   :filenames (list language)))

(defcustom org-dog-context-path-patterns nil
  ""
  :type '(alist :key-type regexp
                :value-type file))

(defun org-dog-context-path-value ()
  (when-let* ((file (buffer-file-name))
              (project (project-current)))
    (file-relative-name file (expand-file-name (project-root project)))))

(defun org-dog-context-path-1 (path)
  (catch 'org-dog-context-path
    (pcase-dolist (`(,pattern . ,file) org-dog-context-path-patterns)
      (when (string-match-p pattern path)
        (throw 'org-dog-context-path
               (make-org-dog-context-in-directory
                :directory (file-name-directory file)
                :filenames (list (file-name-nondirectory file))))))))

(provide 'org-dog-context)
;;; org-dog-context.el ends here

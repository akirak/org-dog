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
  (let* ((dir (org-dog-context-in-directory-directory context))
         (dir-pred (cl-etypecase dir
                     (string
                      `(relative :prefix ,dir))
                     (list
                      `(relative :regexp ,(rx-to-string `(and bol (or ,@dir))))))))
    (cl-flet
        ((make-pred (basename)
           (when-let (objs (org-dog-select nil `(and ,dir-pred (basename ,basename))))
             objs)))
      (seq-some (apply-partially #'make-pred)
                (org-dog-context-in-directory-filenames context)))))

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
     :callback org-dog-context-path-1)
    (machine
     :key ?M
     ;; TODO: Use `file-remote-p' with identification set to 'host
     :value-fn system-name
     :callback org-dog-context-machine-1))
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

(defun org-dog-context-set-alist (key &rest plist)
  "Override properties of an entry in `org-dog-context-alist'.

This is a configuration helper. It updates the properties of an
entry at KEY with PLIST, leaving the other existing properties
unchanged."
  (declare (indent 1))
  (when-let (cell (assq key org-dog-context-alist))
    (let ((new-plist (cdr cell)))
      (cl-loop for (prop value) on plist by #'cddr
               do (plist-put new-plist prop value))
      (setcdr cell new-plist))))

(defvar-local org-dog-context-override-alist nil
  "Fallback alist of contexts.

By setting this variable local to specific buffers, you can set
override the contexts.

The value of this variable should be an alist of (CONTEXT .
VALUE) where CONTEXT is one of the keys in
`org-dog-context-alist' and VALUE should be a value of the type
as returned by :value-fn function in the settings.")

(defvar org-dog-context-cache nil
  "A hash table.")

(defun org-dog-context ()
  (thread-last
    org-dog-context-alist
    (mapcar (pcase-lambda (`(,type . ,_))
              (org-dog-context-edge type)))
    (delq nil)))

;;;###autoload
(defun org-dog-context-edge (type &optional arg)
  (let* ((plist (cdr (or (assq type org-dog-context-alist)
                         (error "No entry for %s in org-dog-context-alist" type))))
         (callback (or (plist-get plist :callback)
                       (message "Missing :callback for %s" type)))
         (arg (cond
               (arg arg)
               ((when-let (cell (assq type org-dog-context-override-alist))
                  (cdr cell)))
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

(defcustom org-dog-context-major-mode-aliases
  '((js-mode . "javascript"))
  "Alist of mapping from major mode symbols to file names."
  :type '(alist :key-type symbol :value-type string))

(defun org-dog-context-major-mode-1 (mode)
  (require 'org-src)
  (catch 'mode-context
    (let ((mode mode)
          filenames)
      (while mode
        ;; The mode context is unapplicable
        (when (memq mode '(fundamental-mode special-mode))
          (throw 'mode-context nil))
        (push (or (cdr (assq mode org-dog-context-major-mode-aliases))
                  (car (rassq (intern (string-remove-suffix "-mode" (symbol-name mode)))
                              org-src-lang-modes))
                  (thread-last
                    (symbol-name mode)
                    (string-remove-suffix "-mode")
                    (string-remove-suffix "-ts")))
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
  (when-let* ((file (buffer-file-name (buffer-base-buffer)))
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

(defun org-dog-context-machine-1 (hostname)
  (make-org-dog-context-in-directory
   :directory "hosts/"
   :filenames (list hostname)))

;;;; Extra functions

(defun org-dog-context-org-project-roots ()
  "Return project roots associated with the current Org buffer."
  (if-let (obj (org-dog-buffer-object))
      (seq-filter (apply-partially #'org-dog-context--project-org-file
                                   (oref obj absolute))
                  (project-known-project-roots))
    (user-error "Not in org-dog buffer")))

(defun org-dog-context--project-org-file (org-file root)
  (seq-find `(lambda (obj)
               (equal (oref obj absolute) ,org-file))
            (let* ((enable-dir-local-variables nil)
                   (default-directory root))
              (when-let (ctx (cdr (org-dog-context-edge 'project)))
                (org-dog-context-file-objects ctx)))))

;;;###autoload
(defun org-dog-context-find-project-file (root)
  "Visit an Org file for the current project.

ROOT can be a file directory. If a universal prefix argument is
given, you will be asked for a directory."
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Project: ")
                       (if-let (pr (project-current))
                           (project-root pr)
                         (user-error "Not in a project")))))
  (let* ((enable-dir-local-variables nil)
         (default-directory root))
    (if-let* ((ctx (cdr (org-dog-context-edge 'project)))
              (objs (org-dog-context-file-objects ctx)))
        (find-file (oref (car objs) absolute))
      (user-error "No associated Org file"))))

(provide 'org-dog-context)
;;; org-dog-context.el ends here

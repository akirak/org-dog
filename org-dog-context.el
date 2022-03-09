;;; org-dog-context.el --- Context support -*- lexical-binding: t -*-

(require 'org-dog)
(defgroup org-dog-context nil
  ""
  :prefix "org-dog-context-"
  :group 'org-dog-context)

(defcustom org-dog-context-alist
  '((project
     :value-fn project-current
     :test equal
     :callback org-dog-context-project-1)
    (major-mode
     :callback org-dog-context-major-mode-1)
    (current-language-environment
     :callback org-dog-context-language-1))
  ""
  :type '(alist :key-type symbol
                :value-type (plist
                             :options ((list (const :value-fn)
                                             function)
                                       (list (const :test)
                                             function)
                                       (list (const :callback)
                                             function)))))

(defvar org-dog-context-cache nil
  "A hash table.")

(cl-defstruct org-dog-context file-whitelisted-p file-masked-p)

(defun org-dog-context (&optional force)
  (thread-last
    org-dog-context-alist
    (mapcar (pcase-lambda (`(,type . ,_))
              (org-dog-context-edge type force)))
    (delq nil)))

(defun org-dog-context-make-file-filter ()
  (apply-partially
   (lambda (predicates operand)
     (seq-reduce (lambda (cur p)
                   (when cur
                     (not (funcall p operand))))
                 predicates
                 t))
   (thread-last
     (org-dog-context)
     (mapcar (pcase-lambda (`(,_ . ,context))
               (apply-partially
                (lambda (context x)
                  (when context
                    (and (funcall (org-dog-context-file-masked-p context)
                                  x)
                         (not
                          (funcall (org-dog-context-file-whitelisted-p context)
                                   x)))))
                context)))
     (delq nil))))

(defun org-dog-context-edge (type &optional force arg)
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
            (if force
                (let ((tbl (or (cdr (assq type org-dog-context-cache))
                               (org-dog-context--make-table type))))
                  (pcase (gethash arg tbl :dflt)
                    (:dflt
                     (when-let (context (funcall callback arg))
                       (let ((files (org-dog-select-files
                                     (org-dog-context-file-whitelisted-p context))))
                         (puthash arg files tbl)
                         files)))
                    (`nil
                     nil)
                    (files
                     files)))
              (funcall callback arg))))))

(defun org-dog-context-whitelisted-files (type)
  "Return file objects whitelisted by context TYPE."
  (cdr (org-dog-context-edge type t)))

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

;;;;; Example context functions

(defun org-dog-context-project-1 (project)
  (require 'project)
  (pcase (file-name-split (abbreviate-file-name (project-root project)))
    (`("~" "work" ,_ ,group ,name "")
     (let ((regexp (rx-to-string `(and (or ,name ,group) (?  "-devel")))))
       (make-org-dog-context
        :file-whitelisted-p
        (org-dog-make-file-pred :relative-prefix "projects/"
                                :basename-regexp regexp)
        :file-masked-p
        (org-dog-make-file-pred :relative-prefix "projects/"))))))

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
      (let ((regexp (rx-to-string `(or ,@filenames))))
        (make-org-dog-context
         :file-whitelisted-p
         (org-dog-make-file-pred :relative-prefix "programming/"
                                 :basename-regexp regexp)
         :file-masked-p
         (org-dog-make-file-pred :relative-prefix "programming/"))))))

(defun org-dog-context-language-1 (language)
  (let ((language (save-match-data
                    (pcase language
                      ((rx bol "UTF-")
                       "English")
                      ("ASCII"
                       "English")
                      ((rx bol (group (+ (not (any "-")))))
                       (match-string 1 language))))))
    (make-org-dog-context
     :file-whitelisted-p
     (org-dog-make-file-pred :relative-prefix "languages/"
                             :basename-regexp (regexp-quote language))
     :file-masked-p
     (org-dog-make-file-pred :relative-prefix "languages/"))))

(provide 'org-dog-context)
;;; org-dog-context.el ends here

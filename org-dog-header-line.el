;;; org-dog-header-line.el --- Header line -*- lexical-binding: t -*-

(require 'org-dog)

(defgroup org-dog-header-line nil
  ""
  :prefix "org-dog-header-line-"
  :group 'org-dog)

(defcustom org-dog-header-line-format
  '((org-dog-buffer-file-object
     (:eval (oref org-dog-buffer-file-object relative)))
    (org-dog-indirect-buffer-p
     " [%b]")
    " "
    (org-dog-buffer-file-object
     (:eval (org-dog-header-line-format-olp org-dog-buffer-file-object))
     org-dog-header-line-fallback-olp))
  ""
  :type 'sexp
  :risky t)

;;;; Mode line constructs

(defvar org-dog-header-line-fallback-olp
  '(:eval (if-let (olp (ignore-errors
                         (org-get-outline-path t t)))
              (org-format-outline-path
               (thread-last
                 olp
                 (reverse)
                 (mapcar #'substring-no-properties))
               nil nil "\\")
            "")))

;;;; Other variables

(defvar org-dog-header-line--orig-format nil)

;;;###autoload
(defun org-dog-header-line-enable ()
  "Enable a custom header line for an Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This function must be called in org-mode"))
  (when (org-dog-header-line--available-p)
    (when (and header-line-format
               (not org-dog-header-line--orig-format))
      (setq-local org-dog-header-line--orig-format header-line-format))
    (setq-local header-line-format org-dog-header-line-format)))

(defun org-dog-header-line--available-p ()
  (and (not (and (boundp 'org-capture-mode)
                 (buffer-local-value 'org-capture-mode (current-buffer))))
       (or org-dog-buffer-file-object
           (buffer-file-name (org-base-buffer (current-buffer))))))

;;;; Formatting

(cl-defgeneric org-dog-header-line-format-olp (_x)
  "Format the entry context according to the file type."
  (format-mode-line org-dog-header-line-fallback-olp))

(provide 'org-dog-header-line)
;;; org-dog-header-line.el ends here

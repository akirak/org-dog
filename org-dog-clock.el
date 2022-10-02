;;; org-dog-clock.el --- org-clock functionalities -*- lexical-binding: t -*-

(declare-function org-ql-completing-read "ext:org-ql")

(defcustom org-dog-clock-use-ql t
  "Whether to use `org-ql-completing-read' to select a heading.

It is recommended to set this option to t. Otherwise,
`org-dog-read-heading-default' will be used, but it does not
support creating a new todo heading."
  :group 'org-dog
  :type 'boolean)

(defcustom org-dog-clock-in-fallback-fn
  #'org-dog-clock-in-fallback-1
  "Fallback function used in `org-dog-clock-in'.

The function takes two arguments: the file and an input string
which is usually the title of a heading. It should creates a new
heading in the file and clock into the heading."
  :type 'function)

(defun org-dog-clock-in-fallback-1 (file title)
  "Create a new heading in a file and clock into it.

This is an example implementation of
`org-dog-clock-in-fallback-fn'."
  (let ((org-capture-entry `("" ""
                             entry
                             (file ,file)
                             ,(concat "* " title "\n%?")
                             :clock-in t :clock-resume t)))
    (org-capture)))

;;;###autoload
(cl-defun org-dog-clock-in (files &key query-prefix query-filter)
  "Clock in to some heading in one of the files."
  (let ((marker (if (and org-dog-clock-use-ql
                         (fboundp 'org-ql-completing-read))
                    (org-ql-completing-read files
                      :query-prefix query-prefix
                      :query-filter query-filter
                      :prompt "Clock in: ")
                  (org-dog-read-heading-default
                   files "Clock in: "))))
    (if marker
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (org-clock-in)))
      ;; HACK: Retrieve the last input from `minibuffer-history'. It is
      ;; currently impossible to use org-ql-completing-read to read an input
      ;; that does not match any of the candidates. See
      ;; https://github.com/alphapapa/org-ql/issues/299#issuecomment-1230170675
      (let ((title (car minibuffer-history)))
        (funcall org-dog-clock-in-fallback-fn
                 (cond
                  ((stringp files)
                   files)
                  ((= 1 (length files))
                   (car files))
                  (t
                   (completing-read (format "Files in which you'll create \"%s\": " title)
                                    (org-dog-file-completion :files files))))
                 title)))))

(provide 'org-dog-clock)
;;; org-dog-clock.el ends here

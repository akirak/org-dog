;;; org-dog-clock.el --- org-clock functionalities -*- lexical-binding: t -*-

(require 'org-macs)
(require 'org-capture)
(require 'org-dog)
(require 'map)

(defvar org-capture-entry)
(declare-function org-ql-completing-read "ext:org-ql")

(defgroup org-dog-clock nil
  "Integration with org-clock."
  :group 'org-dog)

(defcustom org-dog-clock-use-ql t
  "Whether to use `org-ql-completing-read' to select a heading.

It is recommended to set this option to t. Otherwise,
`org-dog-read-heading-default' will be used, but it does not
support creating a new todo heading."
  :group 'org-dog-clock
  :type 'boolean)

(defcustom org-dog-clock-in-fallback-fn
  #'org-dog-clock-in-fallback-1
  "Fallback function used in `org-dog-clock-in'.

The function should take at least two arguments: the file and an
input string which is usually the title of a heading. Extra
arguments to `org-dog-clock-in' are passed as is. It should
creates a new heading in the file and clock into the heading."
  :type 'function)

(defcustom org-dog-before-clock-in-functions nil
  "List of functions called before clocking in from `org-clock-in'.

Each function is called with one argument, the marker to the
entry being clocked in."
  :type 'hook)

(defcustom org-dog-clock-in-hook nil
  "Hook to run after clocking in in `org-dog-clock-in'.

`org-dog-clock-last-marker' is set to the last value of
`org-clock-marker', so you can use it in the hook."
  :type 'hook)

(defcustom org-dog-clock-in-success-hook nil
  "Hook run after `org-dog-clock-in' successfully starts a clock on a marker.

Each function in the hook will be called with the marker clocked
in as the argument. In fallback cases, this hook is not run."
  :type 'hook)

(defcustom org-dog-clock-default-query-prefix nil
  "Default query prefix passed to `org-ql-completing-read'."
  :type '(choice string (const nil)))

(defcustom org-dog-clock-default-query-filter nil
  "Default query filter passed to `org-ql-completing-read'."
  :type '(choice function (const nil)))

(defvar org-dog-clock-last-marker nil
  "`org-clock-marker' before `org-dog-clock-in' is called.")

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
(cl-defun org-dog-clock-in (files &rest args &key query-prefix query-filter
                                  (prompt "Clock in: ")
                                  &allow-other-keys)
  "Clock in to some heading in one of the files."
  (let ((marker (if (and org-dog-clock-use-ql
                         (fboundp 'org-ql-completing-read))
                    (org-ql-completing-read files
                      :query-prefix (or query-prefix
                                        org-dog-clock-default-query-prefix)
                      :query-filter (or query-filter
                                        org-dog-clock-default-query-filter)
                      :prompt prompt)
                  (org-dog-read-heading-default
                   files prompt))))
    (if marker
        (progn
          (with-current-buffer (marker-buffer marker)
            (org-with-wide-buffer
             (goto-char marker)
             (setq org-dog-clock-last-marker org-clock-marker)
             (run-hook-with-args 'org-dog-before-clock-in-functions org-clock-marker)
             (org-clock-in)
             (run-hooks 'org-dog-clock-in-hook)))
          (run-hook-with-args 'org-dog-clock-in-success-hook marker))
      ;; HACK: Retrieve the last input from `minibuffer-history'. It is
      ;; currently impossible to use org-ql-completing-read to read an input
      ;; that does not match any of the candidates. See
      ;; https://github.com/alphapapa/org-ql/issues/299#issuecomment-1230170675
      (let ((title (car minibuffer-history)))
        (apply org-dog-clock-in-fallback-fn
               (cond
                ((stringp files)
                 files)
                ((= 1 (length files))
                 (car files))
                (t
                 (completing-read (format "Files in which you'll create \"%s\": " title)
                                  (org-dog-file-completion :files files))))
               title
               (thread-first
                 args
                 (map-delete :prompt)
                 (map-delete :query-prefix)
                 (map-delete :query-filter)))))))

(provide 'org-dog-clock)
;;; org-dog-clock.el ends here

;;; org-dog-utils.el ---  -*- lexical-binding: t -*-

(require 'org)
(require 'subr-x)
(require 'ts)

(defvar org-keyword-regexp)

(declare-function org-element-timestamp-parser "org-element")

(defsubst org-dog-case-fold-equal (string1 string2)
  "Compare two strings, ignoring case."
  (equal (downcase string1)
         (downcase string2)))

(defmacro org-dog-with-file-header (file &rest progn)
  "Evaluate a block with the headers of an Org file as buffer."
  (declare (indent 1))
  `(if-let (buf (find-buffer-visiting ,file))
       (with-current-buffer buf
         (org-with-wide-buffer
          (goto-char (point-min))
          (when (re-search-forward org-heading-regexp nil t)
            (narrow-to-region (point-min) (1- (point)))
            (goto-char (point-min)))
          ,@progn))
     (with-temp-buffer
       (insert-file-contents ,file)
       (goto-char (point-min))
       ;; Drop entries of the file for faster enabling of org-mode.
       (when (re-search-forward org-heading-regexp nil t)
         (delete-region (match-beginning 0) (point-max))
         (goto-char (point-min)))
       (let ((org-inhibit-startup t))
         (delay-mode-hooks (org-mode)))
       ,@progn)))

(defun org-dog--file-title ()
  "Return the title of the current Org buffer, if any."
  (catch 'org-dog-file-title
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (<= (point) (point-max))
       (save-match-data
         (when (and (looking-at org-keyword-regexp)
                    (org-dog-case-fold-equal (match-string 1)
                                             "title"))
           (throw 'org-dog-file-title
                  (substring-no-properties (string-trim (match-string 2)))))
         (when (looking-at org-heading-regexp)
           (throw 'org-dog-file-title nil))
         (forward-line)))
     nil)))

(defun org-dog--latest-inactive-ts ()
  "Return the latest inactive timestamp in the buffer."
  (let (result)
    (while (re-search-forward org-ts-regexp-inactive nil t)
      (save-excursion
        (goto-char (match-beginning 0))
        (let ((ts (thread-last
                    (org-element-timestamp-parser)
                    (org-timestamp-to-time)
                    (float-time)
                    (make-ts :unix))))
          (when (or (not result)
                    (ts> ts result))
            (setq result ts)))))
    result))

(provide 'org-dog-utils)
;;; org-dog-utils.el ends here

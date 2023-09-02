;;; org-dog-utils.el ---  -*- lexical-binding: t -*-

(require 'org)
(require 'subr-x)
(require 'ts)

(defvar org-keyword-regexp)

(declare-function org-element-timestamp-parser "org-element")

(defvar org-dog-visited-file-name nil)

(eval-and-compile
  (if (version< "29" emacs-version)
      (defalias 'org-dog-case-fold-equal #'string-equal-ignore-case)
    (defsubst org-dog-case-fold-equal (string1 string2)
      "Compare two strings, ignoring case."
      (equal (downcase string1)
             (downcase string2)))))

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
       ;; You can add this if necessary
       ;; (setq-local org-dog-visited-file-name ,file)
       (insert-file-contents ,file)
       (goto-char (point-min))
       ;; Drop entries of the file for faster enabling of org-mode.
       (when (re-search-forward org-heading-regexp nil t)
         (delete-region (match-beginning 0) (point-max))
         (goto-char (point-min)))
       (let ((org-inhibit-startup t)
             ;; Don't load modules.
             (org-modules-loaded t))
         (delay-mode-hooks (org-mode)))
       ,@progn)))

(defmacro org-dog-with-file-header-1 (file &rest progn)
  "Evaluate a block with the headers of an Org file as buffer.

This is like `org-dog-with-file-header', but call
`org-set-regexps-and-options' instead of running `org-mode'. This
is about 25% faster."
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
       ;; You can add this if necessary
       ;; (setq-local org-dog-visited-file-name ,file)
       (insert-file-contents ,file)
       (goto-char (point-min))
       ;; Drop entries of the file for faster enabling of org-mode.
       (when (re-search-forward org-heading-regexp nil t)
         (delete-region (match-beginning 0) (point-max))
         (goto-char (point-min)))
       (org-set-regexps-and-options)
       ,@progn)))

(defmacro org-dog-with-file-content (file &rest progn)
  "Evaluate a block with the headers of an Org file as buffer.

Like `org-dog-with-file-header-1', but without narrowing."
  (declare (indent 1))
  `(if-let (buf (find-buffer-visiting ,file))
       (with-current-buffer buf
         (org-with-wide-buffer
          (goto-char (point-min))
          ,@progn))
     (with-temp-buffer
       ;; You can add this if necessary
       ;; (setq-local org-dog-visited-file-name ,file)
       (insert-file-contents ,file)
       (goto-char (point-min))
       (org-set-regexps-and-options)
       ,@progn)))

(cl-defsubst org-dog--time> (t1 t2)
  (not (time-less-p t1 t2)))

(defun org-dog-search-keyword-line (keyword &optional noprops)
  "Find a next header matching a keyword.

This should be called inside `org-dog-with-file-header'.
 Otherwise, it will look in the entire buffer, which can take
 longer than it should have done."

  (catch 'org-dog-header
    (while (re-search-forward org-keyword-regexp nil t)
      (when (org-dog-case-fold-equal (match-string 1)
                                     keyword)
        (throw 'org-dog-header
               (if noprops
                   (string-trim (match-string-no-properties 2))
                 (string-trim (match-string 2))))))
    nil))

(defun org-dog-insert-keyword-line (keyword content)
  (when (looking-at org-property-drawer-re)
    (goto-char (match-end 0))
    (forward-line 1))
  (while (looking-at org-keyword-regexp)
    (forward-line 1))
  (insert "#+" keyword ": " content "\n"))

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

;;;; Build regular expressions for a particular period

(defun org-dog-inactive-ts-regexp (start &optional end)
  (concat "\\[\\("
          (org-dog--date-range-regexp start (or end (current-time)))
          " [^z-a]*?\\)\\]"))

(defun org-dog--date-range-regexp (start end)
  (let ((end-string (format-time-string "%F" end))
        (date (decode-time start))
        strings)
    (catch 'finish
      (while t
        (let ((str (format-time-string "%F" (encode-time date))))
          (push str strings)
          (when (equal str end-string)
            (throw 'finish t))
          (setq date (decoded-time-add date (make-decoded-time :day 1))))))
    (rx-to-string `(or ,@strings))))

(defun org-dog--day-start (time)
  (let ((decoded (decode-time time)))
    (when (and org-extend-today-until
               (< (nth 2 decoded) org-extend-today-until))
      (decoded-time-add decoded (make-decoded-time :day -1)))
    (setf (decoded-time-minute decoded) 0)
    (setf (decoded-time-second decoded) 0)
    (setf (decoded-time-hour decoded) (or org-extend-today-until 0))
    (encode-time decoded)))

(provide 'org-dog-utils)
;;; org-dog-utils.el ends here

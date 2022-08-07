;;; org-dog-tablist.el --- Tabulated-list interface -*- lexical-binding: t -*-

(require 'org-dog)
(require 'tabulated-list)

;;;###autoload
(defun org-dog-tablist-files ()
  "Browse org-dog files in a tabulated list interface."
  (interactive)
  (with-current-buffer (get-buffer-create "*Org Dog Tablist Files*")
    (let ((inhibit-read-only t))
      (erase-buffer))
    (org-dog-tablist-files-mode)
    (tabulated-list-revert)
    (pop-to-buffer (current-buffer))))

(define-derived-mode org-dog-tablist-files-mode tabulated-list-mode
  "DogTablistFiles"
  "Major mode for displaying a list of files."
  (setq tabulated-list-format [("Relative" 36 t)
                               ("Repo" 12 t)
                               ("Class" 16 t)
                               ("Agenda" 3 t)
                               ("Activity" 10 t)])
  (setq tabulated-list-padding 2)
  ;; I am not sure what would be the best here, so I may change this later.
  (setq tabulated-list-sort-key '("Relative" . nil))
  (add-hook 'tabulated-list-revert-hook #'org-dog-tablist-files-refresh nil t)
  (tabulated-list-init-header))

(defun org-dog-tablist-files-refresh ()
  (let ((agenda-files (thread-last
                        (org-agenda-files)
                        (mapcar #'abbreviate-file-name))))
    (setq tabulated-list-entries
          (thread-last
            (org-dog-select-files)
            (mapcar (lambda (obj)
                      (list (oref obj absolute)
                            (vector (oref obj relative)
                                    (oref obj root)
                                    (org-dog--format-class
                                     (eieio-object-class obj))
                                    (if (member (oref obj absolute)
                                                agenda-files)
                                        "Yes"
                                      "No")
                                    (with-current-buffer
                                        (org-dog-file-buffer obj)
                                      (org-with-wide-buffer
                                       (goto-char (point-min))
                                       (if-let (ts (org-dog--latest-inactive-ts))
                                           (ts-format "%F" ts)
                                         "")))))))))))

(provide 'org-dog-tablist)
;;; org-dog-tablist.el ends here

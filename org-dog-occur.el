;;; org-dog-occur.el --- Search in an Org buffer -*- lexical-binding: t -*-

(require 'org)
(require 'org-dog-utils)

(defvar org-radio-target-group)
(declare-function thing-at-point-looking-at "thingatpt")

(defgroup org-dog-occur nil
  ""
  :group 'org-dog-occur)

(defconst org-dog-occur-buffer
  "*Org Dog Occur*")

(defface org-dog-occur-query-face
  '((t :inherit bold))
  "Face for the query in the org-dog-occur buffer."
  :group 'org-dog-occur)

(defvar org-dog-occur-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-dog-occur-jump)
    (define-key map (kbd "SPC") #'org-dog-occur-show)
    (define-key map (kbd "n") #'org-dog-occur-next)
    (define-key map (kbd "p") #'org-dog-occur-prev)
    (define-key map (kbd "q") #'org-dog-occur-quit)
    map))

;;;; Commands in org-mode buffers

;;;###autoload
(defun org-dog-occur-link (buffer &optional target radio-p)
  (interactive (list (progn
                       (unless (derived-mode-p 'org-mode)
                         (user-error "This command must run in org-mode"))
                       (current-buffer))))
  (if target
      (org-dog-occur--show-occurrences
       target (org-dog-occur--search-links buffer target radio-p))
    (let* ((targets (org-dog-occur--link-targets buffer))
           (target (completing-read "Target: " targets)))
      (org-dog-occur--show-occurrences
       target (org-dog-occur--search-links
               buffer
               target
               (nth 2 (assoc target targets)))))))

;;;###autoload
(defun org-dog-occur-link-at-point ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must run in org-mode"))
  (require 'thingatpt)
  (apply #'org-dog-occur-link
         (current-buffer)
         (save-match-data
           (cond
            ((thing-at-point-looking-at org-radio-target-regexp)
             (list (match-string 1) t))
            ((thing-at-point-looking-at org-target-link-regexp)
             (list (match-string 1) t))
            ((thing-at-point-looking-at org-target-regexp)
             (list (match-string 1)))
            ((thing-at-point-looking-at org-link-bracket-re)
             (list (match-string 1)))))))

(defun org-dog-occur--show-occurrences (target matches)
  "Return a buffer that shows links occurrences."
  (with-current-buffer (get-buffer-create org-dog-occur-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Occurrences of "
              (propertize target 'face 'org-dog-occur-query-face)
              ":" ?\n)
      (pcase-dolist (`(,path ,marker ,occurrence) matches)
        (let ((start (point)))
          (insert path "\n")
          (unless (string-empty-p occurrence)
            (insert occurrence "\n"))
          (let ((overlay (make-overlay start (point))))
            (overlay-put overlay 'marker marker))
          (insert "\n"))))
    (goto-char (point-min))
    (visual-line-mode t)
    (setq-local buffer-read-only t)
    (use-local-map org-dog-occur-map)
    (pop-to-buffer (current-buffer))))

(defun org-dog-occur--search-links (buffer target radio-p)
  "Find occurrences of links to a particular target."
  (org-dog-occur--search-regexp buffer
                                (if radio-p
                                    (regexp-quote target)
                                  (rx-to-string `(or ,(concat "[[" target "]]")
                                                     ,(concat "<<" target ">>"))))))

(defun org-dog-occur--search-regexp (buffer regexp)
  (with-current-buffer buffer
    (thread-last
      (org-map-entries
       `(lambda ()
          (let* ((start (point-marker))
                 (end (org-entry-end-position))
                 (match (re-search-forward ,regexp end t)))
            (when match
              (list (org-format-outline-path
                     (org-get-outline-path t t))
                    (point-marker)
                    (save-excursion
                      (beginning-of-line 1)
                      (while (not (looking-at (rx (or eol (any "#:*")))))
                        (forward-line -1))
                      (forward-line 1)
                      (let ((start (point)))
                        (while (not (looking-at (rx (or eol (any "#:*")))))
                          (forward-line 1))
                        (string-chop-newline
                         (buffer-substring start (1- (point))))))))))
       nil 'file)
      (delq nil))))

(defun org-dog-occur--link-targets (buffer)
  "Return a list of link targets in BUFFER."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (result)
       (save-match-data
         (while (re-search-forward org-target-regexp nil t)
           (push (list (substring-no-properties (match-string 1))
                       (point-marker)
                       ;; Is a radio target?
                       (looking-at ">"))
                 result)))
       result))))

(defun org-dog-occur--nonradio-targets (buffer)
  "Return a list of non-radio targets in BUFFER.

This function filters non-radio targets from the result of
`org-dog-occur--link-targets'."
  (seq-filter (pcase-lambda (`(,_ ,_ ,radio-p))
                (not radio-p))
              (org-dog-occur--link-targets buffer)))

;;;###autoload
(defun org-dog-insert-internal-link ()
  "Insert a link to an in-buffer target."
  (interactive)
  (let* ((targets (org-dog-occur--nonradio-targets (current-buffer)))
         (orig-text (when (use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))))
         (target (completing-read "Link target: "
                                  targets nil nil orig-text)))
    (when orig-text (delete-region (region-beginning) (region-end)))
    (insert (org-link-make-string target
                                  (when (and orig-text
                                             (org-dog-case-fold-equal
                                              orig-text target))
                                    orig-text)))))

;;;; Commands available in org-dog-occur-buffer

(defun org-dog-occur-jump ()
  "Jump to the source of the occurrence at point."
  (interactive)
  (org-dog-occur-show t))

(defun org-dog-occur-show (&optional switch)
  "Display the source of the occurrence at point."
  (interactive)
  (when-let (marker (catch 'marker
                      (dolist (ov (overlays-at (point)))
                        (when-let (marker (overlay-get ov 'marker))
                          (throw 'marker marker)))))
    (with-current-buffer (marker-buffer marker)
      (if switch
          (pop-to-buffer (current-buffer))
        (display-buffer (current-buffer)))
      (with-selected-window (get-buffer-window (current-buffer))
        (org-goto-marker-or-bmk marker)
        (recenter)))))

(defun org-dog-occur-next ()
  "Go to the next occurrence in the buffer."
  (interactive)
  (catch 'found
    (let (pos)
      (while (setq pos (next-overlay-change (point)))
        (goto-char pos)
        (dolist (ov (overlays-at (point)))
          (when-let (marker (overlay-get ov 'marker))
            (org-dog-occur-show)
            (throw 'found t)))))))

(defun org-dog-occur-prev ()
  "Go to the previous occurrence in the buffer."
  (interactive)
  (catch 'found
    (let (pos)
      (while (setq pos (previous-overlay-change (point)))
        (goto-char pos)
        (dolist (ov (overlays-at (point)))
          (when-let (marker (overlay-get ov 'marker))
            (org-dog-occur-show)
            (throw 'found t)))))))

(defun org-dog-occur-quit ()
  (interactive)
  (quit-window))

(provide 'org-dog-occur)
;;; org-dog-occur.el ends here

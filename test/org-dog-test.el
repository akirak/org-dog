;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'org-dog)

;;;; Contexts

(describe "org-dog-major-mode-context-1"
  (it "returns an `org-dog-context' object"
    (expect (org-dog-context-p (org-dog-major-mode-context-1 'emacs-lisp-mode))
            :to-be-truthy))

  (it "returns nil on fundamental-mode"
    (expect (org-dog-major-mode-context-1 'fundamental-mode)
            :to-be nil))

  (it "returns nil on a major mode inheriting special-mode"
    (expect (org-dog-major-mode-context-1 'tabulated-list-mode)
            :to-be nil)))

;;;; Utilities

(describe "org-dog-inactive-ts-regexp"
  (it "The returned regexp matches long timestamps within the range"
    (expect (format-time-string (org-time-stamp-format t t)
                                (org-read-date nil t "2020-09-30 09:00:00"))
            :to-match
            (org-dog-inactive-ts-regexp (org-read-date nil t "2020-09-30")
                                        (org-read-date nil t "2020-10-02")))
    (expect (format-time-string (org-time-stamp-format t t)
                                (org-read-date nil t "2020-10-01 09:00:00"))
            :to-match
            (org-dog-inactive-ts-regexp (org-read-date nil t "2020-09-30")
                                        (org-read-date nil t "2020-10-02")))
    (expect (format-time-string (org-time-stamp-format t t)
                                (org-read-date nil t "2020-10-02 09:00:00"))
            :to-match
            (org-dog-inactive-ts-regexp (org-read-date nil t "2020-09-30")
                                        (org-read-date nil t "2020-10-02"))))

  (it "The returned regexp matches short timestamps within the range"
    (expect (format-time-string (org-time-stamp-format nil t)
                                (org-read-date nil t "2020-09-30 09:00:00"))
            :to-match
            (org-dog-inactive-ts-regexp (org-read-date nil t "2020-09-30")
                                        (org-read-date nil t "2020-10-02"))))

  (it "The returned regexp does not match long timestamps out of the range"
    (expect (format-time-string (org-time-stamp-format t t)
                                (org-read-date nil t "2020-09-01 09:00:00"))
            :not :to-match
            (org-dog-inactive-ts-regexp (org-read-date nil t "2020-09-30")
                                        (org-read-date nil t "2020-10-02")))
    (expect (format-time-string (org-time-stamp-format t t)
                                (org-read-date nil t "2020-10-03 09:00:00"))
            :not :to-match
            (org-dog-inactive-ts-regexp (org-read-date nil t "2020-09-30")
                                        (org-read-date nil t "2020-10-02")))))

(provide 'org-dog-test)

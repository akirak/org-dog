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

(provide 'org-dog-test)

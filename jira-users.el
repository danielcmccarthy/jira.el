;; jira-users.el --- Jira user database  -*- lexical-binding: t; -*-

(require 'jira-api)

(defcustom jira-users-max-results
  1000
  "Maximum number of Jira usernames to retrieve."
  :group 'jira :type 'integer)

(defvar jira-users
  nil
  "Hash table of all Jira users.")

(defun jira-users-get-users ()
  "Fetch the list of all Jira user names and IDs and store it in `jira-users'."
  (interactive)
  (let ((table (make-hash-table :test #'equal)))
    ;; Theses params are undocumented but work:
    ;; https://stackoverflow.com/a/64786638
    (jira-api-call "GET"
                   "users/search"
                   :params
                   `((query . "+")
                     (maxResults . ,jira-users-max-results))
                   :callback
                   (lambda (data _response)
                     (mapc #'(lambda (u)
                               (let ((id (alist-get 'accountId u))
                                     (name (alist-get 'displayName u)))
                                 (unless (eq :json-false (alist-get 'active u))
                                   (setf (gethash name table) id))))
                           data)))
    (setq jira-users table)))

(provide 'jira-users)

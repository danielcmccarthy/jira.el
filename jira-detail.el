;;; jira-detail.el --- Jira Detail  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pablo González Carrizo

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>
;; Created: 2025-02-16

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Show information about a specific Jira elements.

;;; Code:

(require 'magit-section)
(require 'transient)

(require 'jira-actions)
(require 'jira-api)
(require 'jira-doc)
(require 'jira-fmt)
(require 'jira-table)
(require 'jira-utils)
(require 'jira-complete)

(defvar-local jira-detail--current nil
  "All data from current issue being displayed in the detail view.")

(defvar-local jira-detail--current-key nil
  "The key of the current issue being displayed in the detail view.")

(defvar-local jira-detail--current-update-metadata nil
  "Metadata needed to updates fields from the current issue.")

(defvar-local jira-comment--issue-key nil
  "The key of the Jira issue for which a comment is being added.")

(defvar-local jira-comment--callback nil
  "The callback function to call after adding a comment.")

(defconst jira-detail--updatable-fields
  (list (cons "Parent Issue" "parent")
        (cons "Description" "description")
	(cons "Issue Type" "issuetype")
	(cons "Resolution" "resolution")
	(cons "Project" "project")
	(cons "Fix Versions" "fixVersions")
	(cons "Summary" "summary")
	(cons "Components" "components")
	(cons "Priority" "priority")
	(cons "Labels" "labels")
	(cons "Assignee" "assignee")
	(cons "Reporter" "reporter")
	(cons "Due Date" "duedate")
	(cons "Original Estimate" "originalEstimate")
	(cons "Remaining Estimate" "remainingEstimate")
	(cons "Sprint" (cons :custom "Sprint"))
	(cons "Business Line" (cons :custom "Business line"))
	(cons "Cost Center" (cons :custom "Cost center")))
  "List of fields that can be updated in the Jira detail view.")

(defconst jira-comment-instruction-line
  ";; Write your comment below - Press C-c C-c to send or C-c C-k to cancel."
  "The instruction line shown in Jira comment buffers.")

(defconst jira-description-instruction-line
  ";; Write the description below - Press C-c C-c to save or C-c C-k to cancel."
  "The instruction line shown in Jira description editor buffers.")

(defcustom jira-comments-display-recent-first
  t
  "The order to display Jira comments."
  :type '(choice (const :tag "Newest first" t)
                 (const :tag "Oldest first" nil))
  :group 'jira)

;; they override the default formatters specified
;; in jira-issues-fields
(defvar jira-detail-formatters
  '((:key . jira-fmt-issue-key-not-tabulated)
    (:parent-key . jira-fmt-issue-key-not-tabulated)))

(defun jira-detail--header (header)
  "Format HEADER to be used as a header in the detail view."
  (concat
   (propertize header 'face 'italic)
   (when (> (- 16 (string-width header)) 0)
     (make-string (- 16 (string-width header)) ?\s))))

(defun jira-detail--issue-fmt (issue field)
  "Extract FIELD from ISSUE and format it."
  (let* ((extracted (jira-table-extract-field jira-issues-fields field issue))
         (value (or extracted ""))
         (formatter
          (or (cdr (assoc field jira-detail-formatters))
              (jira-table-field-formatter jira-issues-fields field))))
    (if formatter (funcall formatter value) (format "%s" value))))

(defun jira-detail--issue-summary (issue)
  "Show the summary of the ISSUE."
  (let ((key (jira-detail--issue-fmt issue :key))
        (type (jira-detail--issue-fmt issue :issue-type-name))
        (status (jira-detail--issue-fmt issue :status-name))
        (resolution (jira-detail--issue-fmt issue :resolution))
        (project (jira-detail--issue-fmt issue :project-name))
        (estimate (jira-detail--issue-fmt issue :original-estimate))
        (remaining (jira-detail--issue-fmt issue :remaining-time))
        (ratio (jira-detail--issue-fmt issue :work-ratio))
        (fix-versions (jira-detail--issue-fmt issue :fix-versions))
        (summary (jira-detail--issue-fmt issue :summary)))
    (insert (jira-detail--header "Issue Key") key " (" type")\n")
    (insert (jira-detail--header "Status") status)
    (if (and resolution (not (string= resolution "")))
        (insert " (" resolution ")\n")
      (insert "\n"))
    (insert (jira-detail--header "Project") project "\n")
    (insert (jira-detail--header "Time")
            estimate
            " (remaining: " remaining ")\n")
    (insert (jira-detail--header "Work Ratio") ratio "\n")
    (insert (jira-detail--header "Fix Versions") fix-versions "\n")
    (insert (jira-detail--header "Summary") summary "\n")))


(defun jira-detail--issue-team (issue)
  "Show the team information of the ISSUE."
  (let ((line (jira-detail--issue-fmt issue :line))
        (parent-key (jira-detail--issue-fmt issue :parent-key))
        (parent-type (jira-detail--issue-fmt issue :parent-type-name))
        (parent-status (jira-detail--issue-fmt issue :parent-status))
        (sprint (jira-detail--issue-fmt issue :sprints))
        (components (jira-detail--issue-fmt issue :components)))
    (insert (jira-detail--header "Business Line") line "\n")
    (insert (jira-detail--header "Parent")
            parent-key " (" parent-type ") (" parent-status ")\n")
    (insert (jira-detail--header "Sprint") sprint "\n")
    (insert (jira-detail--header "Components") components "\n")))

(defun jira-detail--issue-manager-data (issue)
  "Show the manager data of the ISSUE."
  (let ((cost-center (jira-detail--issue-fmt issue :cost-center))
        (priority (jira-detail--issue-fmt issue :priority-name))
        (labels (jira-detail--issue-fmt issue :labels))
        (assignee (jira-detail--issue-fmt issue :assignee-name))
        (reporter (jira-detail--issue-fmt issue :reporter-name))
        (due-date (jira-detail--issue-fmt issue :due-date)))
    (insert (jira-detail--header "Cost Center") cost-center "\n")
    (insert (jira-detail--header "Priority") priority "\n")
    (insert (jira-detail--header "Labels") labels "\n")
    (insert (jira-detail--header "Assignee")  assignee "\n")
    (insert (jira-detail--header "Reporter") reporter "\n")
    (insert (jira-detail--header "Due Date") due-date "\n")))

(defun jira-detail--description (issue)
  "Show the description of the ISSUE."
  (let* ((doc (alist-get 'description (alist-get 'fields issue))))
    (insert  (jira-doc-format doc))))

(defcustom jira-detail-show-announcements t
  "Whether to show announcements in Jira detail view."
  :type 'boolean
  :group 'jira)

(cl-defun jira-detail--issue (key issue)
  "Show the detailed information of the ISSUE with KEY."
  (with-current-buffer (get-buffer-create (concat "*Jira Issue Detail: [" key "]*"))
    (jira-detail)
    (setq jira-detail--current-key key)
    (setq jira-detail--current issue)
    ;; prepare the information about how to update each field
    ;; so that it is ready if user asks for it
    (jira-detail--ensure-update-metadata)
    ;; avoid horizontal scroll
    (setq truncate-lines nil)
    (visual-line-mode 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; section: Jira Issue Summary
      (magit-insert-section (jira-issue-summary nil nil)
        (magit-insert-heading "Jira Issue Summary")
        (magit-insert-section-body (jira-detail--issue-summary issue)))
      (insert "\n")
      ;; section: Information
      (magit-insert-section (jira-issue-information nil nil)
        ;; section: Team Data
        (magit-insert-section (team-data nil nil)
          (magit-insert-heading "Team Data")
          (magit-insert-section-body (jira-detail--issue-team issue))
          (insert "\n"))
        ;; section: Manager Data
        (magit-insert-section (manager-data nil nil)
          (magit-insert-heading "Manager Data")
          (magit-insert-section-body
            (jira-detail--issue-manager-data issue)
            (insert "\n")
            (when jira-detail-show-announcements
              (insert "----------------------------------------------------------------\n")
              (insert (propertize "jira.el v2.0" 'face 'jira-face-h1) " allows you to ")
              (insert "update fields with smart suggestions!\n")
              (insert "Press " (propertize "?" 'face 'jira-face-date) " to open transient menu, ")
              (insert "and " (propertize "U" 'face 'jira-face-date) " to update a field.")
              (insert "\n----------------------------------------------------------------\n\n"))))
        (magit-insert-section (description nil nil)
          (magit-insert-heading "Description")
          (magit-insert-section-body
            (jira-detail--description issue)
            (insert "\n\n")))))
    (jira-detail--show-attachments key issue)
    (jira-detail--show-comments key)
    (pop-to-buffer (current-buffer))))


(defun jira-detail--create-editor-buffer
    (buffer-name initial-content instructions save-callback)
  "Create and display an editor buffer with INITIAL-CONTENT and a SAVE-CALLBACK."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert instructions "\n\n")
      (insert initial-content)
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c")
          (lambda () (interactive)
            (let ((content (buffer-string)))
              (kill-buffer buf)
              (funcall save-callback
		       (string-trim (string-remove-prefix instructions content))))))
        (define-key map (kbd "C-c C-k")
          (lambda () (interactive) (kill-buffer buf)))
        (set-buffer-modified-p nil)
        (use-local-map map))
      (display-buffer buf)
      (select-window (get-buffer-window buf)))))

(defun jira-detail--add-comment (key)
  "Add a comment to the issue with KEY."
  (jira-detail--create-editor-buffer
   (format "*Jira Comment: %s*" key)
   "" jira-comment-instruction-line
   (lambda (content)
     (jira-actions-add-comment
      key content
      (lambda () (jira-detail-show-issue key))))))

(defun jira-detail--edit-description-and-update ()
  "Open a buffer to edit the description and update the issue."
  (let ((key jira-detail--current-key))
    (jira-detail--create-editor-buffer
     (concat "*Jira Description [" key "]*")
     ""
     jira-description-instruction-line
     (lambda (new-description)
       (jira-detail--update-field-action
	"description" (jira-doc-build new-description) key)))))

(defun jira-detail--comment-author (comment)
  "Show the author of the COMMENT."
  (concat
   (jira-fmt-bold (alist-get 'displayName (alist-get 'author comment)))
   " @ "
   (jira-fmt-datetime (alist-get 'updated comment))))

(defun jira-detail--comments (key comments)
  "Format and insert COMMENTS from issue KEY."
  (with-current-buffer (get-buffer-create (concat "*Jira Issue Detail: [" key "]*"))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (magit-insert-section (jira-issue-comments nil nil)
	(magit-insert-section (comments-list nil nil)
          (magit-insert-heading "Comments (press + to add new)")
          (magit-insert-section-body
	    (mapcar (lambda (comment)
		      (magit-insert-section (comment (alist-get 'id comment) nil)
			(magit-insert-heading (jira-detail--comment-author comment))
			(magit-insert-section-body
			  (insert (jira-doc-format (alist-get 'body comment)))
			  (insert "\n\n"))))
		    comments)))))))

(defun jira-detail--show-comments (key)
  "Retrieve and display comments for issue KEY."
  (jira-api-call
   "GET" (format "issue/%s/comment?orderBy=%screated"
                 key
                 (if jira-comments-display-recent-first
                     "-"
                   ""))
   :callback
   (lambda (data _response)
     (jira-detail--comments key (alist-get 'comments data)))))

(defvar-keymap jira-attachment-section-map
  :doc "Keymap for Jira attachment sections."
  "<RET>" #'jira-detail--get-attachment)

(defclass jira-attachment-section (magit-section)
  ((keymap :initform 'jira-attachment-section-map)))

(defun jira-detail--show-attachments (key issue)
  "Display attachments for ISSUE with key KEY."
  (let* ((fields (alist-get 'fields issue))
         (attachments (alist-get 'attachment fields)))
    (when (> (length attachments) 0)
      (with-current-buffer (get-buffer-create (concat "*Jira Issue Detail: [" key "]*"))
	(let ((inhibit-read-only t))
          (goto-char (point-max))
          (magit-insert-section (jira-issue-attachments nil nil)
	    (magit-insert-section (attachements-list nil nil)
              (magit-insert-heading "Attachments (press RET to visualize)")
              (magit-insert-section-body
		(mapcar
		 (lambda (attachment)
		   (let* ((url (url-generic-parse-url (alist-get 'content attachment)))
                          ;; FIXME: verify that filename matches
                          ;; "attachment/content/[0-9]+"
                          (id (file-name-nondirectory (url-filename url)))
                          (val (list (alist-get 'filename attachment) id)))
                     (magit-insert-section (jira-attachment-section val nil)
                       (magit-insert-section-body
			 (insert (format "%-30s %10s %5sB %s\n"
					 (alist-get 'filename attachment)
					 (alist-get 'mimeType attachment)
					 (file-size-human-readable
                                          (alist-get 'size attachment))
					 (jira-fmt-datetime
                                          (alist-get 'created attachment))))))))
		 attachments))))
          (insert "\n"))))))

(defun jira-detail--get-attachment ()
  "Get the attachment in the current section and visit it in a new buffer."
  (interactive)
  (pcase (magit-section-value-if [jira-attachment-section])
    (`(,name ,id)
     (message "Fetching %s..." name)
     (jira-api-call
      "GET" (format "attachment/content/%s" id)
      ;; don't want the default parser `json-read' here: the
      ;; attachment content is not wrapped in JSON.
      :parser #'buffer-string
      :callback
      (lambda (data _response)
        (jira-detail--show-attachment name data))))
    (_ (error "Not a Jira attachment"))))

(defun jira-detail--show-attachment (name data)
  (let ((x (generate-new-buffer-name name)))
    (pop-to-buffer x)
    (insert data)
    (goto-char (point-min))
    (normal-mode)
    (set-buffer-modified-p t)))

(defun jira-detail-show-issue (key)
  "Retrieve and show the detail information of the issue with KEY."
  (jira-api-call
   "GET" (concat "issue/" key)
   :callback (lambda (data _) (jira-detail--issue key data))))

(defun jira-detail--remove-comment-at-point ()
  (let ((current-section (magit-current-section)))
    (if current-section
	(if (string-equal (caar (magit-section-ident current-section)) "comment")
	    (let ((comment-id (magit-section-ident-value current-section)))
	      (when (yes-or-no-p (format "Really delete comment %s?" comment-id))
                (jira-actions-delete-comment
		 jira-detail--current-key
                 comment-id
		 (lambda () (jira-detail-show-issue jira-detail--current-key))))
	      )
	  (message "No comment at point"))
      (message "No comment at point"))))

(defun jira-detail--update-field-action (field-id value key)
  ;; Update FIELD-ID with VALUE for the current issue.
  (jira-api-call
   "PUT" (concat "issue/" key)
   :data `(("fields" . ((,field-id . ,value))))
   :callback (lambda (_data _response) (jira-detail-show-issue key))))

(defun jira-detail--update-timetracking-action (field-id value key)
  ;; Update timetracking FIELD-ID with VALUE for the current issue.
  (jira-api-call
   "PUT" (concat "issue/" key)
   :data `(("fields" . (("timetracking" . ((,field-id . ,value))))))
   :callback (lambda (_data _response) (jira-detail-show-issue key))))

(defun jira-detail--ensure-update-metadata ()
  ;; Ensure jira-detail--current-update-metadata is populated and return it.
  (unless jira-detail--current-update-metadata
    (setq jira-detail--current-update-metadata
          (jira-api-get-create-metadata
           (jira-table-extract-field jira-issues-fields :project-key jira-detail--current)
           (jira-table-extract-field jira-issues-fields :issue-type-id jira-detail--current)
           :sync t)))
  jira-detail--current-update-metadata)

(defun jira-detail--updatable-field-id (field-name)
  (let ((id (cdr (assoc field-name jira-detail--updatable-fields))))
    (if (and (consp id) (eq (car id) :custom))
        (cdr (assoc (cdr id) jira-fields))
      id)))

(defun jira-detail--change-issue-status ()
  ;; Change the status of the current issue.
  (let ((jira-utils-marked-item jira-detail--current-key))
    (jira-actions-change-issue-menu)))

(defun jira-detail--update-field ()
  "Update a field for the current issue."
  (let* ((field-alist jira-detail--updatable-fields)
         (field-names (mapcar #'car field-alist))
         (chosen-field-name (completing-read "Field to update: " field-names nil t)))
    (when chosen-field-name
      (jira-detail--ensure-update-metadata)
      (cond ((string= chosen-field-name "Description")
             (jira-detail--edit-description-and-update))
            ((or (string= chosen-field-name "Original Estimate")
		 (string= chosen-field-name "Remaining Estimate"))
             (let* ((field-id (jira-detail--updatable-field-id chosen-field-name))
                    (value (read-string (format "Enter value for %s: " chosen-field-name))))
               (jira-detail--update-timetracking-action field-id value jira-detail--current-key)))
            (t (let* ((field-id (jira-detail--updatable-field-id chosen-field-name))
                      (fields (alist-get 'fields jira-detail--current-update-metadata))
                      (field-metadata
                       (car (seq-filter (lambda (f) (string= (alist-get 'fieldId f) field-id))
                                        fields))))
                 (if field-metadata
                     (let ((value (jira-complete-ask-field field-metadata)))
                       (jira-detail--update-field-action field-id value jira-detail--current-key))
                   (message "Could not find metadata for field %s" field-id))))))))

(defun jira-detail-find-issue-by-key ()
  "Find and show a Jira issue by key."
  (let ((key (jira-complete-ask-issue)))
    (when key (jira-detail-show-issue key))))

(transient-define-prefix jira-detail--actions-menu ()
  "Show menu for actions on Jira Detail."
  ["Comments"
   ("+" "Add comment to issue"
    (lambda () (interactive ) (jira-detail--add-comment jira-detail--current-key)))
   ("-" "Remove comment at point"
    (lambda () (interactive ) (jira-detail--remove-comment-at-point)))]
  ["Issue Actions"
   ("C" "Change issue status"
    (lambda () (interactive) (jira-detail--change-issue-status)))
   ("U" "Update issue field"
    (lambda () (interactive) (jira-detail--update-field)))
   ("f" "Find issue by key/url"
    (lambda () (interactive) (jira-detail-find-issue-by-key)))
   ("c" "Copy selected issue id to clipboard"
    (lambda () (interactive)
      (jira-actions-copy-issues-id-to-clipboard jira-detail--current-key)))
   ("g" "Refresh issue detail"
    (lambda () (interactive)
      (jira-detail-show-issue jira-detail--current-key)))
   ("O" "Open issue in browser"
    (lambda () (interactive) (jira-actions-open-issue jira-detail--current-key)))])

(defvar jira-detail-changed-hook nil
  "Hook run after a Jira issue has been changed in jira-detail-mode.")

(defvar jira-detail-mode-map
  (let ((map (copy-keymap magit-section-mode-map)))
    (define-key map (kbd "?") 'jira-detail--actions-menu)
    (define-key map (kbd "+")
      (lambda () (interactive ) (jira-detail--add-comment jira-detail--current-key)))
    (define-key map (kbd  "-")
     (lambda () "Remove comment at point"
       (interactive) (jira-detail--remove-comment-at-point)))
    (define-key map (kbd "C")
      (lambda () "Change issue status"
	(interactive) (jira-detail--change-issue-status)))
    (define-key map (kbd "O")
      (lambda () "Open issue in browser"
	(interactive)  (jira-actions-open-issue jira-detail--current-key)))
    (define-key map (kbd "U")
      (lambda () "Update issue field"
	(interactive) (jira-detail--update-field)))
    (define-key map (kbd "f")
      (lambda () "Find issue by key"
        (interactive) (jira-detail-find-issue-by-key)))
    (define-key map (kbd "c")
      (lambda () "Copy selected issue id to clipboard"
	(interactive)
	(jira-actions-copy-issues-id-to-clipboard jira-detail--current-key)))
    (define-key map (kbd "g")
      (lambda () "Refresh issue detail"
	(interactive) (jira-detail-show-issue jira-detail--current-key)))
    map)
  "Keymap for Jira Issue Detail buffers.")


(define-derived-mode jira-detail-mode magit-section-mode "Jira Detail"
  :interactive nil
  (add-hook 'jira-detail-changed-hook
            (lambda () (jira-detail-show-issue jira-detail--current-key)) nil t))

(defun jira-detail ()
  "Activate `jira-detail-mode' in the current buffer."
  (interactive)
  (kill-all-local-variables)
  (magit-section-mode)
  (jira-detail-mode))

(provide 'jira-detail)

;;; jira-detail.el ends here

;;; jira-comment.el --- Writing comments  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'rx))

(require 'jira-users)

(defvar-local jira-comment--callback nil
  "The callback function to call after adding a comment.")

(defface jira-face-deleted
  '((t (:strike-through t)))
  "Face for Jira deleted markup."
  :group 'jira)

(defface jira-face-inserted
  '((t (:underline t)))
  "Face for Jira inserted markup."
  :group 'jira)

(defvar jira-font-lock-keywords
  `((,(rx "[~" (*? (not "]")) "]")
     . 'jira-face-mention)
    (,(rx "[" (*? (not "]")) "]")
     . 'jira-face-link)
    (,(rx "{{" (*? (not "}")) "}}")
     . 'jira-face-code)
    (,(rx "`" (+? (not "`")) "`")
     . 'jira-face-code)
    (,(rx bow "*" (+? not-newline) "*" eow)
     0 'bold prepend)
    (,(rx bow "_" (+? not-newline) "_" eow)
     0 'italic prepend)
    (,(rx bow "-" (+? (not "-")) "-")
     0 'jira-face-deleted prepend)
    (,(rx "+" (+? not-newline) "+")
     0 'jira-face-inserted prepend)
    (,(rx "bq. " (+ not-newline))
     0 'jira-face-blockquote prepend)
    (,(rx ":" (+ (or lower digit "-")) ":")
     0 'jira-face-emoji-reference prepend)
    (,(rx bol (submatch (+ (or "*" "#" "-"))) " ")
     . font-lock-builtin-face)
    (,(rx bol "h1. " (*? not-newline) eol)
     . 'jira-face-h1)
    (,(rx bol "h2. " (*? not-newline) eol)
     . 'jira-face-h2)
    (,(rx bol "h3. " (*? not-newline) eol)
     . 'jira-face-h3)
    (,(rx bol "h4. " (*? not-newline) eol)
     . 'jira-face-h4)
    (,(rx bol "h5. " (*? not-newline) eol)
     . 'jira-face-h5)
    (,(rx bol "h6. " (*? not-newline) eol)
     . 'jira-face-h6)))

(defun jira-comment-insert-mention (user)
  "Insert a mention for USER."
  (interactive
   (list (completing-read "Mention user: " jira-users)))
  (let ((user-id (gethash user jira-users)))
    (insert (propertize (format "[~%s]" user)
                        'jira-mention-id user-id
                        'rear-nonsticky '(jira-mention-id)))))

(defun jira-comment--for-each-property-region (f prop)
  "Call F on every region of the current buffer where PROP is set.

F is called with three arguments: (F start end value)."
  (if-let ((p (if (get-char-property (point-min) prop)
                  (point-min)
                (next-single-property-change (point-min)
                                             prop))))
      (while (< p (point-max))
        (let ((end (next-single-property-change p
                                                prop
                                                nil
                                                (point-max)))
              (val (get-char-property p prop)))
          (funcall f p end val)
          (setq p (next-single-property-change end
                                               prop
                                               nil
                                               (point-max)))))))

(defun jira-comment-format-buffer ()
  "Convert the current buffer to Confluence Wiki Markup and return it as a string."
  ;; convert mentions into ID references
  (save-excursion
    (jira-comment--for-each-property-region
     #'(lambda (mention-start mention-end account-id)
         (delete-region mention-start mention-end)
         (goto-char mention-start)
         (insert (format "[~accountid:%s]" account-id)))
     'jira-mention-id))

  ;; convert `...` into {{...}}. Jira doesn't accept backticks, but
  ;; for compatibility the web editor does this substitution.
  (save-excursion
    (goto-char (point-min))
    (named-let search ((bracket-level 0)
                       (backtick-start nil))
      (when (re-search-forward (rx (or "`"
                                       "{{"
                                       "}}"))
                               nil
                               t)
        (pcase (char-before)
          (?`
           (cond ((> bracket-level 0)
                  (search bracket-level nil))
                 (backtick-start
                  (let ((s (buffer-substring backtick-start (1- (point)))))
                    (delete-region (1- backtick-start)
                                   (point))
                    (insert (format "{{%s}}" s))
                    (search bracket-level nil)))
                 (t
                  (search bracket-level (point)))))
          (?{
           (search (if backtick-start
                       bracket-level
                     (1+ bracket-level))
                   backtick-start))
          (?}
           (search (1- bracket-level)
                   backtick-start))))))

  (buffer-substring (point-min) (point-max)))

(define-derived-mode jira-comment-mode text-mode
  "Jira Comment"
  "Major mode for writing Jira comments."
  (setq font-lock-defaults '(jira-font-lock-keywords))
  (set-syntax-table (let ((st (make-syntax-table)))
                      (modify-syntax-entry ?+ "w" st)
                      (modify-syntax-entry ?* "w" st)
                      (modify-syntax-entry ?_ "w" st)
                      (modify-syntax-entry ?- "w" st)
                      (modify-syntax-entry ?{ "w" st)
                      (modify-syntax-entry ?} "w" st)
                      st))
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c")
                (lambda ()
                  (interactive)
                  (funcall jira-comment--callback)))
    (define-key map (kbd "C-c C-k")
                (lambda () (interactive) (kill-buffer buf)))
    (define-key map (kbd "C-c m") 'jira-comment-insert-mention)
    (set-buffer-modified-p nil)
    (use-local-map map)))

(defun jira-comment-create-editor-buffer
    (buffer-name initial-content instructions save-callback)
  "Create and display an editor buffer with INITIAL-CONTENT and a SAVE-CALLBACK."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert instructions "\n\n")
      (insert initial-content)
      (jira-comment-mode)
      (setq jira-comment--callback
            (lambda ()
              (let ((content (jira-comment-format-buffer)))
                (kill-buffer buf)
                (funcall save-callback
		         (string-trim (string-remove-prefix instructions content))))))
      (display-buffer buf)
      (select-window (get-buffer-window buf)))))

(provide 'jira-comment)

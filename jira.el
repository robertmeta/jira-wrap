;;; jira.el --- Emacs interface for Jira CLI -*- lexical-binding: t -*-

;; Author: Robert Melton
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, jira, project-management

;;; Commentary:

;; This package provides an Emacs interface to the Jira CLI tool.
;; It allows you to view, create, edit, and manage Jira issues from within Emacs.

;;; Code:

(require 'json)

(defgroup jira nil
  "Interface to Jira CLI."
  :group 'tools)

(defcustom jira-command "jira"
  "Path to the jira command-line tool."
  :type 'string
  :group 'jira)

(defcustom jira-default-project nil
  "Default Jira project to use.
If nil, uses the project from jira config."
  :type '(choice (const :tag "Use config default" nil)
                 (string :tag "Project key"))
  :group 'jira)

(defcustom jira-issue-pagesize 50
  "Number of issues to fetch at a time."
  :type 'integer
  :group 'jira)

(defvar jira-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'jira-view-issue-at-point)
    (define-key map (kbd "c") 'jira-comment-issue-at-point)
    (define-key map (kbd "e") 'jira-edit-issue-at-point)
    (define-key map (kbd "a") 'jira-assign-issue-at-point)
    (define-key map (kbd "m") 'jira-move-issue-at-point)
    (define-key map (kbd "C") 'jira-create-issue)
    (define-key map (kbd "g") 'jira-refresh)
    (define-key map (kbd "o") 'jira-open-issue-in-browser)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "s") 'jira-filter-by-status)
    (define-key map (kbd "t") 'jira-filter-by-type)
    (define-key map (kbd "A") 'jira-filter-by-assignee)
    (define-key map (kbd "P") 'jira-filter-by-priority)
    (define-key map (kbd "S") 'jira-show-sprints)
    (define-key map (kbd "M") 'jira-my-issues)
    (define-key map (kbd "W") 'jira-watching)
    map)
  "Keymap for `jira-mode'.")

(defvar-local jira-current-project nil
  "The currently displayed Jira project.")

(defvar-local jira-current-filters nil
  "Current filters applied to the issue list.")

(defvar-local jira-issues-data nil
  "Cached issues data for the current buffer.")

;;; Utility functions

(defun jira--strip-ansi-codes (string)
  "Remove ANSI color codes from STRING."
  (replace-regexp-in-string "\033\\[[0-9;]*[mGKHf]" "" string))

(defun jira--run-command (&rest args)
  "Run jira command with ARGS and return output."
  (with-temp-buffer
    (let* ((all-args (if jira-default-project
                         (append (list "--project" jira-default-project) args)
                       args))
           (exit-code (apply 'call-process jira-command nil t nil all-args)))
      (if (zerop exit-code)
          (jira--strip-ansi-codes (buffer-string))
        (error "Jira command failed: %s" (buffer-string))))))

(defun jira--run-command-interactive (&rest args)
  "Run jira command interactively with ARGS in a shell."
  (let* ((all-args (if jira-default-project
                       (append (list "--project" jira-default-project) args)
                     args))
         (cmd (mapconcat 'shell-quote-argument (cons jira-command all-args) " ")))
    (async-shell-command cmd)))

(defun jira--run-command-json (&rest args)
  "Run jira command with ARGS and return JSON output as parsed Lisp data."
  (let* ((all-args (append args '("--raw")))
         (output (apply 'jira--run-command all-args)))
    (if (string-empty-p output)
        nil
      (json-read-from-string output))))

(defun jira--parse-issue-key-at-point ()
  "Get the issue key at point."
  (get-text-property (point) 'jira-issue-key))

(defun jira--parse-issue-data-at-point ()
  "Get the full issue data at point."
  (get-text-property (point) 'jira-issue-data))

;;; Emacspeak integration

(defun jira--emacspeak-speak-line ()
  "Custom Emacspeak line speaking for jira."
  (when (and (boundp 'emacspeak-speak-mode) emacspeak-speak-mode)
    (let ((issue-data (jira--parse-issue-data-at-point)))
      (when issue-data
        (let* ((key (plist-get issue-data :key))
               (summary (plist-get issue-data :summary))
               (status (plist-get issue-data :status))
               (type (plist-get issue-data :type))
               (assignee (or (plist-get issue-data :assignee) "unassigned"))
               (priority (or (plist-get issue-data :priority) "no priority"))
               (speech-text (format "%s, %s, %s, assigned to %s, %s, %s"
                                    key
                                    summary
                                    type
                                    assignee
                                    status
                                    priority)))
          (dtk-speak speech-text))
        t))))

;;; Display functions

(defun jira--format-issue-line (issue-data)
  "Format ISSUE-DATA for display."
  (let* ((key (plist-get issue-data :key))
         (summary (plist-get issue-data :summary))
         (status (plist-get issue-data :status))
         (type (plist-get issue-data :type))
         (assignee (or (plist-get issue-data :assignee) "Unassigned"))
         (priority (or (plist-get issue-data :priority) "")))
    (format "%-12s %-10s %-12s %-15s %s"
            key
            type
            status
            assignee
            summary)))

(defun jira--insert-issue (issue-data)
  "Insert ISSUE-DATA into the buffer."
  (let ((start (point))
        (line (jira--format-issue-line issue-data)))
    (insert line)
    (put-text-property start (point) 'jira-issue-key (plist-get issue-data :key))
    (put-text-property start (point) 'jira-issue-data issue-data)
    ;; Add Emacspeak-specific spoken text
    (when (featurep 'emacspeak)
      (let* ((key (plist-get issue-data :key))
             (summary (plist-get issue-data :summary))
             (status (plist-get issue-data :status))
             (type (plist-get issue-data :type))
             (assignee (or (plist-get issue-data :assignee) "unassigned"))
             (priority (or (plist-get issue-data :priority) "no priority"))
             (spoken-text (format "%s, %s, %s, assigned to %s, %s, %s"
                                  key
                                  summary
                                  type
                                  assignee
                                  status
                                  priority)))
        (put-text-property start (point) 'emacspeak-speak spoken-text)))
    (insert "\n")))

(defun jira--parse-issue-json (json-data)
  "Parse JSON-DATA from jira issue list into structured data."
  (let (issues)
    (mapc (lambda (issue)
            (let* ((key (alist-get 'key issue))
                   (fields (alist-get 'fields issue))
                   (summary (alist-get 'summary fields))
                   (status (alist-get 'name (alist-get 'status fields)))
                   (type (alist-get 'name (alist-get 'issueType fields)))
                   (assignee-obj (alist-get 'assignee fields))
                   (assignee (if assignee-obj
                                (alist-get 'displayName assignee-obj)
                              "Unassigned"))
                   (priority-obj (alist-get 'priority fields))
                   (priority (if priority-obj
                                (alist-get 'name priority-obj)
                              "")))
              (push (list :key key
                          :summary summary
                          :status status
                          :type type
                          :assignee assignee
                          :priority priority)
                    issues)))
          json-data)
    (nreverse issues)))

(defun jira--display-issues (project filters)
  "Display issues from PROJECT with FILTERS applied."
  (message "Loading Jira issues...")
  (let* ((args (list "issue" "list"))
         (args (if filters (append args filters) args))
         (json-data (apply 'jira--run-command-json args))
         (issues (jira--parse-issue-json json-data)))
    (setq jira-issues-data issues)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Jira Issues - %s" (or project "Default Project")))
      (when filters
        (insert (format " [Filtered: %s]" (mapconcat 'identity filters " "))))
      (insert "\n\n")
      (insert "Commands: [RET] view  [c] comment  [e] edit  [a] assign  [m] move  [C] create  [g] refresh  [q] quit\n")
      (insert "Filters:  [s] status  [t] type  [A] assignee  [P] priority  [M] my issues  [W] watching  [S] sprints\n\n")
      (insert (format "%-12s %-10s %-12s %-15s %s\n" "KEY" "TYPE" "STATUS" "ASSIGNEE" "SUMMARY"))
      (insert (make-string 80 ?-) "\n")
      (if (null issues)
          (insert "No issues found.\n")
        (dolist (issue issues)
          (jira--insert-issue issue))))
    (message "Loaded %d Jira issues" (length issues))))

;;; Interactive commands

;;;###autoload
(defun jira-list-issues (&optional project)
  "List Jira issues from PROJECT."
  (interactive)
  (let ((proj (or project jira-default-project "Default")))
    (with-current-buffer (get-buffer-create (format "*Jira: %s*" proj))
      (jira-mode)
      (setq jira-current-project proj)
      (setq jira-current-filters nil)
      (jira--display-issues proj nil)
      (goto-char (point-min))
      (forward-line 6)
      (switch-to-buffer (current-buffer)))))

(defun jira-refresh ()
  "Refresh the current jira buffer."
  (interactive)
  (when jira-current-project
    (let ((line (line-number-at-pos)))
      (jira--display-issues jira-current-project jira-current-filters)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun jira-view-issue-at-point ()
  "View the full details of the issue at point."
  (interactive)
  (let ((key (jira--parse-issue-key-at-point)))
    (unless key
      (error "No issue at point"))
    (jira-view-issue key)))

(defun jira-view-issue (issue-key)
  "View details of ISSUE-KEY in a separate buffer."
  (interactive "sIssue key: ")
  (let* ((json-data (jira--run-command-json "issue" "view" issue-key))
         (fields (alist-get 'fields json-data))
         (summary (alist-get 'summary fields))
         (description (or (alist-get 'description fields) "No description"))
         (status (alist-get 'name (alist-get 'status fields)))
         (type (alist-get 'name (alist-get 'issueType fields)))
         (assignee-obj (alist-get 'assignee fields))
         (assignee (if assignee-obj
                      (alist-get 'displayName assignee-obj)
                    "Unassigned"))
         (reporter-obj (alist-get 'reporter fields))
         (reporter (if reporter-obj
                      (alist-get 'displayName reporter-obj)
                    "Unknown"))
         (priority-obj (alist-get 'priority fields))
         (priority (if priority-obj
                      (alist-get 'name priority-obj)
                    "None"))
         (created (alist-get 'created fields))
         (updated (alist-get 'updated fields)))
    (with-current-buffer (get-buffer-create (format "*Jira Issue: %s*" issue-key))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (jira-issue-view-mode)
        (setq-local jira-current-issue-key issue-key)
        (setq-local jira-current-issue-data json-data)

        ;; Header
        (insert (propertize (format "%s: %s\n" issue-key (or summary "No summary"))
                           'face 'bold 'font-lock-face 'bold))
        (insert (make-string 80 ?=) "\n\n")

        ;; Metadata
        (insert (propertize "Type:      " 'face 'bold) (or type "Unknown") "\n")
        (insert (propertize "Status:    " 'face 'bold) (or status "Unknown") "\n")
        (insert (propertize "Priority:  " 'face 'bold) (or priority "None") "\n")
        (insert (propertize "Assignee:  " 'face 'bold) (or assignee "Unassigned") "\n")
        (insert (propertize "Reporter:  " 'face 'bold) (or reporter "Unknown") "\n")
        (insert (propertize "Created:   " 'face 'bold) (or created "Unknown") "\n")
        (insert (propertize "Updated:   " 'face 'bold) (or updated "Unknown") "\n\n")

        ;; Description
        (insert (propertize "Description:\n" 'face 'bold))
        (insert (make-string 80 ?-) "\n")
        (insert description "\n\n")

        ;; Actions
        (insert (make-string 80 ?=) "\n")
        (insert (propertize "Actions:\n" 'face 'bold))
        (insert "  [e] Edit issue\n")
        (insert "  [a] Assign issue\n")
        (insert "  [m] Move/transition status\n")
        (insert "  [C] Add comment\n")
        (insert "  [o] Open in browser\n")
        (insert "  [g] Refresh\n")
        (insert "  [q] Quit\n")

        (goto-char (point-min))
        (switch-to-buffer (current-buffer))))))

(defun jira-create-issue (summary type)
  "Create a new Jira issue with SUMMARY and TYPE."
  (interactive "sSummary: \nsType (e.g., Story, Task, Bug): ")
  (jira--run-command-interactive "issue" "create" "--summary" summary "--type" type)
  (message "Creating issue in shell buffer..."))

(defun jira-edit-issue-at-point ()
  "Edit the issue at point.
Prompts for what field to edit: summary, body, assignee, or priority."
  (interactive)
  (let* ((key (jira--parse-issue-key-at-point))
         (field (completing-read "Edit field: " '("summary" "body" "assignee" "priority" "labels") nil t))
         (value (read-string (format "New %s: " field))))
    (unless key
      (error "No issue at point"))
    (pcase field
      ("summary" (jira--run-command "issue" "edit" key "--summary" value))
      ("body" (jira--run-command "issue" "edit" key "--body" value))
      ("assignee" (jira--run-command "issue" "edit" key "--assignee" value))
      ("priority" (jira--run-command "issue" "edit" key "--priority" value))
      ("labels" (jira--run-command "issue" "edit" key "--label" value)))
    (message "Updated %s for %s" field key)
    (jira-refresh)))

(defun jira-assign-issue-at-point (assignee)
  "Assign the issue at point to ASSIGNEE."
  (interactive "sAssign to (email or display name): ")
  (let ((key (jira--parse-issue-key-at-point)))
    (unless key
      (error "No issue at point"))
    (jira--run-command "issue" "assign" key assignee)
    (message "Assigned %s to %s" key assignee)
    (jira-refresh)))

(defun jira-move-issue-at-point (status)
  "Move the issue at point to STATUS."
  (interactive
   (list (completing-read "Move to status: "
                          '("To Do" "In Progress" "In Review" "Done" "Blocked" "Backlog")
                          nil nil)))
  (let ((key (jira--parse-issue-key-at-point)))
    (unless key
      (error "No issue at point"))
    (jira--run-command "issue" "move" key status)
    (message "Moved %s to %s" key status)
    (jira-refresh)))

(defun jira-comment-issue-at-point (comment)
  "Add a comment to the issue at point."
  (interactive "sComment: ")
  (let ((key (jira--parse-issue-key-at-point)))
    (unless key
      (error "No issue at point"))
    (when (string-empty-p comment)
      (error "Comment cannot be empty"))
    (jira--run-command "issue" "comment" "add" key comment)
    (message "Added comment to %s" key)))

(defun jira-open-issue-in-browser ()
  "Open the issue at point in a web browser."
  (interactive)
  (let ((key (jira--parse-issue-key-at-point)))
    (unless key
      (error "No issue at point"))
    (jira--run-command "open" key)
    (message "Opening %s in browser..." key)))

(defun jira-filter-by-status (status)
  "Filter issues by STATUS."
  (interactive
   (list (completing-read "Filter by status: "
                          '("To Do" "In Progress" "In Review" "Done" "Blocked" "Backlog" "Open" "Closed" "Reopened")
                          nil nil)))
  (setq jira-current-filters (list "--status" status))
  (jira-refresh))

(defun jira-filter-by-type (type)
  "Filter issues by TYPE."
  (interactive
   (list (completing-read "Filter by type: "
                          '("Story" "Task" "Bug" "Epic" "Subtask" "Spike" "Improvement")
                          nil nil)))
  (setq jira-current-filters (list "--type" type))
  (jira-refresh))

(defun jira-filter-by-assignee (assignee)
  "Filter issues by ASSIGNEE."
  (interactive "sFilter by assignee: ")
  (setq jira-current-filters (list "--assignee" assignee))
  (jira-refresh))

(defun jira-filter-by-priority (priority)
  "Filter issues by PRIORITY."
  (interactive
   (list (completing-read "Filter by priority: "
                          '("Highest" "High" "Medium" "Low" "Lowest")
                          nil nil)))
  (setq jira-current-filters (list "--priority" priority))
  (jira-refresh))

(defun jira-my-issues ()
  "Show issues assigned to me, excluding Done and Cancelled issues."
  (interactive)
  (let* ((me (string-trim (jira--run-command "me")))
         (proj (or jira-default-project "Default")))
    (with-current-buffer (get-buffer-create (format "*Jira: %s (My Issues)*" proj))
      (jira-mode)
      (setq jira-current-project proj)
      (setq jira-current-filters (list "--assignee" me "--status" "~Done" "--status" "~Cancelled" "--status" "~Canceled"))
      (jira--display-issues proj jira-current-filters)
      (goto-char (point-min))
      (forward-line 6)
      (switch-to-buffer (current-buffer)))))

(defun jira-watching ()
  "Show issues I'm watching."
  (interactive)
  (setq jira-current-filters (list "--watching"))
  (jira-refresh))

(defun jira-show-sprints ()
  "Show active sprints."
  (interactive)
  (let ((output (jira--run-command "sprint" "list" "--state" "active")))
    (with-current-buffer (get-buffer-create "*Jira Sprints*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (goto-char (point-min))
        (view-mode)
        (switch-to-buffer (current-buffer))))))

;;; Issue view mode

(defvar-local jira-current-issue-key nil
  "The issue key being viewed in the current buffer.")

(defvar-local jira-current-issue-data nil
  "The full JSON data for the issue being viewed.")

(defvar jira-issue-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'jira-issue-view-edit)
    (define-key map (kbd "a") 'jira-issue-view-assign)
    (define-key map (kbd "m") 'jira-issue-view-move)
    (define-key map (kbd "C") 'jira-issue-view-comment)
    (define-key map (kbd "o") 'jira-issue-view-open-browser)
    (define-key map (kbd "g") 'jira-issue-view-refresh)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `jira-issue-view-mode'.")

(define-derived-mode jira-issue-view-mode special-mode "Jira-Issue"
  "Major mode for viewing a single Jira issue.

\\{jira-issue-view-mode-map}"
  (setq buffer-read-only t))

(defun jira-issue-view-edit ()
  "Edit the current issue."
  (interactive)
  (when jira-current-issue-key
    (jira--run-command "issue" "edit" jira-current-issue-key)
    (message "Editing %s..." jira-current-issue-key)
    (jira-issue-view-refresh)))

(defun jira-issue-view-assign ()
  "Assign the current issue to someone."
  (interactive)
  (when jira-current-issue-key
    (let ((assignee (read-string "Assign to (email or name): ")))
      (jira--run-command "issue" "assign" jira-current-issue-key assignee)
      (message "Assigned %s to %s" jira-current-issue-key assignee)
      (jira-issue-view-refresh))))

(defun jira-issue-view-move ()
  "Move/transition the current issue to a new status."
  (interactive)
  (when jira-current-issue-key
    (let ((status (read-string "Move to status: ")))
      (jira--run-command "issue" "move" jira-current-issue-key status)
      (message "Moved %s to %s" jira-current-issue-key status)
      (jira-issue-view-refresh))))

(defun jira-issue-view-comment ()
  "Add a comment to the current issue."
  (interactive)
  (when jira-current-issue-key
    (jira--run-command "issue" "comment" "add" jira-current-issue-key)
    (message "Adding comment to %s..." jira-current-issue-key)))

(defun jira-issue-view-open-browser ()
  "Open the current issue in a web browser."
  (interactive)
  (when jira-current-issue-key
    (jira--run-command "open" jira-current-issue-key)
    (message "Opening %s in browser..." jira-current-issue-key)))

(defun jira-issue-view-refresh ()
  "Refresh the current issue view."
  (interactive)
  (when jira-current-issue-key
    (jira-view-issue jira-current-issue-key)))

;;; Major mode

(define-derived-mode jira-mode special-mode "Jira"
  "Major mode for viewing and managing Jira issues.

\\{jira-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  ;; Set up Emacspeak integration
  (when (featurep 'emacspeak)
    (add-hook 'post-command-hook 'jira--emacspeak-post-command nil t)))

(defun jira--emacspeak-post-command ()
  "Emacspeak post-command hook for jira mode."
  (when (and (featurep 'emacspeak)
             (eq major-mode 'jira-mode)
             (memq this-command '(next-line previous-line)))
    (jira--emacspeak-speak-line)))

;;; Evil mode integration

(with-eval-after-load 'evil
  (evil-set-initial-state 'jira-mode 'emacs))

;;; Emacspeak advice

(with-eval-after-load 'emacspeak
  ;; Override emacspeak-speak-line for jira-mode
  (defadvice emacspeak-speak-line (around jira-mode activate)
    "Use custom line speaking in jira-mode."
    (if (and (eq major-mode 'jira-mode)
             (get-text-property (point) 'emacspeak-speak))
        (dtk-speak (get-text-property (point) 'emacspeak-speak))
      ad-do-it))

  (defadvice jira-list-issues (after emacspeak activate)
    "Provide auditory feedback when opening issues."
    (when (ems-interactive-p)
      (emacspeak-icon 'open-object)
      (dtk-speak "Showing Jira issues")))

  (defadvice jira-create-issue (after emacspeak activate)
    "Provide auditory feedback when creating an issue."
    (when (ems-interactive-p)
      (emacspeak-icon 'item)))

  (defadvice jira-move-issue-at-point (after emacspeak activate)
    "Provide auditory feedback when moving an issue."
    (when (ems-interactive-p)
      (emacspeak-icon 'select-object))))

(provide 'jira)

;;; jira.el ends here

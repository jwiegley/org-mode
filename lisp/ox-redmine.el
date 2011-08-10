;;; ox-redmine.el --- Org-X backend for Org-mode data

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: John Wiegley
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 7.7

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'org-x)

(eval-when-compile
  (require 'cl))

(defgroup org-x-redmine nil
  "Org-X backend for Redmine."
  :tag "Org-X Redmine"
  :group 'org-x)

;;; Customization variables:

(defcustom org-x-redmine-dispatchers
  '((read-entry		. org-x-redmine-fetch)
    (write-entry	. org-x-redmine-push)
    (add-log-entry	. org-x-redmine-add-log-entry)
    (apply-changes	. ignore)	; jww (2011-08-08): NYI
    (applicable-backend . org-x-redmine-applicable-backend)
    (get-identifier	. org-x-redmine-get-identifier)
    (group-identifiers	. org-x-redmine-group-identifiers))
  "An Org-X backend for Redmine."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-x-redmine)

(defvar ox-redmine 'org-x-redmine-dispatchers)

(defcustom org-x-redmine-trackers '(("Support" . 3)
				    ("Feature" . 2)
				    ("Bug"     . 1))
  "An alist of all the trackers on the Redmine installation."
  :type '(alist :key-type string :value-type integer)
  :group 'org-x-redmine)

(defcustom org-x-redmine-priorities '(("Immediate" . 1)
				      ("Urgent"    . 1)
				      ("High"      . 1)
				      ("Normal"    . 2)
				      ("Low"       . 3))
  "An alist of all the priorities on the Redmine installation."
  :type '(alist :key-type string :value-type integer)
  :group 'org-x-redmine)

(defcustom org-x-redmine-statuses '(("TODO"     . ("New"         . 1))
				    ("STARTED"  . ("In Progress" . 2))
				    ("DONE"     . ("Resolved"    . 3))
				    ("WAITING"  . ("Feedback"    . 4))
				    ("DONE"     . ("Closed"      . 5))
				    ("CANCELED" . ("Rejected"    . 6)))
  "An alist of all the statuses on the Redmine installation.
These are keyed by the related Org mode state."
  :type '(alist :key-type string :value-type integer)
  :group 'org-x-redmine)

(defcustom org-x-redmine-title-prefix-function nil
  "If non-nil, a function returning a string maintained in Org titles.
This function takes a numerical identifier, and must return either nil
or a string.

Using the builtin prefix style [[redmine:<ID>]] requires that the
following be placed in your Org file:
  #+LINK: redmine <Redmine_URL>"
  :type '(choice (const :tag "Don't use title prefixes" nil)
		 (const :tag "Use a link to [[redmine:<ID>]]"
			org-x-redmine-title-prefix)
		 (function :tag "Use a custom function"))
  :group 'org-x-redmine)

(defcustom org-x-redmine-title-prefix-match-function nil 
  "If non-nil, a function matching Redmine identifiers in Org titles.
The function takes title string, and must return either nil or an
integer.
See `org-x-redmine-title-prefix-function'."
  :type '(choice (const :tag "Don't use title prefixes" nil)
		 (const :tag "Match links of the form [[redmine:<ID>]]"
			org-x-redmine-title-prefix-match)
		 (function :tag "Match using a custom function"))
  :group 'org-x-redmine)

;;; Redmine contextual info:

(defsubst org-x-redmine-property (info name)
  (cond
   ((or (numberp info) (markerp info))
    (org-entry-get info (concat "Redmine_" name) t))
   ((assq 'entry info)
    (org-x-get-property info (concat "Redmine_" name) t))
   (t
    (cdr (assoc (concat "Redmine_" name) info)))))

(defun org-x-redmine-applicable-backend (entry-or-pos)
  (cons 'ox-redmine
	(list (cons "Redmine_URL"
		    (org-x-redmine-property entry-or-pos "URL"))
	      (cons "Redmine_APIKey"
		    (org-x-redmine-property entry-or-pos "APIKey"))
	      (cons "Redmine_Project"
		    (org-x-redmine-property entry-or-pos "Project")))))

(defun org-x-redmine-title-prefix (id)
  (format "[[redmine:%d][#%d]] " id id))

(defun org-x-redmine-title-prefix-match (title)
  (and (string-match "\\[\\[redmine:\\([0-9]+\\)\\]\\[#" title)
       (string-to-number (match-string 1 title))))

(defun org-x-redmine-get-identifier (entry)
  (let ((issue-id (org-x-redmine-property entry "Id")))
    (if issue-id
	(setq issue-id (string-to-number issue-id))
      (if org-x-redmine-title-prefix-match-function
	  (setq issue-id
		(funcall org-x-redmine-title-prefix-match-function
			 (org-x-title entry)))))
    (org-x-set-property entry "Redmine_Id" issue-id t)
    issue-id))

(defun org-x-redmine-group-identifiers ()
  (save-excursion
    (outline-up-heading 1)
    (outline-next-heading)
    (let* ((depth (org-x--heading-depth))
	   (current-depth depth)
	   identifiers)
      (while (= depth current-depth)
	(setq identifiers
	      (cons (if (featurep 'org-id)
			(org-id-get-create)
		      (point-marker))
		    identifiers))
	(org-forward-same-level 1 t)
	(setq depth (org-x--heading-depth)))
      (nreverse identifiers))))

;;; Redmine REST API:

(defvar org-x-redmine-debug t)

(defun org-x-redmine-rest-api
  (type root-url url api-key &optional input params)
  (with-temp-buffer
    (if input (insert input))
    (let ((command
	   (format (concat
		    "curl -s -k -X %s %s "
		    "-H 'Content-type: text/xml' -H 'Accept: text/xml' "
		    "'%s/%s?%s%sformat=xml&key=%s'")
		   type (if (string= type "GET") "" "-d @-")
		   root-url url (or params "") (if params "&" "")
		   api-key)))
      (if org-x-redmine-debug
	  (message "Invoking: %s" command))
      (shell-command-on-region (point-min) (point-max) command nil t))
    (goto-char (point-min))
    (skip-syntax-forward " ")
    (unless (eobp)
      (nthcdr 2 (car (xml-parse-region (point-min) (point-max)))))))

;;; Reading Redmine issues:

(defun org-x-redmine-convert-timestamp (stamp &optional with-hm inactive)
  (when (string-match (concat "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)"
                              "T\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)-.+")
                      stamp)
    (let ((year (string-to-number (match-string 1 stamp)))
          (mon  (string-to-number (match-string 2 stamp)))
          (day  (string-to-number (match-string 3 stamp)))
          (hour (string-to-number (match-string 4 stamp)))
          (min  (string-to-number (match-string 5 stamp)))
          ;;(sec  (string-to-number (match-string 6 stamp)))
	  )
      (encode-time 0 min hour day mon year))))

(defun org-x-redmine-parse-entry (data)
  (let ((entry (org-x-create-entry)) id)
    (dolist (elem data)
      (cond
       ((eq 'id (car elem))
	(org-x-set-property entry "Redmine_Id" (nth 2 elem)))

       ((eq 'subject (car elem))
	(org-x-set-title entry (nth 2 elem)))

       ((eq 'description (car elem))
	(org-x-set-body entry (nth 2 elem)))

       ((eq 'status (car elem))
	(let ((stat (cdr (assq 'name (cadr elem)))))
          (dolist (status org-x-redmine-statuses)
            (if (string= stat (cadr status))
                (setq stat (car status))))
	  (org-x-set-state entry stat)))

       ((eq 'priority (car elem))
	(org-x-set-priority entry
			    (cdr (assoc (cdr (assq 'name (cadr elem)))
					org-x-redmine-priorities))))

       ((eq 'created_on (car elem))
	(org-x-set-property
	 entry "CREATED"
	 (org-x-redmine-convert-timestamp (nth 2 elem) t t)))

       ((eq 'journals (car elem))
        (dolist (journal (nthcdr 2 elem))
          (let* ((body (nth 2 (assq 'notes journal)))
                 (timestamp (org-x-redmine-convert-timestamp
			     (nth 2 (assq 'created_on journal)) t))
		 (name (cdr (assq 'name (cadr (assq 'user journal))))))
	    ;; jww (2011-08-04): Distinguish notes from state changes
	    (if (and body (> (length body) 1))
		(org-x-add-log-entry entry timestamp
				     (if (equal user-full-name name)
					 body
				       (concat name ": " body)) t)))))))
    entry))

(defun org-x-redmine-fetch (issue-id)
  (org-x-redmine-parse-entry
   (org-x-redmine-rest-api
    "GET" (org-x-redmine-property org-x-dispatch-context "URL")
    (format "issues/%d.xml" issue-id)
    (org-x-redmine-property org-x-dispatch-context "APIKey")
    nil "include=journals")))

(defun org-x-redmine-fetch-issue-ids (project-id)
  (org-x-redmine-rest-api
   "GET" (org-x-redmine-property org-x-dispatch-context "URL")
   (format "issues.xml?project_id=%s&assigned_to=me" project-id)
   (org-x-redmine-property org-x-dispatch-context "APIKey")))

(defun show-issue-ids ()
  (interactive)
  (let ((backend (org-x-applicable-backend 'ox-redmine (point))))
    (with-org-x-context (cdr backend)
      (message "%s" (pp-to-string (org-x-redmine-fetch-issue-ids "it"))))))

;;; Writing Redmine issues:

(defun org-x-redmine-push (entry)
  (let* ((issue-id (org-x-redmine-get-identifier entry))
	 (root-url (org-x-redmine-property entry "URL"))
	 (api-key (org-x-redmine-property entry "APIKey"))
	 (project (org-x-redmine-property entry "Project"))
	 (subject
	  (replace-regexp-in-string "\\[\\[redmine:.+?\\]\\[.+?\\]\\]\\s-*" ""
				    (org-x-title entry)))
	 (result
          (org-x-redmine-rest-api
           (if issue-id "PUT" "POST")
	   root-url (if issue-id
			(format "issues/%d.xml"
				(string-to-number issue-id))
		      "issues.xml")
	   api-key
	   (format "<?xml version=\"1.0\"?>
<issue>
  <project_id>%s</project_id>
  <subject>%s</subject>
  <tracker_id>%d</tracker_id>
  %s,
  <priority_id>%d</priority_id>
  %s
</issue>"
		   project
		   (xml-escape-string subject)
		   (cdr (assoc
			 (or (org-x-redmine-property entry "Tracker") "Bug")
			 org-x-redmine-trackers))
		   (let ((user-id (org-x-redmine-property entry "UserId")))
		     (if user-id
			 (concat "<assigned_to_id>" user-id
				 "</assigned_to_id>")
		       ""))
		   (or (org-x-priority entry) 2)
		   (if (org-x-body entry)
		       (concat "<description>"
			       (xml-escape-string (org-x-body entry))
			       "</description>")
		     ""))))
         (id (nth 2 (assq 'id result))))
    (if id
        (org-x-set-property entry "Redmine_Id" (string-to-number id))
      (error "Failed to push Org-X entry to Redmine project %s" project))))

(defun org-x-redmine-add-log-entry (entry timestamp body is-note
					  to-state from-state)
  ;; jww (2011-08-05): Need to propagate the timestamp somehow, and
  ;; any state changes.
  (let ((issue-id (org-x-redmine-get-identifier entry))
	(root-url (org-x-redmine-property entry "URL"))
	(api-key (org-x-redmine-property entry "APIKey")))
    (org-x-redmine-rest-api
     "PUT" root-url (format "issues/%d.xml" issue-id) api-key
     (format "<?xml version=\"1.0\"?>
<issue>
  <notes>%s</notes>
</issue>" (xml-escape-string body)))))

(defun org-x-redmine-set-state (entry state note)
  (let ((issue-id (org-x-redmine-get-identifier entry))
	(root-url (org-x-redmine-property entry "URL"))
	(api-key (org-x-redmine-property entry "APIKey")))
    (org-x-redmine-rest-api
     "PUT" root-url (format "issues/%d.xml" issue-id) api-key
     (format "<?xml version=\"1.0\"?>
<issue>
  <status_id>%s</status_id>
</issue>"
	     (cddr (assoc state org-x-redmine-statuses))))))


(provide 'ox-redmine)

;; arch-tag:

;;; ox-redmine.el ends here

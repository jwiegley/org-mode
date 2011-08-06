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

(defcustom org-x-redmine-dispatchers
  '((read-entry	   . org-x-redmine-fetch)
    (write-entry   . org-x-redmine-push)
    (add-log-entry . org-x-redmine-add-log-entry))
  "An Org-X backend for Redmine."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-x-redmine)

(add-to-list 'org-x-backends (cons 'redmine 'org-x-redmine-backend))

(defcustom org-x-redmine-trackers '(("Support" . 3)
				    ("Feature" . 2)
				    ("Bug"     . 1))
  "An alist of all the trackers on the Redmine installation."
  :type '(alist :key-type string :value-type integer)
  :group 'org-x-redmine)

(defcustom org-x-redmine-priorities '(("Immediate" . 1)
				      ("Urgent"    . 2)
				      ("High"      . 3)
				      ("Normal"    . 4)
				      ("Low"       . 5))
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

(defun org-x-redmine-rest-api
  (type root-url url api-key &optional input params)
  (with-temp-buffer
    (if input (insert input))
    (shell-command-on-region
     (point-min) (point-max)
     (format (concat
              "curl -s -k -X %s %s "
              "-H 'Content-type: text/xml' -H 'Accept: text/xml' "
              "'%s/%s?%s%sformat=xml&key=%s'")
             type (if (string= type "GET") "" "-d @-")
             root-url url (or params "") (if params "&" "") api-key)
     nil t)
    (goto-char (point-min))
    (skip-syntax-forward " ")
    (unless (eobp)
      (nthcdr 2 (car (xml-parse-region (point-min) (point-max)))))))

(defun org-x-redmine-convert-timestamp (stamp &optional with-hm inactive)
  (when (string-match (concat "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)"
                              "T\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)-.+")
                      stamp)
    (let ((year (string-to-number (match-string 1 stamp)))
          (mon  (string-to-number (match-string 2 stamp)))
          (day  (string-to-number (match-string 3 stamp)))
          (hour (string-to-number (match-string 4 stamp)))
          (min  (string-to-number (match-string 5 stamp)))
          (sec  (string-to-number (match-string 6 stamp))))
      (encode-time sec min hour day mon year))))

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
	(org-x-set-priority
	 entry (let ((pri (cdr (assq 'name (cadr elem)))))
		 (cond
		  ((string-match "\\(High\\|Urgent\\|Immediate\\)" pri) 1)
		  ((string= "Normal" pri) 2)
		  ((string= "Low" pri) 3)))))

       ((eq 'created_on (car elem))
	(org-x-set-property
	 entry "CREATED"
	 (org-x-redmine-convert-timestamp (nth 2 elem) t t)))

       ((eq 'journals (car elem))
        (dolist (journal (nthcdr 2 elem))
          (let* ((body (nth 2 (assq 'notes journal)))
                 (timestamp (org-x-redmine-convert-timestamp
			     (nth 2 (assq 'created_on journal)) t)))
	    ;; jww (2011-08-04): Distinguish notes from state changes
	    (org-x-add-log-entry entry timestamp body t))))))
    entry))

(defsubst org-x-redmine-property (entry name)
  (let ((property-name (concat "Redmine_" name)))
    (org-x-get-property entry property-name t)))

(defun org-x-redmine-fetch (issue-id context)
  (let ((root-url (org-x-redmine-property context "URL"))
	(api-key (org-x-redmine-property context "APIKey")))
    (org-x-redmine-parse-entry
     (org-x-redmine-rest-api "GET" root-url (format "issues/%d.xml" issue-id)
			     api-key nil "include=journals"))))

(defun org-x-redmine-push (entry)
  (let* ((issue-id (org-x-redmine-get-identifier entry))
	 (root-url (org-x-redmine-property entry "URL"))
	 (api-key (org-x-redmine-property entry "APIKey"))
	 (project (org-x-redmine-property entry "Project"))
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
  %s
  <priority_id>%d</priority_id>
  %s
</issue>"
		   project
		   (xml-escape-string (org-x-title entry))
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

(defsubst org-x-redmine-get-identifier (entry)
  (let ((id (org-x-get-property entry "Redmine_Id")))
    (and id (string-to-number id))))

(defun org-x-redmine-add-log-entry (entry timestamp body is-note
					  to-state from-state)
  ;; jww (2011-08-05): Need to propagate the timestamp somehow, and
  ;; any state changes.
  (org-x-redmine-rest-api
   "PUT" (format "issues/%d.xml" (org-x-redmine-get-identifier entry))
   (format "<?xml version=\"1.0\"?>
<issue>
  <notes>%s</notes>
</issue>" (xml-escape-string body))))

(defun org-x-redmine-set-state (entry state note)
  (org-x-redmine-rest-api
   "PUT" (format "issues/%d.xml" (org-x-redmine-get-identifier entry))
   (format "<?xml version=\"1.0\"?>
<issue>
  <status_id>%s</status_id>
</issue>"
           (cddr (assoc state org-redmine-statuses)))))


(provide 'ox-redmine)

;; arch-tag:

;;; ox-redmine.el ends here

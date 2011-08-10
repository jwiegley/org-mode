;;; ox-bugzilla.el --- Org-X backend for Org-mode data

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

(defgroup org-x-bugzilla nil
  "Org-X backend for Bugzilla."
  :tag "Org-X Bugzilla"
  :group 'org-x)

;;; Customization variables:

(defcustom org-x-bugzilla-dispatchers
  '((read-entry		. org-x-bugzilla-fetch)
    (write-entry	. org-x-bugzilla-push)
    (add-log-entry	. org-x-bugzilla-add-log-entry)
    (apply-changes	. ignore)	; jww (2011-08-08): NYI
    (applicable-backend . org-x-bugzilla-applicable-backend)
    (get-identifier	. org-x-bugzilla-get-identifier)
    (group-identifiers	. org-x-bugzilla-group-identifiers))
  "An Org-X backend for Bugzilla."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-x-bugzilla)

(defvar ox-bugzilla 'org-x-bugzilla-dispatchers)

(defcustom org-x-bugzilla-priorities '(("P1" . 1)
				       ("P2" . 2)
				       ("P3" . 3)
				       ("P4" . 4)
				       ("P5" . 5))
  "An alist of all the priorities on the Bugzilla installation."
  :type '(alist :key-type string :value-type integer)
  :group 'org-x-bugzilla)

(defcustom org-x-bugzilla-statuses '(("TODO"     . ("New"         . 1))
				     ("STARTED"  . ("In Progress" . 2))
				     ("DONE"     . ("Resolved"    . 3))
				     ("WAITING"  . ("Feedback"    . 4))
				     ("DONE"     . ("Closed"      . 5))
				     ("CANCELED" . ("Rejected"    . 6)))
  "An alist of all the statuses on the Bugzilla installation.
These are keyed by the related Org mode state."
  :type '(alist :key-type string :value-type integer)
  :group 'org-x-bugzilla)

(defcustom org-x-bugzilla-title-prefix-function nil
  "If non-nil, a function returning a string maintained in Org titles.
This function takes a numerical identifier, and must return either nil
or a string.

Using the builtin prefix style [[bugzilla:<ID>]] requires that the
following be placed in your Org file:
  #+LINK: bugzilla <Bugzilla_URL>"
  :type '(choice (const :tag "Don't use title prefixes" nil)
		 (const :tag "Use a link to [[bugzilla:<ID>]]"
			org-x-bugzilla-title-prefix)
		 (function :tag "Use a custom function"))
  :group 'org-x-bugzilla)

(defcustom org-x-bugzilla-title-prefix-match-function nil 
  "If non-nil, a function matching Bugzilla identifiers in Org titles.
The function takes title string, and must return either nil or an
integer.
See `org-x-bugzilla-title-prefix-function'."
  :type '(choice (const :tag "Don't use title prefixes" nil)
		 (const :tag "Match links of the form [[bugzilla:<ID>]]"
			org-x-bugzilla-title-prefix-match)
		 (function :tag "Match using a custom function"))
  :group 'org-x-bugzilla)

;;; Bugzilla contextual info:

(defsubst org-x-bugzilla-property (info name)
  (cond
   ((or (numberp info) (markerp info))
    (org-entry-get info (concat "Bugzilla_" name) t))
   ((assq 'entry info)
    (org-x-get-property info (concat "Bugzilla_" name) t))
   (t
    (cdr (assoc (concat "Bugzilla_" name) info)))))

(defun org-x-bugzilla-applicable-backend (entry-or-pos)
  (cons 'ox-bugzilla
	(list (cons "Bugzilla_URL"
		    (org-x-bugzilla-property entry-or-pos "URL"))
	      (cons "Bugzilla_APIKey"
		    (org-x-bugzilla-property entry-or-pos "APIKey"))
	      (cons "Bugzilla_Project"
		    (org-x-bugzilla-property entry-or-pos "Project")))))

(defun org-x-bugzilla-title-prefix (id)
  (format "[[bugzilla:%d][#%d]] " id id))

(defun org-x-bugzilla-title-prefix-match (title)
  (and (string-match "\\[\\[bugzilla:\\([0-9]+\\)\\]\\[#" title)
       (string-to-number (match-string 1 title))))

(defun org-x-bugzilla-get-identifier (entry)
  (let ((issue-id (org-x-bugzilla-property entry "Id")))
    (if issue-id
	(setq issue-id (string-to-number issue-id))
      (if org-x-bugzilla-title-prefix-match-function
	  (setq issue-id
		(funcall org-x-bugzilla-title-prefix-match-function
			 (org-x-title entry)))))
    (org-x-set-property entry "Bugzilla_Id" issue-id t)
    issue-id))

(defun org-x-bugzilla-group-identifiers ()
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

;;; Bugzilla REST API:

(defvar org-x-bugzilla-debug t)

(defun org-x-bugzilla-rest-api
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
      (if org-x-bugzilla-debug
	  (message "Invoking: %s" command))
      (shell-command-on-region (point-min) (point-max) command nil t))
    (goto-char (point-min))
    (skip-syntax-forward " ")
    (unless (eobp)
      (nthcdr 2 (car (xml-parse-region (point-min) (point-max)))))))

;;; Reading Bugzilla issues:

(defun org-x-bugzilla-convert-timestamp (stamp &optional with-hm inactive)
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

(defun org-x-bugzilla-parse-entry (data)
  (let ((entry (org-x-create-entry)) id)
    (dolist (elem data)
      (cond
       ((eq 'id (car elem))
	(org-x-set-property entry "Bugzilla_Id" (nth 2 elem)))

       ((eq 'subject (car elem))
	(org-x-set-title entry (nth 2 elem)))

       ((eq 'description (car elem))
	(org-x-set-body entry (nth 2 elem)))

       ((eq 'status (car elem))
	(let ((stat (cdr (assq 'name (cadr elem)))))
          (dolist (status org-x-bugzilla-statuses)
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
	 (org-x-bugzilla-convert-timestamp (nth 2 elem) t t)))

       ((eq 'journals (car elem))
        (dolist (journal (nthcdr 2 elem))
          (let* ((body (nth 2 (assq 'notes journal)))
                 (timestamp (org-x-bugzilla-convert-timestamp
			     (nth 2 (assq 'created_on journal)) t))
		 (name (cdr (assq 'name (cadr (assq 'user journal))))))
	    ;; jww (2011-08-04): Distinguish notes from state changes
	    (if (and body (> (length body) 1))
		(org-x-add-log-entry entry timestamp
				     (if (equal user-full-name name)
					 body
				       (concat name ": " body)) t)))))))
    entry))

(defun org-x-bugzilla-fetch (issue-id)
  (org-x-bugzilla-parse-entry
   (org-x-bugzilla-rest-api
    "GET" (org-x-bugzilla-property org-x-dispatch-context "URL")
    (format "issues/%d.xml" issue-id)
    (org-x-bugzilla-property org-x-dispatch-context "APIKey")
    nil "include=journals")))

(defun org-x-bugzilla-fetch-issue-ids (project-id)
  (org-x-bugzilla-rest-api
   "GET" (org-x-bugzilla-property org-x-dispatch-context "URL")
   (format "issues.xml?project_id=%s&assigned_to=me" project-id)
   (org-x-bugzilla-property org-x-dispatch-context "APIKey")))

(defun show-issue-ids ()
  (interactive)
  (let ((backend (org-x-applicable-backend 'ox-bugzilla (point))))
    (with-org-x-context (cdr backend)
      (message "%s" (pp-to-string (org-x-bugzilla-fetch-issue-ids "it"))))))

;;; Writing Bugzilla issues:

(defun org-x-bugzilla-push (entry)
  (let* ((issue-id (org-x-bugzilla-get-identifier entry))
	 (root-url (org-x-bugzilla-property entry "URL"))
	 (api-key (org-x-bugzilla-property entry "APIKey"))
	 (project (org-x-bugzilla-property entry "Project"))
	 (subject
	  (replace-regexp-in-string "\\[\\[bugzilla:.+?\\]\\[.+?\\]\\]\\s-*" ""
				    (org-x-title entry)))
	 (result
          (org-x-bugzilla-rest-api
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
			 (or (org-x-bugzilla-property entry "Tracker") "Bug")
			 org-x-bugzilla-trackers))
		   (let ((user-id (org-x-bugzilla-property entry "UserId")))
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
        (org-x-set-property entry "Bugzilla_Id" (string-to-number id))
      (error "Failed to push Org-X entry to Bugzilla project %s" project))))

(defun org-x-bugzilla-add-log-entry (entry timestamp body is-note
					  to-state from-state)
  ;; jww (2011-08-05): Need to propagate the timestamp somehow, and
  ;; any state changes.
  (let ((issue-id (org-x-bugzilla-get-identifier entry))
	(root-url (org-x-bugzilla-property entry "URL"))
	(api-key (org-x-bugzilla-property entry "APIKey")))
    (org-x-bugzilla-rest-api
     "PUT" root-url (format "issues/%d.xml" issue-id) api-key
     (format "<?xml version=\"1.0\"?>
<issue>
  <notes>%s</notes>
</issue>" (xml-escape-string body)))))

(defun org-x-bugzilla-set-state (entry state note)
  (let ((issue-id (org-x-bugzilla-get-identifier entry))
	(root-url (org-x-bugzilla-property entry "URL"))
	(api-key (org-x-bugzilla-property entry "APIKey")))
    (org-x-bugzilla-rest-api
     "PUT" root-url (format "issues/%d.xml" issue-id) api-key
     (format "<?xml version=\"1.0\"?>
<issue>
  <status_id>%s</status_id>
</issue>"
	     (cddr (assoc state org-x-bugzilla-statuses))))))


(provide 'ox-bugzilla)

;; arch-tag:

(defun make-ledger-bugzilla-bug (product component version priority severity)
  (interactive
   (let ((omk (get-text-property (point) 'org-marker)))
     (with-current-buffer (marker-buffer omk)
       (save-excursion
	 (goto-char omk)
	 (let ((components
		(list "data" "doc" "expr" "lisp" "math" "python" "report"
		      "test" "util" "website" "build" "misc"))
	       (priorities (list "P1" "P2" "P3" "P4" "P5"))
	       (severities (list "blocker" "critical" "major"
				 "normal" "minor" "trivial" "enhancement"))
	       (product "Ledger")
	       (version "3.0.0-20100623"))
	   (list product
		 (ido-completing-read "Component: " components
				      nil t nil nil (car (last components)))
		 version
		 (let ((orgpri (nth 3 (org-heading-components))))
		   (if (and orgpri (= ?A orgpri))
		       "P1"
		     (ido-completing-read "Priority: " priorities
					  nil t nil nil "P3")))
		 (ido-completing-read "Severity: " severities nil t nil nil
				      "normal") ))))))
  (let ((omk (get-text-property (point) 'org-marker)))
    (with-current-buffer (marker-buffer omk)
      (save-excursion
	(goto-char omk)
	(let ((heading (nth 4 (org-heading-components)))
	      (contents (buffer-substring-no-properties
			 (org-entry-beginning-position)
			 (org-entry-end-position)))
	      bug)
	  (with-temp-buffer
	    (insert contents)
	    (goto-char (point-min))
	    (delete-region (point) (1+ (line-end-position)))
	    (search-forward ":PROP")
	    (delete-region (match-beginning 0) (point-max))
	    (goto-char (point-min))
	    (while (re-search-forward "^   " nil t)
	      (delete-region (match-beginning 0) (match-end 0)))
	    (goto-char (point-min))
	    (while (re-search-forward "^SCHE" nil t)
	      (delete-region (match-beginning 0) (1+ (line-end-position))))
	    (goto-char (point-min))
	    (when (eobp)
	      (insert "No description.")
	      (goto-char (point-min)))
	    (insert (format "Product: %s
Component: %s
Version: %s
Priority: %s
Severity: %s
Hardware: Other
OS: Other
Summary: %s" product component version priority severity heading) ?\n ?\n)
	    (let ((buf (current-buffer)))
	      (with-temp-buffer
		(let ((tmpbuf (current-buffer)))
		  (if nil
		      (insert "Bug 999 posted.")
		    (with-current-buffer buf
		      (shell-command-on-region
		       (point-min) (point-max)
		       "~/bin/bugzilla-submit http://newartisans.com/bugzilla/"
		       tmpbuf)))
		  (goto-char (point-min))
		  (re-search-forward "Bug \\([0-9]+\\) posted.")
		  (setq bug (match-string 1))))))
	  (save-excursion
	    (org-back-to-heading t)
	    (re-search-forward "\\(TODO\\|DEFERRED\\|STARTED\\|WAITING\\|DELEGATED\\) \\(\\[#[ABC]\\] \\)?")
	    (insert (format "[[bug:%s][#%s]] " bug bug)))))))
  (org-agenda-redo))

;;; ox-bugzilla.el ends here

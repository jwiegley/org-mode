;;; ox-org.el --- Org-X backend for Org-mode data

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

(require 'org)
(require 'org-x)

(eval-when-compile
  (require 'cl))

(defgroup org-x-org nil
  "Org-X backend for Org-mode."
  :tag "Org-X Org"
  :group 'org-x)

;;; Customization variables:

(defvar org-x-org-dispatchers
  '((applicable-backend . ignore)	; not meaningful
    (get-identifier	. org-x-get-identifier)
    (group-identifiers	. org-x-group-identifiers)
    (read-entry		. org-x-parse-entry)
    (write-entry	. org-x-insert-entry)
    (delete-entry	. org-x-delete-entry)
    (apply-changes	. nil)))	; use standard org-x functions

(defvar ox-org 'org-x-org-dispatchers)

(defcustom org-x-default-priority-silent t
  "If non-nil, priority B is never used since it's the default priority."
  :type 'boolean
  :group 'ox-org)

;;; Org contextual info:

(defun org-x-get-identifier (entry)
  (org-x-getter entry 'org-id))

(defsubst org-x--heading-depth ()
  (save-excursion
    (org-back-to-heading t)
    (if (let (case-fold-search) (looking-at org-complex-heading-regexp))
	(length (match-string 1)))))

(defun org-x-group-identifiers ()
  (save-excursion
    (outline-up-heading 1)
    (outline-next-heading)
    (let* ((depth (org-x--heading-depth))
	   (current-depth depth)
	   identifiers)
      (while (= depth current-depth)
	(setq identifiers
	      (cons (if (featurep 'org-id)
			;; jww (2011-11-08): What is `entry' bound to?
			(org-x-setter entry 'org-id (org-id-get-create))
		      (org-x-setter entry 'org-id (point-marker)))
		    identifiers))
	(org-forward-same-level 1 t)
	(setq depth (org-x--heading-depth)))
      (nreverse identifiers))))

;;; Org parser:

(defvar org-x-org-repeat-regexp
  (concat "<\\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [^>\n]*?\\)"
	  "\\(\\([.+]?\\+[0-9]+[dwmy]\\(/[0-9]+[dwmy]\\)?\\)\\)"))

(defsubst org-x-narrow-to-entry ()
  (outline-back-to-heading)
  (narrow-to-region (point) (progn (outline-next-heading) (point)))
  (goto-char (point-min)))

(defsubst trim-string (str)
  (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" ""
			    str))

(defun org-x-parse-entry (&optional position)
  (let ((entry (org-x-create-entry)))
    (save-restriction
      (save-excursion
	(if position (goto-char position))
	(if (featurep 'org-id)
	    (org-x-setter entry 'org-id (org-id-get-create))
	  (org-x-setter entry 'org-id (point-marker)))
	(org-x-narrow-to-entry)

	(let (log-entry body)
	  (while (not (eobp))
	    (cond
	     ((looking-at (concat "^\\(\\*+\\)\\( \\([A-Z][A-Z]+\\)\\)?"
				  "\\( \\[#\\([A-C]\\)\\]\\)? \\(.+?\\)"
				  "\\( +:\\(.+?\\):\\)?$"))
	      (org-x-set-depth entry (length (match-string-no-properties 1)))
	      (if (match-string-no-properties 2)
		  (org-x-set-state entry (match-string-no-properties 3)))
	      (let ((pri (match-string-no-properties 5)))
		(if pri
		    (org-x-set-priority entry (cond ((string= pri "A") 1)
						    ((string= pri "B") 2)
						    ((string= pri "C") 3)
						    (t 2)))))
	      (let ((title (match-string-no-properties 6)))
		(if (and title (> (length title) 0))
		    (org-x-set-title entry title)))
	      (if (match-string-no-properties 8)
		  (mapc (lambda (tag) (org-x-add-tag entry tag))
			(split-string (match-string-no-properties 8) ":" t))))

	     ((looking-at (concat "^\\(\\s-*\\)- State \"\\([A-Z]+\\)\"\\s-*"
				  "\\(from \"\\([A-Z]*\\)\"\\s-*\\)?"
				  "\\[\\([^]]+\\)\\]"
				  "\\(\\s-*\\\\\\\\\\)?\\s-*$"))
	      (if (and log-entry body)
		  (org-x-log-set-body log-entry (trim-string body)))
	      (setq body nil
		    log-entry
		    (org-x-add-log-entry entry
					 (apply 'encode-time
						(save-match-data
						  (org-parse-time-string
						   (match-string 5))))
					 nil nil
					 (match-string-no-properties 2)
					 (and (match-string-no-properties 3)
					      (match-string-no-properties 4)))))

	     ((looking-at (concat "^\\(\\s-*\\)- Note taken on\\s-*"
				  "\\[\\([^]]+\\)\\]"
				  "\\(\\s-*\\\\\\\\\\)?\\s-*$"))
	      (if (and log-entry body)
		  (org-x-log-set-body log-entry (trim-string body)))
	      (setq body nil
		    log-entry
		    (org-x-add-log-entry entry
					 (apply 'encode-time
						(save-match-data
						  (org-parse-time-string
						   (match-string 2))))
					 nil t)))

	     ((re-search-forward ":PROPERTIES:" (line-end-position) t)
	      (while (not (re-search-forward ":END:" (line-end-position) t))
		(assert (not (eobp)))
		(if (looking-at "^\\s-*:\\([^:]+\\):\\s-*\\(.*\\)")
		    (let ((name (match-string-no-properties 1))
			  (data (match-string-no-properties 2)))
		      (org-x-set-property entry name data)))
		(forward-line)))

	     ;; An old way of timestamping entries
	     ((looking-at "^\\s-*\\[\\([^]]+\\)\\]\\s-*$")
	      (unless (assoc "CREATED" (get 'item 'properties))
		(org-x-set-property entry "CREATED"
				    (concat "[" (match-string-no-properties 1) "]")
				    t)))

	     (t
	      (let (skip-line)
		(goto-char (line-beginning-position))
		(when (re-search-forward (concat "SCHEDULED:\\s-*"
						 org-x-org-repeat-regexp)
					 (line-end-position) t)
		  (org-x-set-scheduled entry
				       (apply 'encode-time
					      (save-match-data
						(org-parse-time-string
						 (match-string 1)))))
		  (if (match-string 2)
		      (org-x-set-scheduled-repeat
		       entry (match-string-no-properties 2)))
		  (setq skip-line t))

		(goto-char (line-beginning-position))
		(when (re-search-forward (concat "DEADLINE:\\s-*"
						 org-x-org-repeat-regexp)
					 (line-end-position) t)
		  (org-x-set-deadline entry
				      (apply 'encode-time
					     (save-match-data
					       (org-parse-time-string
						(match-string 1)))))
		  (if (match-string 2)
		      (org-x-set-deadline-repeat
		       entry (match-string-no-properties 2)))
		  (setq skip-line t))

		(goto-char (line-beginning-position))
		(when (re-search-forward "CLOSED:\\s-*\\[\\([^]\n]+\\)\\]"
					 (line-end-position) t)
		  (org-x-add-log-entry entry
				       (apply 'encode-time
					      (save-match-data
						(org-parse-time-string
						 (match-string 1))))
				       nil nil
				       (org-x-state entry))
		  (setq skip-line t))

		(goto-char (line-beginning-position))
		(when (re-search-forward "ARCHIVED:\\s-*<\\([^>\n]+\\)>"
					 (line-end-position) t)
		  (org-x-set-property entry "ARCHIVE_TIME"
				      (match-string-no-properties 1)
				      t)
		  (setq skip-line t))

		(if skip-line
		    (goto-char (line-end-position))))

	      (dotimes (i (+ (if log-entry 3 1) (org-x-depth entry)))
		(if (eq (char-after) ? )
		    (forward-char)
		  (unless (looking-at "^\\s-*$")
		    (when (and log-entry body)
		      (org-x-log-set-body log-entry (trim-string body))
		      (setq log-entry nil body nil)))))

	      (setq body (concat body "\n"
				 (buffer-substring-no-properties
				  (point) (line-end-position))))))
	    (forward-line))

	  (when (and log-entry body)
	    (org-x-log-set-body log-entry body)
	    (setq log-entry nil body nil))

	  (if body
	      (org-x-set-body entry (trim-string body)))

	  (save-restriction
	    (save-excursion
	      (widen)
	      (ignore-errors
		(dotimes (i 20)
		  (outline-up-heading 1)
		  (mapc (lambda (prop)
			  (org-x-set-parent-property entry (car prop)
						     (cdr prop)))
			(org-entry-properties)))))))))
    entry))

;;; Org writer:

(defsubst time-to-org-timestamp (time &optional long inactive)
  (format-time-string (substring (org-time-stamp-format long inactive) 1 -1)
		      time))

(defun org-x-insert-entry (entry)
  (let ((depth (or (org-x-depth entry)
		   (org-current-level))))
    (insert (make-string depth ?*) ? )

    (let ((state (org-x-state entry)))
      (if state
	  (insert state ? )))

    (let ((priority (org-x-priority entry)))
      (if (and priority (not (and org-x-default-priority-silent
				  (= 2 priority))))
	  (insert "[#" (cond ((= priority 1) "A")
			     ((= priority 2) "B")
			     ((>= priority 3) "C")) "] ")))

    (let ((title (org-x-title entry)))
      (insert title))

    (let ((tags (org-x-tags entry)))
      (when tags
	(insert "  :" (mapconcat 'identity tags ":") ":")))

    (insert ?\n)

    (let ((scheduled (org-x-scheduled entry))
	  (scheduled-repeat (org-x-scheduled-repeat entry))
	  (deadline (org-x-deadline entry))
	  (deadline-repeat (org-x-deadline-repeat entry)))
      (when (or scheduled deadline)
	(insert (make-string (1+ depth) ? ))

	(when scheduled
	  (insert "SCHEDULED: <" (time-to-org-timestamp scheduled))
	  (if scheduled-repeat
	      (insert " " scheduled-repeat))
	  (insert ">"))

	(when deadline
	  (if scheduled
	      (insert ? ))
	  (insert "DEADLINE: <" (time-to-org-timestamp deadline))
	  (if deadline-repeat
	      (insert " " deadline-repeat))
	  (insert ">"))

	(insert ?\n)))

    (let ((log-entries (org-x-log-entries entry)))
      (dolist (log log-entries)
	(setq log (cdr log))

	(let ((to-state (org-x-log-to-state log)))
	  (insert (make-string (1+ depth) ? ))

	  (cond
	   ((org-x-log-is-note log)
	    (insert "- Note taken on ["
		    (time-to-org-timestamp (org-x-log-timestamp log) t) "]"))
	   ((org-x-log-from-state log)
	    (insert (format "- State %-12s from %-12s [%s]"
			    (concat "\"" to-state "\"")
			    (concat "\"" (org-x-log-from-state log) "\"")
			    (time-to-org-timestamp (org-x-log-timestamp log) t))))
	   (t
	    (insert (format "- State %-12s [%s]"
			    (concat "\"" to-state "\"")
			    (time-to-org-timestamp (org-x-log-timestamp log) t)))))

	  (let ((body (org-x-log-body log)))
	    (if body
		(progn
		  (insert " \\\\\n")
		  (dolist (line (split-string body "\n"))
		    (insert (make-string (+ 3 depth) ? ) line ?\n)))
	      (insert ?\n))))))

    (let ((body (org-x-body entry)))
      (if body
	  (dolist (line (split-string body "\n"))
	    (insert (make-string (1+ depth) ? ) line ?\n))))

    (let ((props (org-x-properties entry)))
      (when props
	(insert (make-string (1+ depth) ? ) ":PROPERTIES:\n")
	(dolist (prop props)
	  (insert (make-string (1+ depth) ? )
		  (format "%-10s %s\n"
			  (concat ":" (car prop) ":") (cdr prop))))
	(insert (make-string (1+ depth) ? ) ":END:\n")))

    (outline-back-to-heading)
    (org-set-tags t)))

;;; Org utility functions:

(defun org-x-delete-entry ()
  (interactive)
  (save-restriction
    (save-excursion
      (org-x-narrow-to-entry)
      (delete-region (point-min) (point-max)))))

(defun org-x-replace-entry (entry)
  (interactive)
  (org-x-delete-entry)
  (org-x-insert-entry entry))

(defun org-x-normalize-entry ()
  (interactive)
  (org-x-replace-entry (org-x-parse-entry)))

(defun org-x-normalize-all-entries ()
  (interactive)
  (goto-char (point-min))
  (show-all)
  (untabify (point-min) (point-max))
  (while (re-search-forward "^\\*" nil t)
    (org-x-normalize-entry)
    (forward-line))
  (goto-char (point-min))
  (delete-trailing-whitespace)
  (save-buffer))

(provide 'ox-org)

;; arch-tag:

;;; ox-org.el ends here

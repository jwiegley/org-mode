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

(require 'ox)

(eval-when-compile
  (require 'cl))

(defvar org-x-org-backend
  '((read-entry  . org-x-parse-entry)
    (write-entry . org-x-insert-entry)))

(add-to-list 'org-x-backends (cons 'org 'org-x-org-backend))

(defun plist-to-alist (sym)
  (let ((l (symbol-plist sym))
        props)
    (while l
      (unless (or (null (cadr l))
                  (and (stringp (cadr l))
                       (= 0 (length (cadr l)))))
        (push (cons (car l) (cadr l)) props))
      (setq l (cddr l)))
    props))

(defsubst trim-string (str)
  (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" ""
                            str))

(defmacro org-x-org-resolve-log-entry ()
  `(when log-entry
     (put 'log-entry 'body
          (trim-string (get 'log-entry 'body)))
     (put 'item 'log
          (cons (plist-to-alist 'log-entry)
                (get 'item 'log)))
     (setq log-entry nil)
     (setplist 'log-entry '())))

(defsubst org-x-org-narrow-to-entry ()
  (outline-back-to-heading)
  (narrow-to-region (point) (progn (outline-next-heading) (point)))
  (goto-char (point-min)))

(defvar org-x-repeat-regexp
  (concat "<\\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [^>\n]*?\\)"
	  "\\(\\([.+]?\\+[0-9]+[dwmy]\\(/[0-9]+[dwmy]\\)?\\)\\)"))

(defun org-x-parse-entry (&optional position)
  (save-restriction
    (save-excursion
      (if position
          (goto-char position))
      (org-x-org-narrow-to-entry)

      (let (item log-entry)
        (setplist 'item '())
        (put 'item 'body "")
        (put 'item 'tags '())
        (put 'item 'log '())
        (put 'item 'properties '())
        (while (not (eobp))
          (cond
           ((looking-at (concat "^\\(\\*+\\)\\( \\([A-Z][A-Z]+\\)\\)?"
                                "\\( \\[#\\([A-C]\\)\\]\\)? \\(.+?\\)"
                                "\\( +:\\(.+?\\):\\)?$"))
            (put 'item 'depth (length (match-string-no-properties 1)))
            (if (match-string-no-properties 2)
                (put 'item 'state (match-string-no-properties 3)))
            (put 'item 'priority
                 (let ((pri (match-string-no-properties 5)))
                   (cond ((string= pri "A") 1)
                         ((string= pri "B") 2)
                         ((string= pri "C") 3)
                         (t 2))))
            (put 'item 'title (match-string-no-properties 6))
            (if (match-string-no-properties 8)
                (put 'item 'tags
                     (split-string (match-string-no-properties 8) ":" t))))

           ((looking-at (concat "^\\(\\s-*\\)- State \"\\([A-Z]+\\)\"\\s-*"
                                "\\(from \"\\([A-Z]*\\)\"\\s-*\\)?"
                                "\\[\\([^]]+\\)\\]\\(\\s-*\\\\\\\\\\)?\\s-*$"))
            (org-x-org-resolve-log-entry)

            (put 'log-entry 'depth
                 (+ 1 (length (match-string-no-properties 1))))
            (put 'log-entry 'to-state (match-string-no-properties 2))
            (if (and (match-string-no-properties 3)
                     (match-string-no-properties 4))
                (put 'log-entry 'from-state (match-string-no-properties 4)))
            (put 'log-entry 'timestamp
                 (apply 'encode-time
                        (save-match-data
                          (org-parse-time-string
                           (match-string-no-properties 5)))))
            (if (match-string-no-properties 6)
                (progn
                  (put 'log-entry 'body "")
                  (setq log-entry t))
              (put 'item 'log
                   (cons (plist-to-alist 'log-entry)
                         (get 'item 'log)))
              (setplist 'log-entry '())))

           ((looking-at (concat "^\\(\\s-*\\)- Note taken on\\s-*"
                                "\\[\\([^]]+\\)\\]\\(\\s-*\\\\\\\\\\)?\\s-*$"))
            (org-x-org-resolve-log-entry)

            (put 'log-entry 'depth
                 (+ 1 (length (match-string-no-properties 1))))
            (put 'log-entry 'timestamp
                 (apply 'encode-time
                        (save-match-data
                          (org-parse-time-string
                           (match-string-no-properties 2)))))
            (put 'log-entry 'note t)
            (if (match-string-no-properties 3)
                (progn
                  (put 'log-entry 'body "")
                  (setq log-entry t))
              (put 'item 'log
                   (cons (plist-to-alist 'log-entry)
                         (get 'item 'log)))
              (setplist 'log-entry '())))

           ((re-search-forward ":PROPERTIES:" (line-end-position) t)
            (while (not (re-search-forward ":END:" (line-end-position) t))
              (assert (not (eobp)))
              (if (looking-at "^\\s-*:\\([^:]+\\):\\s-*\\(.*\\)")
                  (let ((name (match-string-no-properties 1))
                        (data (match-string-no-properties 2)))
                    ;;(if (and (string= name "CREATED")
                    ;;         (string-match "\\[\\([^]\n]+\\)\\]" data))
                    ;;    (setq data (match-string 1 data)))
                    (unless (assoc name (get 'item 'properties))
                      (put 'item 'properties
                           (cons (cons name data)
                                 (get 'item 'properties))))))
              (forward-line)))

           ;; My old way of timestamping entries
           ((looking-at "^\\s-*\\[\\([^]]+\\)\\]\\s-*$")
            (unless (assoc "CREATED" (get 'item 'properties))
              (put 'item 'properties
                   (cons (cons "CREATED"
                               (concat "[" (match-string-no-properties 1) "]"))
                         (get 'item 'properties)))))

           (t
            (let (skip-line)
              (goto-char (line-beginning-position))
              (when (re-search-forward (concat "SCHEDULED:\\s-*"
					       org-x-repeat-regexp)
                                       (line-end-position) t)
                (put 'item 'scheduled
                     (apply 'encode-time
                            (save-match-data
                              (org-parse-time-string
                               (match-string-no-properties 1)))))
		(if (match-string 2)
		    (put 'item 'scheduled-repeat
			 (match-string-no-properties 2)))
                (setq skip-line t))
              (goto-char (line-beginning-position))
              (when (re-search-forward (concat "DEADLINE:\\s-*"
					       org-x-repeat-regexp)
                                       (line-end-position) t)
                (put 'item 'deadline (match-string-no-properties 1))
		(if (match-string 2)
		    (put 'item 'deadline-repeat
			 (match-string-no-properties 2)))
                (setq skip-line t))
              (goto-char (line-beginning-position))
              (when (re-search-forward "CLOSED:\\s-*\\[\\([^]\n]+\\)\\]"
                                       (line-end-position) t)
                (put 'log-entry 'to-state (get 'item 'state))
                (put 'log-entry 'timestamp
                     (apply 'encode-time
                            (save-match-data
                              (org-parse-time-string
                               (match-string-no-properties 1)))))
                (put 'item 'log
                     (cons (plist-to-alist 'log-entry)
                           (get 'item 'log)))
                (setplist 'log-entry '())
                (setq skip-line t))
              (goto-char (line-beginning-position))
              (when (re-search-forward "ARCHIVED:\\s-*<\\([^>\n]+\\)>"
                                       (line-end-position) t)
                (unless (assoc "ARCHIVE_TIME" (get 'item 'properties))
                  (put 'item 'properties
                       (cons (cons "ARCHIVE_TIME"
                                   (match-string-no-properties 1))
                             (get 'item 'properties))))
                (setq skip-line t))
              (if skip-line
                  (goto-char (line-end-position))))

            (assert (get (if log-entry 'log-entry 'item) 'depth))
            (dotimes (i (1+ (get (if log-entry 'log-entry 'item) 'depth)))
              (if (eq (char-after) ? )
                  (forward-char)
                (unless (looking-at "^\\s-*$")
                  (org-x-org-resolve-log-entry))))

            (put (if log-entry 'log-entry 'item) 'body
                 (concat (get (if log-entry 'log-entry 'item) 'body) "\n"
                         (buffer-substring-no-properties
                          (point) (line-end-position))))))
          (forward-line))

        (org-x-org-resolve-log-entry)

        (put 'item 'body (trim-string (get 'item 'body)))
	(put 'item 'log (sort (get 'item 'log)
			      (lambda (l r)
				(not (time-less-p (cdr (assq 'timestamp l))
						  (cdr (assq 'timestamp r)))))))
	(put 'item 'properties
	     (sort (get 'item 'properties)
		   (lambda (a b) (or (string= (car a) "ID")
				(string< (car a) (car b))))))

	(put 'item 'parent-properties
	     (let (props)
	       (save-restriction
		 (save-excursion
		   (widen)
		   (ignore-errors
		     (while t
		       (outline-up-heading 1)
		       (dolist (prop (org-entry-properties))
			 (let ((prop-entry (assoc (car prop) props)))
			   (if prop-entry
			       (setcdr prop-entry (cdr prop))
			     (setq props (cons prop props)))))))))
	       props))

        (plist-to-alist 'item)))))

(defcustom org-x-priority-B-silent t
  "If non-nil, priority B is never used since it's the default priority."
  :type 'boolean
  :group 'ox-org)

(defsubst time-to-org-timestamp (time &optional long)
  (format-time-string (substring (org-time-stamp-format long) 1 -1) time))

(defun org-x-insert-entry (entry)
  (let ((depth (or (org-x-depth entry)
		   (org-current-level))))
    (insert (make-string depth ?*) ? )

    (let ((state (org-x-state entry)))
      (if state
	  (insert state ? )))

    (let ((priority (org-x-priority entry)))
      (if (and priority (not (and org-x-priority-B-silent
				  (= 2 priority))))
	  (insert "[#" (cond ((= priority 1) "A")
			     ((= priority 2) "B")
			     ((>= priority 3) "C")) "] ")))

    (let ((title (org-x-title entry)))
      (insert title))

    (let ((tags (org-x-tags entry)))
      (when tags
	(insert "  :" (mapconcat 'identity tags ":") ":")
	(org-align-all-tags)))

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

	  (if (org-x-log-body log)
	      (progn
		(insert " \\\\\n")
		(dolist (line (split-string (org-x-log-body log) "\n"))
		  (insert (make-string (+ 3 depth) ? ) line ?\n)))
	    (insert ?\n)))))

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
	(insert (make-string (1+ depth) ? ) ":END:\n")))))

(defun org-x-delete-entry ()
  (interactive)
  (save-restriction
    (save-excursion
      (org-x-org-narrow-to-entry)
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
    (org-normalize-entry)
    (forward-line))
  (goto-char (point-min))
  (delete-trailing-whitespace)
  (save-buffer))

(provide 'ox-org)

;; arch-tag:

;;; ox-org.el ends here

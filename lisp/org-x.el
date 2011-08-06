;;; org-x.el --- working with Org data in a modular awy

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

(require 'ox-plugin)

(eval-when-compile
  (require 'cl))

(defgroup org-x nil
  "Org-X, the Org-mode Exchange."
  :tag "Org-X"
  :group 'org)

(defvar org-x-backends-loaded nil)

(defun org-x-set-backends (var value)
  "Set VAR to VALUE and load all requested backends."
  (set var value)
  (when (featurep 'org-x)
    (mapc
     (lambda (ext)
       (condition-case nil (require ext)
	 (error (message "Problems while trying to load Org-X backend `%s'"
			 ext))))
     org-x-backends)
    (setq org-x-backends-loaded t)))

(defcustom org-x-backends '(ox-org)
  "Org-X backends to be used."
  :set 'org-x-set-backends
  :type
  '(set :greedy t
	(const :tag "org:               Backend used to communicate with Org-mode" ox-org)
	(const :tag "redmine:           Redmine issue tracker" ox-redmine)
	(const :tag "bugzilla:          [NOT DONE] Bugzilla bug tracking system" ox-bugzilla)
	(const :tag "dired:             [NOT DONE] Files within a diectory" ox-dired)
	(const :tag "gnus:              [NOT DONE] Messages within a Gnus group" ox-gnus)
	(const :tag "wordpress:         [NOT DONE] WordPress posts and comments" ox-wordpress)
	(repeat :tag "External backends" :inline t
		(symbol :tag "Backend")))
  :group 'org-x)

(defcustom org-x-backend-dispatchers
  '((read-entry	       . (lambda (identifier)))
    (write-entry       . (lambda (entry)))
    (delete-entry      . (lambda (identifier)))
    (merge-entries     . (lambda (entry-list)))
    (pull-entry	       . (lambda (identifier)))
    (push-entry	       . (lambda (entry)))
    (sync-entry	       . (lambda (entry)))
    (get-identifier    . (lambda (entry)))
    (group-identifiers . (lambda (group-ident))))
  "A prototypical Org-X backend.  This variable is for demonstration only."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-x)

(defcustom org-x-states
  '("TODO" "STARTED" "WAITING" "CANCELED" "DONE" "NOTE")
  "The set of todo states which Org-X knows about.
Note that adding to this list only affects backends you may write, it
will not make existing backends sensitive to those states."
  :type '(repeat string)
  :group 'org-x)

;;; Dispatch functions:

(defvar org-x-dispatch-context nil
  "Org-mode context (location of point) for the current Org-X dispatch.
This is set automatically by the `context' parameter accepted by
the majority of dispatch API functions.")

(defun org-x-dispatch (backend symbol &optional context arg)
  (let ((be (symbol-value (cdr (assq backend org-x-backends))))
	(org-x-dispatch-context context))
    (if be
	(funcall (cdr (assq symbol be)) arg))))

(defsubst org-x-read-entry (backend &optional identifier context)
  (org-x-dispatch backend 'read-entry context identifier))

(defsubst org-x-write-entry (backend &optional entry context)
  (org-x-dispatch backend 'write-entry context entry))

(defsubst org-x-get-identifier (backend &optional entry context)
  (org-x-dispatch backend 'get-identifier context entry))

;;; Entry creation:

(defun org-x-create-entry () (list (cons 'is-entry t)))

;;; Entry atttribute getters:

(defun org-x-title (entry)
  (let ((title (cdr (assq 'title entry))))
    (if (null title)
	nil
      (assert (stringp title) t "Org-X entry title must be a string")
      title)))

(defun org-x-body (entry)
  (let ((body (cdr (assq 'body entry))))
    (if (null body)
	nil
      (assert (stringp body) t "Org-X entry body must be a string")
      body)))

(defun org-x-depth (entry)
  (let ((depth (cdr (assq 'depth entry))))
    (if (null depth)
	nil
      (assert (integerp depth) t "Org-X entry depth must be an integer")
      depth)))

(defun org-x-state (entry)
  (let ((state (cdr (assq 'state entry))))
    (if (null state)
	nil
      (assert (member state org-x-states) t
	      (format "Org-X entry state must be one of: %s" org-x-states))
      state)))

(defun org-x-priority (entry)
  (let ((priority (cdr (assq 'priority entry))))
    (if (null priority)
	nil
      (assert (integerp priority) t "Org-X entry priority must be an integer")
      priority)))

(defsubst org-x-scheduled (entry) (cdr (assq 'scheduled entry)))

(defun org-x-scheduled-repeat (entry)
  (let ((repeat (cdr (assq 'scheduled-repeat entry))))
    (if (null repeat)
	nil
      (assert (stringp repeat) t "Org-X log scheduled repeat must be a string")
      repeat)))

(defsubst org-x-deadline (entry) (cdr (assq 'deadline entry)))

(defun org-x-deadline-repeat (entry)
  (let ((repeat (cdr (assq 'deadline-repeat entry))))
    (if (null repeat)
	nil
      (assert (stringp repeat) t "Org-X log deadline repeat must be a string")
      repeat)))

(defun org-x-entry-backends (entry)
  "Return an alist of all the backends associated with ENTRY.
Each member of the alist is of the form (BACKEND . IDENTIFIER),
where BACKEND is a symbol identifying the related backend, and
IDENTIFIER is how that backend knows this entry."
  (let (entry-backends)
    (dolist (backend org-x-backends)
      (let ((ident (org-x-get-identifier entry)))
	(if ident
	    (add-to-list 'entry-backends (cons backend ident)))))))

;;; Entry property getters:

(defun org-x-properties (entry)
  (let ((properties (cdr (assq 'properties entry))))
    (if (null properties)
	nil
      (assert (listp properties) t "Org-X entry properties must be an alist")
      properties)))

(defun org-x-parent-properties (entry)
  (let ((properties (cdr (assq 'parent-properties entry))))
    (if (null properties)
	nil
      (assert (listp properties) t
	      "Org-X entry parent properties must be an alist")
      properties)))

(defsubst org-x-has-property (entry name &optional check-parents)
  (or (assoc name (org-x-properties entry))
      (if check-parents
	  (assoc name (org-x-parent-properties entry)))))

(defsubst org-x-get-property (entry name &optional check-parents)
  (cdr (org-x-has-property entry name check-parents)))

;;; Entry tag getters:

(defun org-x-tags (entry)
  (let ((tags (cdr (assq 'tags entry))))
    (if (null tags)
	nil
      (assert (listp tags) t "Org-X entry tags must be a list")
      tags)))

(defsubst org-x-has-tag (entry name)
  (not (null (member name (cdr (assq 'tags entry))))))

(defsubst org-x-get-tag (entry name)
  (car (member name (cdr (assq 'tags entry)))))

;;; Entry log getters:

(defun org-x-log-entries (entry)
  (let ((log-entries (cdr (assq 'log entry))))
    (if (null log-entries)
	nil
      (assert (listp log-entries) t "Org-X log entries must be a list")
      log-entries)))

(defun org-x-log-entry (entry timestamp)
  (catch 'log-entry
    (dolist (log (org-x-log-entries entry))
      (let ((log-timestamp (org-x-log-timestamp log)))
	(if (equal timestamp log-timestamp)
	    (throw 'log-entry log))))))

(defsubst org-x-has-log-entry (entry timestamp)
  (not (null (org-x-log-entry entry timestamp))))

(defsubst org-x-log-timestamp (log-entry) (cdr (assq 'timestamp log-entry)))

(defun org-x-log-body (log-entry)
  (let ((body (cdr (assq 'body log-entry))))
    (if (null body)
	nil
      (assert (stringp body) t "Org-X log body must be a string")
      body)))

(defun org-x-log-from-state (log-entry)
  (let ((state (cdr (assq 'from-state log-entry))))
    (if (null state)
	nil
      (assert (member state org-x-states) t
	      (format "Org-X log from state must be one of: %s" org-x-states))
      state)))

(defun org-x-log-to-state (log-entry)
  (let ((state (cdr (assq 'to-state log-entry))))
    (if (null state)
	nil
      (assert (member state org-x-states) t
	      (format "Org-X log to state must be one of: %s" org-x-states))
      state)))

(defsubst org-x-log-is-note (log-entry) (cdr (assq 'note log-entry)))

;;; Entry atttribute setters:

(defun org-x-propagate (entry symbol data)
  (mapc (lambda (info)
	  (org-x-dispatch (car info)
			  (intern (concat "set-" (symbol-name symbol)))
			  (cdr info) data))
	(org-x-entry-backends entry)))

(defun org-x-setter (entry symbol data &optional propagate no-overwrite)
  (let ((cell (assq symbol entry)))
    (unless (and (cdr cell) no-overwrite)
      (if cell
	  (setcdr cell data)
	(nconc entry (list (cons symbol data))))
      (if propagate
	  (org-x-propagate entry (intern (concat "set-" (symbol-name symbol)))
			   data))))
  data)

(defun org-x-eraser (entry symbol &optional propagate no-overwrite)
  (let ((cell (assq symbol entry)))
    (unless (and (cdr cell) no-overwrite)
      (if cell
	  (setcdr entry (delq cell (cdr entry))))
      (if propagate
	  (org-x-propagate entry
			   (intern (concat "remove-" (symbol-name symbol)))
			   nil)))))

(defun org-x-set-title (entry title &optional propagate no-overwrite)
  (assert (stringp title) t "Org-X entry title must be a string")
  (org-x-setter entry 'title title propagate no-overwrite))
(defun org-x-clear-title (entry &optional propagate)
  (org-x-eraser entry 'title propagate))

(defun org-x-set-body (entry body &optional propagate no-overwrite)
  (assert (stringp body) t "Org-X entry body must be a string")
  (org-x-setter entry 'body body propagate no-overwrite))
(defun org-x-clear-body (entry &optional propagate)
  (org-x-eraser entry 'body propagate))

(defun org-x-set-depth (entry depth &optional propagate no-overwrite)
  (assert (integerp depth) t "Org-X entry depth must be an integer")
  (org-x-setter entry 'depth depth propagate no-overwrite))
(defun org-x-clear-depth (entry &optional propagate)
  (org-x-eraser entry 'depth propagate))

(defun org-x-set-state (entry state &optional propagate no-overwrite)
  (assert (member state org-x-states) t
	  (format "Org-X entry state must be one of: %s" org-x-states))
  (org-x-setter entry 'state state propagate no-overwrite))
(defun org-x-clear-state (entry &optional propagate)
  (org-x-eraser entry 'state propagate))

(defun org-x-set-priority (entry priority &optional propagate no-overwrite)
  (assert (integerp priority) t "Org-X entry priority must be an integer")
  (org-x-setter entry 'priority priority propagate no-overwrite))
(defun org-x-clear-priority (entry &optional propagate)
  (org-x-eraser entry 'priority propagate))

(defun org-x-set-scheduled (entry scheduled &optional propagate no-overwrite)
  (org-x-setter entry 'scheduled scheduled propagate no-overwrite))
(defun org-x-clear-scheduled (entry &optional propagate)
  (org-x-eraser entry 'scheduled propagate))

(defun org-x-set-scheduled-repeat
  (entry repeat &optional propagate no-overwrite)
  (assert (stringp repeat) t "Org-X log scheduled repeat must be a string")
  (org-x-setter entry 'scheduled-repeat repeat propagate no-overwrite))
(defun org-x-clear-scheduled-repeat (entry &optional propagate)
  (org-x-eraser entry 'scheduled-repeat propagate))

(defun org-x-set-deadline (entry deadline &optional propagate no-overwrite)
  (org-x-setter entry 'deadline deadline propagate no-overwrite))
(defun org-x-clear-deadline (entry &optional propagate)
  (org-x-eraser entry 'deadline propagate))

(defun org-x-set-deadline-repeat
  (entry repeat &optional propagate no-overwrite)
  (assert (stringp repeat) t "Org-X log deadline repeat must be a string")
  (org-x-setter entry 'deadline-repeat repeat propagate no-overwrite))
(defun org-x-clear-deadline-repeat (entry &optional propagate)
  (org-x-eraser entry 'deadline-repeat propagate))

;;; Entry property setters:

(defsubst org-x-set-property
  (entry name value &optional propagate no-overwrite)
  (let ((cell (assq name (org-x-properties entry))))
    (unless (and (cdr cell) no-overwrite)
      (if cell
	  (setcdr cell value)
	(nconc (org-x-properties entry)
	       (list (cons name value))))
      (if propagate
	  (org-x-propagate entry 'set-property (cons name value)))))
  value)

(defsubst org-x-remove-property (entry name value &optional propagate)
  (let* ((properties (assq 'properties entry))
	 (cell (assq name (cdr properties))))
    (if cell
	(setcdr properties (delq cell (cdr properties))))
    (if propagate
	(org-x-propagate entry 'remove-property name))))

;;; Entry tag setters:

(defun org-x-add-tag (entry name &optional propagate)
  (let ((cell (member name (org-x-tags entry))))
    (unless cell
      (nconc (org-x-tags entry) (list name))
      (if propagate
	  (org-x-propagate entry 'add-tag name))))
  name)

(defun org-x-remove-tag (entry name &optional propagate)
  (let* ((tags (assq 'tags entry))
	 (cell (member name (cdr tags))))
    (if cell
	(setcdr tags (delete name (cdr tags))))
    (if propagate
	(org-x-propagate entry 'remove-tag name))))

;;; Entry log setters:

(defun org-x-add-log-entry (entry timestamp body &optional is-note to-state
				  from-state propagate no-overwrite)
  (let* ((log-entries (assq 'log entry))
	 (log (catch 'found
		(dolist (log (cdr log-entries))
		  (if (equal timestamp (org-x-log-timestamp log))
		      (throw 'found log))))))
    (unless (and log no-overwrite)
      (let ((new-log (list (cons 'timestamp timestamp))))
	(if body       (add-to-list 'new-log (cons 'body body)))
	(if is-note    (add-to-list 'new-log (cons 'is-note is-note)))
	(if to-state   (add-to-list 'new-log (cons 'to-state to-state)))
	(if from-state (add-to-list 'new-log (cons 'from-state from-state)))

	(if (and log log-entries)
	    (setcdr log-entries (delq log (cdr log-entries))))
	(if log-entries
	    (setcdr log-entries
		    (sort (cons new-log (cdr log-entries))
			  (lambda (l r)
			    (not (time-less-p (cdr (assq 'timestamp l))
					      (cdr (assq 'timestamp r)))))))
	  (nconc entry (list (list (cons 'log new-log)))))
	(if propagate
	    (org-x-propagate entry 'add-log-entry
			     (list timestamp body is-note
				   to-state from-state)))))))

(defun org-x-remove-log-entry (entry timestamp &optional propagate)
  (let ((log-entries (assq 'log entry)))
    (dolist (log (cdr log-entries))
      (when (equal timestamp (org-x-log-timestamp log))
	(setcdr log-entries (delq log (cdr log-entries)))
	(if propagate
	    (org-x-propagate entry 'remove-log-entry timestamp))))))

(defun org-x-log-setter
  (log-entry symbol data &optional propagate no-overwrite)
  (let ((cell (assq symbol log-entry)))
    (unless (and (cdr cell) no-overwrite)
      (setcdr cell data)
      (if propagate
	  (org-x-propagate log-entry
			   (intern (concat "set-log-" (symbol-name symbol)))
			   data)))))

(defsubst org-x-log-set-timestamp
  (log-entry timestamp &optional propagate no-overwrite)
  (org-x-log-setter log-entry 'timestamp timestamp propagate no-overwrite))

(defun org-x-log-set-body (log-entry body &optional propagate no-overwrite)
  (assert (stringp body) t "Org-X log entry body must be a string")
  (org-x-log-setter log-entry 'body body propagate no-overwrite))

(defun org-x-log-set-from-state
  (log-entry state &optional propagate no-overwrite)
  (assert (member state org-x-states) t
	  (format "Org-X log entry from-state must be one of: %s"
		  org-x-states))
  (org-x-log-setter log-entry 'from-state state propagate no-overwrite))

(defun org-x-log-set-to-state
  (log-entry state &optional propagate no-overwrite)
  (assert (member state org-x-states) t
	  (format "Org-X log entry to-state must be one of: %s" org-x-states))
  (org-x-log-setter log-entry 'to-state state propagate no-overwrite))

(defsubst org-x-log-set-is-note
  (log-entry is-note &optional propagate no-overwrite)
  (org-x-log-setter log-entry 'note is-note propagate no-overwrite))

(provide 'org-x)

;; arch-tag: 

;;; org-x.el ends here

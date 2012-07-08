;;; org-x.el --- working with Org data in a modular way

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

;;; Customization variables:

(eval-when-compile
  (defvar org-x-backends))
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
  '((applicable-backend . (lambda (entry-or-position)))
    (get-identifier	. (lambda (entry)))
    (group-identifiers	. (lambda ()))
    (read-entry		. (lambda (identifier)))
    (write-entry	. (lambda (entry)))
    (delete-entry	. (lambda (identifier)))
    (apply-changes	. (lambda ((entry . changes)))))
  "A prototypical Org-X backend.  This variable is for demonstration only."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-x)

(defcustom org-x-states
  '("TODO" "STARTED" "WAITING" "CANCELED" "DONE" "NOTE"
    "DELEGATED" "DEFERRED" "SOMEDAY" "PROJECT")
  "The set of todo states which Org-X knows about.
Note that adding to this list only affects backends you may write, it
will not make existing backends sensitive to those states."
  :type '(repeat string)
  :group 'org-x)

;;; Org-X dispatcher

(defvar org-x-dispatch-context)

(put 'org-x-dispatch-context 'variable-documentation
     "Org-mode context (location of point) for the current Org-X dispatch.
This is set automatically by the `context' parameter accepted by
the majority of dispatch API functions.")

(defmacro with-org-x-context (context &rest body)
  `(let ((org-x-dispatch-context ,context))
     ,@body))

(put 'with-org-x-context 'lisp-indent-function 1)

(and (fboundp 'font-lock-add-keywords)
     (font-lock-add-keywords
      'emacs-lisp-mode
      '(("(with-org-x-context\\>"  . font-lock-keyword-face))))

;;; Dispatch functions:

(defsubst org-x-backend-symbol (backend) (car backend))
(defsubst org-x-backend-context (backend) (cdr backend))

(defun org-x-backends (entry-or-pos)
  "Return a list of all backends associated with ENTRY-OR-POS."
  (delete nil (mapcar (lambda (backend)
			(org-x-applicable-backend backend entry-or-pos))
		      org-x-backends)))

(defun org-x-dispatch (backend symbol &rest args)
  (let* ((be (symbol-value
	      (symbol-value (if (symbolp backend)
				backend
			      (car backend)))))
         (func (and be (cdr (assq symbol be)))))
    (and func
	 (if (symbolp backend)
	     (apply func args)
	   (with-org-x-context (cdr backend)
	     (apply func args))))))

(defun org-x-applicable-backend (backend entry-or-pos)
  (org-x-dispatch backend 'applicable-backend entry-or-pos))

(defsubst org-x-get-identifier (backend entry)
  (org-x-dispatch backend 'get-identifier entry))

(defsubst org-x-group-identifiers (backend)
  (org-x-dispatch backend 'group-identifiers))

(defsubst org-x-identifier-list (backend entry)
  (org-x-dispatch backend 'get-identifier entry))

(defsubst org-x-read-entry (backend identifier)
  (org-x-dispatch backend 'read-entry identifier))

(defsubst org-x-write-entry (backend entry)
  (org-x-dispatch backend 'write-entry entry))

;;; Entry creation:

(defun org-x-create-entry () (list (cons 'entry t)))

;;; Entry generic getter and setter:

(defun org-x-getter (entry symbol)
  (cdr (assq symbol entry)))

;;; Entry atttribute getters:

(defun org-x-title (entry)
  (let ((title (cdr (assq 'title entry))))
    (if (null title)
        nil
      (assert (stringp title) nil "Org-X entry title must be a string")
      title)))

(defun org-x-body (entry)
  (let ((body (cdr (assq 'body entry))))
    (if (null body)
        nil
      (assert (stringp body) nil "Org-X entry body must be a string")
      body)))

(defun org-x-depth (entry)
  (let ((depth (cdr (assq 'depth entry))))
    (if (null depth)
        nil
      (assert (integerp depth) nil "Org-X entry depth must be an integer")
      depth)))

(defun org-x-state (entry)
  (let ((state (cdr (assq 'state entry))))
    (if (null state)
        nil
      (assert (member state org-x-states) nil
              (format "Org-X entry state must be one of: %s" org-x-states))
      state)))

(defun org-x-priority (entry)
  (let ((priority (cdr (assq 'priority entry))))
    (if (null priority)
        nil
      (assert (integerp priority) nil "Org-X entry priority must be an integer")
      priority)))

(defsubst org-x-scheduled (entry) (cdr (assq 'scheduled entry)))

(defun org-x-scheduled-time (entry)
  (let ((time (cdr (assq 'scheduled-time entry))))
    (if (null time)
        nil
      (assert (stringp time) nil "Org-X log scheduled time must be a string")
      time)))

(defun org-x-scheduled-repeat (entry)
  (let ((repeat (cdr (assq 'scheduled-repeat entry))))
    (if (null repeat)
        nil
      (assert (stringp repeat) nil "Org-X log scheduled repeat must be a string")
      repeat)))

(defsubst org-x-deadline (entry) (cdr (assq 'deadline entry)))

(defun org-x-deadline-time (entry)
  (let ((time (cdr (assq 'deadline-time entry))))
    (if (null time)
        nil
      (assert (stringp time) nil "Org-X log deadline time must be a string")
      time)))

(defun org-x-deadline-repeat (entry)
  (let ((repeat (cdr (assq 'deadline-repeat entry))))
    (if (null repeat)
        nil
      (assert (stringp repeat) nil "Org-X log deadline repeat must be a string")
      repeat)))

;;; Entry property getters:

(defun org-x-properties (entry)
  (let ((properties (cdr (assq 'properties entry))))
    (if (null properties)
        nil
      (assert (listp properties) nil "Org-X entry properties must be an alist")
      properties)))

(defun org-x-parent-properties (entry)
  (let ((properties (cdr (assq 'parent-properties entry))))
    (if (null properties)
        nil
      (assert (listp properties) nil
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
      (assert (listp tags) nil "Org-X entry tags must be a list")
      tags)))

(defsubst org-x-has-tag (entry name)
  (not (null (member name (org-x-tags entry)))))

(defsubst org-x-get-tag (entry name)
  (car (member name (org-x-tags entry))))

;;; Entry log getters:

(defun org-x-log-entries (entry)
  (let ((log-entries (cdr (assq 'log entry))))
    (if (null log-entries)
        nil
      (assert (listp log-entries) nil "Org-X log entries must be a list")
      log-entries)))

(defsubst org-x-has-log-entry (entry timestamp)
  (not (null (assoc timestamp (org-x-log-entries entry)))))

(defsubst org-x-get-log-entry (entry timestamp)
  (cdr (assoc timestamp (org-x-log-entries entry))))

(defsubst org-x-log-timestamp (log-entry)
  (cdr (assq 'timestamp log-entry)))

(defun org-x-log-body (log-entry)
  (let ((body (cdr (assq 'body log-entry))))
    (if (null body)
        nil
      (assert (stringp body) nil "Org-X log body must be a string")
      body)))

(defun org-x-log-from-state (log-entry)
  (let ((state (cdr (assq 'from-state log-entry))))
    (if (null state)
        nil
      (assert (member state org-x-states) nil
              (format "Org-X log from state must be one of: %s" org-x-states))
      state)))

(defun org-x-log-to-state (log-entry)
  (let ((state (cdr (assq 'to-state log-entry))))
    (if (null state)
        nil
      (assert (member state org-x-states) nil
              (format "Org-X log to state must be one of: %s" org-x-states))
      state)))

(defsubst org-x-log-is-note (log-entry)
  (cdr (assq 'note log-entry)))

;;; Entry logbook getters:

(defun org-x-logbook-entries (entry)
  (let ((logbook-entries (cdr (assq 'logbook entry))))
    (if (null logbook-entries)
        nil
      (assert (listp logbook-entries) nil
	      "Org-X logbook entries must be a list")
      logbook-entries)))

(defsubst org-x-has-logbook-entry (entry begin)
  (not (null (assoc begin (org-x-logbook-entries entry)))))

(defsubst org-x-get-logbook-entry (entry begin)
  (cdr (assoc begin (org-x-logbook-entries entry))))

(defun org-x-logbook-begin (logbook-entry)
  (let ((begin (cdr (assq 'begin logbook-entry))))
    (if (null begin)
        nil
      (assert (listp begin) nil "Org-X logbook begin must be a time value")
      begin)))

(defun org-x-logbook-end (logbook-entry)
  (let ((end (cdr (assq 'end logbook-entry))))
    (if (null end)
        nil
      (assert (listp end) nil "Org-X logbook end must be a time value")
      end)))

(defun org-x-logbook-duration (logbook-entry)
  (let ((duration (cdr (assq 'duration logbook-entry))))
    (if (null duration)
        nil
      (assert (stringp duration) nil "Org-X logbook duration must be a string")
      duration)))

;;; Entry atttribute setters:

(defun org-x-propagate (entry symbol data)
  (mapc (lambda (info)
          (org-x-dispatch (car info) symbol (cdr info) data))
        (org-x-backends entry)))

(defun org-x-setter (entry symbol data &optional no-overwrite propagate)
  (let ((cell (assq symbol entry)))
    (unless (and (cdr cell) no-overwrite)
      (if cell
          (setcdr cell data)
        (nconc entry (list (cons symbol data))))))
  (if propagate
      (org-x-propagate entry (intern (concat "set-" (symbol-name symbol)))
                       data))
  data)

(defun org-x-eraser (entry symbol &optional no-overwrite propagate)
  (let ((cell (assq symbol entry)))
    (unless (and (cdr cell) no-overwrite)
      (if cell
          (setcdr entry (delq cell (cdr entry))))))
  (if propagate
      (org-x-propagate entry
                       (intern (concat "clear-" (symbol-name symbol)))
                       nil)))

(defun org-x-set-title (entry title &optional no-overwrite propagate)
  (assert (stringp title) nil "Org-X entry title must be a string")
  (org-x-setter entry 'title title no-overwrite propagate))
(defun org-x-clear-title (entry &optional propagate)
  (org-x-eraser entry 'title propagate))

(defun org-x-set-body (entry body &optional no-overwrite propagate)
  (assert (stringp body) nil "Org-X entry body must be a string")
  (if (and body (> (length body) 0))
      (org-x-setter entry 'body body no-overwrite propagate)
    (org-x-clear-body entry propagate)))
(defun org-x-clear-body (entry &optional propagate)
  (org-x-eraser entry 'body propagate))

(defun org-x-set-depth (entry depth &optional no-overwrite propagate)
  (assert (integerp depth) nil "Org-X entry depth must be an integer")
  (org-x-setter entry 'depth depth no-overwrite propagate))
(defun org-x-clear-depth (entry &optional propagate)
  (org-x-eraser entry 'depth propagate))

(defun org-x-set-state (entry state &optional no-overwrite propagate)
  (assert (member state org-x-states) nil
          (format "Org-X entry state must be one of: %s" org-x-states))
  (org-x-setter entry 'state state no-overwrite propagate))
(defun org-x-clear-state (entry &optional propagate)
  (org-x-eraser entry 'state propagate))

(defun org-x-set-priority (entry priority &optional no-overwrite propagate)
  (assert (integerp priority) nil "Org-X entry priority must be an integer")
  (org-x-setter entry 'priority priority no-overwrite propagate))
(defun org-x-clear-priority (entry &optional propagate)
  (org-x-eraser entry 'priority propagate))

(defun org-x-set-scheduled (entry scheduled &optional no-overwrite propagate)
  (org-x-setter entry 'scheduled scheduled no-overwrite propagate))
(defun org-x-clear-scheduled (entry &optional propagate)
  (org-x-eraser entry 'scheduled propagate))

(defun org-x-set-scheduled-time
  (entry time &optional no-overwrite propagate)
  (assert (stringp time) nil "Org-X log scheduled time must be a string")
  (org-x-setter entry 'scheduled-time time no-overwrite propagate))
(defun org-x-clear-scheduled-time (entry &optional propagate)
  (org-x-eraser entry 'scheduled-time propagate))

(defun org-x-set-scheduled-repeat
  (entry repeat &optional no-overwrite propagate)
  (assert (stringp repeat) nil "Org-X log scheduled repeat must be a string")
  (org-x-setter entry 'scheduled-repeat repeat no-overwrite propagate))
(defun org-x-clear-scheduled-repeat (entry &optional propagate)
  (org-x-eraser entry 'scheduled-repeat propagate))

(defun org-x-set-deadline (entry deadline &optional no-overwrite propagate)
  (org-x-setter entry 'deadline deadline no-overwrite propagate))
(defun org-x-clear-deadline (entry &optional propagate)
  (org-x-eraser entry 'deadline propagate))

(defun org-x-set-deadline-time
  (entry time &optional no-overwrite propagate)
  (assert (stringp time) nil "Org-X log deadline time must be a string")
  (org-x-setter entry 'deadline-time time no-overwrite propagate))
(defun org-x-clear-deadline-time (entry &optional propagate)
  (org-x-eraser entry 'deadline-time propagate))

(defun org-x-set-deadline-repeat
  (entry repeat &optional no-overwrite propagate)
  (assert (stringp repeat) nil "Org-X log deadline repeat must be a string")
  (org-x-setter entry 'deadline-repeat repeat no-overwrite propagate))
(defun org-x-clear-deadline-repeat (entry &optional propagate)
  (org-x-eraser entry 'deadline-repeat propagate))

;;; Entry property setters:

(defun org-x-set-parent-property (entry name value)
  (let* ((properties (org-x-parent-properties entry))
         (cell (assoc name properties)))
    (if cell
        (setcdr cell value)
      (if properties
          (nconc properties (list (cons name value)))
        (nconc entry (list (cons 'parent-properties
                                 (list (cons name value))))))))
  value)

(defun org-x-set-property (entry name value &optional no-overwrite propagate)
  (let* ((properties (org-x-properties entry))
         (cell (assoc name properties)))
    (unless (and (cdr cell) no-overwrite)
      (if cell
          (setcdr cell value)
        (if properties
            (nconc properties (list (cons name value)))
          (nconc entry (list (cons 'properties
                                   (list (cons name value)))))))))
  (if propagate
      (org-x-propagate entry 'set-property (cons name value)))
  value)

(defun org-x-remove-property (entry name &optional propagate)
  (let* ((properties (assq 'properties entry))
         (cell (assoc name (cdr properties))))
    (if cell
        (setcdr properties (delq cell (cdr properties)))))
  (if propagate
      (org-x-propagate entry 'remove-property name)))

;;; Entry tag setters:

(defun org-x-add-tag (entry name &optional propagate)
  (let* ((tags (org-x-tags entry))
         (cell (member name tags)))
    (unless cell
      (if tags
          (nconc tags (list name))
        (nconc entry (list (cons 'tags (list name)))))))
  (if propagate
      (org-x-propagate entry 'add-tag name))
  name)

(defun org-x-remove-tag (entry name &optional propagate)
  (let* ((tags (assq 'tags entry))
         (cell (member name (cdr tags))))
    (if cell
        (setcdr tags (delete name (cdr tags)))))
  (if propagate
      (org-x-propagate entry 'remove-tag name)))

;;; Entry log setters:

(defun org-x-add-log-entry (entry timestamp body &optional is-note to-state
                                  from-state no-overwrite propagate)
  (let ((new-log (list (cons 'timestamp timestamp))))
    (let* ((log-entries (assq 'log entry))
           (log (assoc timestamp (cdr log-entries))))
      (unless (and log no-overwrite)
        (if body       (add-to-list 'new-log (cons 'body body)))
        (if is-note    (add-to-list 'new-log (cons 'note is-note)))
        (if to-state   (add-to-list 'new-log (cons 'to-state to-state)))
        (if from-state (add-to-list 'new-log (cons 'from-state from-state)))

        (if (and log log-entries)
            (setcdr log-entries (delq log (cdr log-entries))))
        (if log-entries
            (setcdr log-entries
                    (cons (cons timestamp new-log)
                          (cdr log-entries)))
          (nconc entry (list (cons 'log (list (cons timestamp new-log))))))))
    (if propagate
        (org-x-propagate entry 'add-log-entry
                         (list timestamp body is-note
                               to-state from-state)))
    new-log))

(defun org-x-add-whole-log-entry (entry log &optional no-overwrite propagate)
  (org-x-add-log-entry entry (org-x-log-timestamp log)
                       (org-x-log-body log) (org-x-log-is-note log)
                       (org-x-log-to-state log) (org-x-log-from-state log)
                       no-overwrite propagate))

(defun org-x-remove-log-entry (entry timestamp &optional propagate)
  (let* ((log-entries (assq 'log entry))
         (log (assoc timestamp (cdr log-entries))))
    (if (and log log-entries)
        (setcdr log-entries (delq log (cdr log-entries)))))
  (if propagate
      (org-x-propagate entry 'remove-log-entry timestamp)))

(defun org-x-log-setter
  (log-entry symbol data &optional no-overwrite propagate)
  (let ((cell (assq symbol log-entry)))
    (if cell
        (unless (and (cdr cell) no-overwrite)
          (setcdr cell data))
      (nconc log-entry (list (cons symbol data)))))
  (if propagate
      (org-x-propagate log-entry
                       (intern (concat "log-set-" (symbol-name symbol)))
                       data)))

(defun org-x-log-eraser (log-entry symbol &optional no-overwrite propagate)
  (let ((cell (assq symbol log-entry)))
    (unless (and (cdr cell) no-overwrite)
      (if cell
          (setcdr log-entry (delq cell (cdr log-entry))))))
  (if propagate
      ;; jww (2011-08-07): Should I propagate log-entry changes?
      (org-x-propagate log-entry
                       (intern (concat "log-clear-" (symbol-name symbol)))
                       nil)))

(defun org-x-log-set-body (log-entry body &optional no-overwrite propagate)
  (assert (stringp body) nil "Org-X log entry body must be a string")
  (if (and body (> (length body) 0))
      (org-x-log-setter log-entry 'body body no-overwrite propagate)
    (org-x-log-clear-body log-entry propagate)))
(defun org-x-log-clear-body (log-entry &optional propagate)
  (org-x-log-eraser log-entry 'log-clear-body propagate))

(defun org-x-log-set-from-state
  (log-entry state &optional no-overwrite propagate)
  (assert (member state org-x-states) nil
          (format "Org-X log entry from-state must be one of: %s"
                  org-x-states))
  (org-x-log-setter log-entry 'from-state state no-overwrite propagate))
(defun org-x-log-clear-from-state (log-entry &optional propagate)
  (org-x-log-eraser log-entry 'log-clear-from-state propagate))

(defun org-x-log-set-to-state
  (log-entry state &optional no-overwrite propagate)
  (assert (member state org-x-states) nil
          (format "Org-X log entry to-state must be one of: %s" org-x-states))
  (org-x-log-setter log-entry 'to-state state no-overwrite propagate))
(defun org-x-log-clear-to-state (log-entry &optional propagate)
  (org-x-log-eraser log-entry 'log-clear-to-state propagate))

(defsubst org-x-log-set-is-note
  (log-entry is-note &optional no-overwrite propagate)
  (org-x-log-setter log-entry 'note is-note no-overwrite propagate))
(defun org-x-log-clear-is-note (log-entry &optional propagate)
  (org-x-log-eraser log-entry 'log-clear-is-note propagate))

(defun org-x-log-set-body-by-timestamp
  (entry timestamp body &optional no-overwrite propagate)
  (org-x-log-set-body (org-x-get-log-entry entry timestamp)
                      body no-overwrite propagate))
(defun org-x-log-clear-body-by-timestamp (entry timestamp &optional propagate)
  (org-x-log-clear-body (org-x-get-log-entry entry timestamp) propagate))

(defun org-x-log-set-from-state-by-timestamp
  (entry timestamp state &optional no-overwrite propagate)
  (org-x-log-set-from-state (org-x-get-log-entry entry timestamp)
                            state no-overwrite propagate))
(defun org-x-log-clear-from-state-by-timestamp
  (entry timestamp &optional propagate)
  (org-x-log-clear-from-state (org-x-get-log-entry entry timestamp) propagate))

(defun org-x-log-set-to-state-by-timestamp
  (entry timestamp state &optional no-overwrite propagate)
  (org-x-log-set-to-state (org-x-get-log-entry entry timestamp)
                          state no-overwrite propagate))
(defun org-x-log-clear-to-state-by-timestamp
  (entry timestamp &optional propagate)
  (org-x-log-clear-to-state (org-x-get-log-entry entry timestamp) propagate))

(defsubst org-x-log-set-is-note-by-timestamp
  (entry timestamp is-note &optional no-overwrite propagate)
  (org-x-log-set-is-note (org-x-get-log-entry entry timestamp)
                         is-note no-overwrite propagate))
(defun org-x-log-clear-is-note-by-timestamp
  (entry timestamp &optional propagate)
  (org-x-log-clear-is-note (org-x-get-log-entry entry timestamp) propagate))

;;; Entry logbook setters:

(defun org-x-add-logbook-entry (entry begin &optional end duration
				      no-overwrite propagate)
  (let ((new-logbook (list (cons 'begin begin))))
    (let* ((logbook-entries (assq 'logbook entry))
           (logbook (assoc begin (cdr logbook-entries))))
      (unless (and logbook no-overwrite)
        (if end      (add-to-list 'new-logbook (cons 'end end)))
        (if duration (add-to-list 'new-logbook (cons 'duration duration)))

        (if (and logbook logbook-entries)
            (setcdr logbook-entries (delq logbook (cdr logbook-entries))))
        (if logbook-entries
            (setcdr logbook-entries
                    (cons (cons begin new-logbook)
                          (cdr logbook-entries)))
          (nconc entry
		 (list (cons 'logbook (list (cons begin new-logbook))))))))
    (if propagate
        (org-x-propagate entry 'add-logbook-entry (list begin end duration)))
    new-logbook))

(defun org-x-add-whole-logbook-entry (entry logbook
					    &optional no-overwrite propagate)
  (org-x-add-logbook-entry entry (org-x-logbook-begin logbook)
			   (org-x-logbook-end logbook)
			   (org-x-logbook-duration logbook)
			   no-overwrite propagate))

(defun org-x-remove-logbook-entry (entry begin &optional propagate)
  (let* ((logbook-entries (assq 'logbook entry))
         (logbook (assoc begin (cdr logbook-entries))))
    (if (and logbook logbook-entries)
        (setcdr logbook-entries (delq logbook (cdr logbook-entries)))))
  (if propagate
      (org-x-propagate entry 'remove-logbook-entry begin)))

(defun org-x-logbook-setter (logbook-entry symbol data
					   &optional no-overwrite propagate)
  (let ((cell (assq symbol logbook-entry)))
    (if cell
        (unless (and (cdr cell) no-overwrite)
          (setcdr cell data))
      (nconc logbook-entry (list (cons symbol data)))))
  (if propagate
      (org-x-propagate logbook-entry
                       (intern (concat "logbook-set-" (symbol-name symbol)))
                       data)))

(defun org-x-logbook-eraser (logbook-entry symbol
					   &optional no-overwrite propagate)
  (let ((cell (assq symbol logbook-entry)))
    (unless (and (cdr cell) no-overwrite)
      (if cell
          (setcdr logbook-entry (delq cell (cdr logbook-entry))))))
  (if propagate
      ;; jww (2011-08-07): Should I propagate logbook-entry changes?
      (org-x-propagate logbook-entry
                       (intern (concat "logbook-clear-" (symbol-name symbol)))
                       nil)))

(defun org-x-logbook-set-begin (logbook-entry begin
					      &optional no-overwrite propagate)
  (assert (listp begin) nil "Org-X logbook entry begin must be a time value")
  (org-x-logbook-setter logbook-entry 'begin begin no-overwrite propagate))
(defun org-x-logbook-clear-begin (logbook-entry &optional propagate)
  (org-x-logbook-eraser logbook-entry 'logbook-clear-begin propagate))

(defun org-x-logbook-set-end (logbook-entry end
					    &optional no-overwrite propagate)
  (assert (listp end) nil "Org-X logbook entry end must be a time value")
  (org-x-logbook-setter logbook-entry 'end end no-overwrite propagate))
(defun org-x-logbook-clear-end (logbook-entry &optional propagate)
  (org-x-logbook-eraser logbook-entry 'logbook-clear-end propagate))

(defun org-x-logbook-set-duration
  (logbook-entry duration &optional no-overwrite propagate)
  (assert (stringp duration) nil
	  "Org-X logbook entry duration must be a string")
  (org-x-logbook-setter logbook-entry 'duration duration
			no-overwrite propagate))
(defun org-x-logbook-clear-duration (logbook-entry &optional propagate)
  (org-x-logbook-eraser logbook-entry 'logbook-clear-duration propagate))

;;; Entry comparison:

(defun org-x-compare-entries (l r &optional ignore-deletions ignore-changes)
  "Compare two entries, L and R.
Return a list of the operations that would turn L into R.  This last
can be passed to org-x-apply-changes."
  (let (ops)
    (dolist (elem l)
      (let* ((key (car elem))
             (data (assq key r)))
        (cond
         ((eq key 'tags)
          (unless ignore-deletions
            (dolist (tag (cdr elem))
              ;; the log-entry from l is not in r
              (unless (member tag (cdr data))
                (push (list 'remove-tag tag) ops)))))

         ((eq key 'log)
          (dolist (log (cdr elem))
            (let ((data-log (assoc (car log) (cdr data))))
              (if (and data data-log)
                  ;; r has the same log-entry as l, merge them
                  (dolist (log-item (cdr log))
                    (let ((data-log-item (assoc (car log-item)
                                                (cdr data-log))))
                      (if data-log-item
                          ;; r has the same log-entry detail as l,
                          ;; check the value
                          (unless (or ignore-changes
                                      (equal (cdr log-item)
                                             (cdr data-log-item)))
                            (push (list (intern
                                         (concat "log-set-"
                                                 (symbol-name (car log-item))
                                                 "-by-timestamp"))
                                        (car log)
                                        (cdr data-log-item)) ops))
                        ;; the log-entry detail from l is not in r
                        (unless ignore-deletions
                          (push (list (intern
                                       (concat "log-clear-"
                                               (symbol-name (car log-item))
                                               "-by-timestamp"))
                                      (car log)) ops)))))
                ;; the log-entry from l is not in r
                (unless ignore-deletions
                  (push (list 'remove-log-entry (car log)) ops))))))

         ((eq key 'properties)
          (dolist (prop (cdr elem))
            (let ((data-prop (assoc (car prop) (cdr data))))
              (if (and data data-prop)
                  ;; r has the same property as l, check the value
                  (unless (or ignore-changes
                              (equal (cdr elem) (cdr data)))
                    (push (list 'set-property (car prop) (cdr data-prop))
                          ops))
                ;; the property from l is not in r
                (unless ignore-deletions
                  (push (list 'remove-property (car prop)) ops))))))
         (t
          (if data
              ;; r has the same element as l, check the value
              (unless (or ignore-changes
                          (equal (cdr elem) (cdr data)))
                (push (list (intern (concat "set-" (symbol-name key)))
                            (cdr data)) ops))
            ;; r does not have the same element as l
            (unless ignore-deletions
              (push (list (intern (concat "clear-"
                                          (symbol-name key)))) ops)))))))
    (dolist (data r)
      (let* ((key (car data))
             (elem (assq key l)))
        (cond
         ((eq key 'tags)
          (dolist (tag (cdr data))
            (unless (assoc tag (cdr elem))
              ;; r has a tag not in l
              (push (list 'add-tag tag) ops))))

         ((eq key 'log)
          (dolist (log (cdr data))
            (let ((elem-log (assoc (car log) (cdr elem))))
              (if (and elem elem-log)
                  (dolist (log-item (cdr log))
                    (let ((elem-log-item (assoc (car log-item)
                                                (cdr elem-log))))
                      (unless elem-log-item
                        ;; the log-entry detail from l is not in r
                        (push (list (intern
                                     (concat "log-set-"
                                             (symbol-name (car log-item))
                                             "-by-timestamp"))
                                    (car log)
                                    (cdr log-item)) ops))))
                ;; r has a log-entry not in l
                (push (list 'add-log-entry (car log)
                            (org-x-log-body (cdr log))
                            (org-x-log-is-note (cdr log))
                            (org-x-log-to-state (cdr log))
                            (org-x-log-from-state (cdr log)))
                      ops)))))

         ((eq key 'properties)
          (dolist (prop (cdr data))
            (let ((elem-prop (assoc (car prop) (cdr elem))))
              (unless (and elem elem-prop)
                ;; r has a property not in l
                (push (list 'set-property (car prop) (cdr prop)) ops)))))
         (t
          (unless elem
            ;; r has an element not in l
            (push (list (intern (concat "set-" (symbol-name key)))
                        (cdr data)) ops))))))
    ops))

(defun org-x-apply-changes (backend entry changes)
  (mapc (lambda (op)
	  (if backend
	      (org-x-dispatch backend 'apply-changes entry changes)
	   (apply (intern-soft (concat "org-x-" (symbol-name (car op))))
		  entry (cdr op))))
        changes))

;;; Entry updating:

(eval-when-compile
  (autoload 'org-x-parse-entry "ox-org")
  (autoload 'org-x-replace-entry "ox-org"))

(defun org-x-pull (&optional changes-too position backends)
  (interactive)
  (let ((entry (org-x-parse-entry position))
        (here (point))
	all-changes)
    (dolist (backend (or backends (org-x-backends position)))
      (let ((ident (org-x-get-identifier backend entry)))
	(when ident
	  (if (called-interactively-p 'any)
	      (message "Reading data from backend %s..."
		       (org-x-backend-symbol backend)))
	  (let ((changes (org-x-compare-entries
			  entry (org-x-read-entry backend ident)
			  t (not changes-too))))
	    (when changes
	      (org-x-apply-changes nil entry changes)))
	  (if (called-interactively-p 'any)
	      (message "Pulling data from backend %s...done"
		       (org-x-backend-symbol backend))))))
    (when all-changes
      (org-x-replace-entry entry)
      (goto-char here))
    (cons entry all-changes)))

(defun org-x-push (&optional changes-too position backends)
  (interactive)
  (let ((entry (org-x-parse-entry position))
	all-changes)
    (dolist (backend (or backends (org-x-backends position)))
      (let ((ident (org-x-get-identifier backend entry)))
	(when ident
	  (if (called-interactively-p 'any)
	      (message "Pushing data to backend %s..."
		       (org-x-backend-symbol backend)))
	  (let ((changes (org-x-compare-entries
			  (org-x-read-entry backend ident) entry
			  t (not changes-too))))
	    (when changes
	      (org-x-apply-changes backend entry changes)
	      (setq all-changes (cons (cons backend changes)
				      all-changes))))
	  (if (called-interactively-p 'any)
	      (message "Pushing data to backend %s...done"
		       (org-x-backend-symbol backend))))))
    (cons entry all-changes)))

(defun org-x-sync (&optional changes-too position)
  (let ((backends (org-x-backends position)))
    (org-x-pull changes-too position backends)
    (org-x-push changes-too position backends)))

(provide 'org-x)

;; arch-tag:

;;; org-x.el ends here

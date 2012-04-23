;;; org-e-publish.el --- publish related org-mode files as a website
;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

;; Author: David O'Toole <dto@gnu.org>
;; Maintainer: Carsten Dominik <carsten DOT dominik AT gmail DOT com>
;; Keywords: hypermedia, outlines, wp

;; This file is part of GNU Emacs.
;;
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

;;; Commentary:

;; This program allow configurable publishing of related sets of
;; Org mode files as a complete website.
;;
;; org-e-publish.el can do the following:
;;
;; + Publish all one's Org files to HTML or PDF
;; + Upload HTML, images, attachments and other files to a web server
;; + Exclude selected private pages from publishing
;; + Publish a clickable sitemap of pages
;; + Manage local timestamps for publishing only changed files
;; + Accept plugin functions to extend range of publishable content
;;
;; Documentation for publishing is in the manual.

;;; Code:

(eval-when-compile (require 'cl))
(require 'format-spec)

(declare-function org-element-property "org-element" (property element))
(declare-function org-element-map "org-element"
		  (data types fun &optional info first-match))

(declare-function org-export-output-file-name "org-export"
		  (extension &optional subtreep pub-dir))
(declare-function
 org-export-to-file "org-export"
 (backend file &optional subtreep visible-only body-only ext-plist))
(declare-function org-export-get-parent-headline "org-export" (blob info))
(declare-function org-export-get-environment "org-export"
		  (&optional backend subtreep ext-plist))
(declare-function org-export-get-inbuffer-options "org-export"
		  (&optional backend files))



;;; Variables
(defvar org-e-publish-initial-buffer nil
  "The buffer `org-e-publish' has been called from.")

(defvar org-e-publish-temp-files nil
  "Temporary list of files to be published.")

;; Here, so you find the variable right before it's used the first time:
(defvar org-e-publish-cache nil
  "This will cache timestamps and titles for files in publishing projects.
Blocks could hash sha1 values here.")

(defgroup org-e-publish nil
  "Options for publishing a set of Org-mode and related files."
  :tag "Org Publishing"
  :group 'org)

(defcustom org-e-publish-project-alist nil
  "Association list to control publishing behavior.
Each element of the alist is a publishing 'project.'  The CAR of
each element is a string, uniquely identifying the project.  The
CDR of each element is in one of the following forms:

1. A well-formed property list with an even number of elements,
   alternating keys and values, specifying parameters for the
   publishing process.

     \(:property value :property value ... )

2. A meta-project definition, specifying of a list of
   sub-projects:

     \(:components \(\"project-1\" \"project-2\" ...))

When the CDR of an element of org-e-publish-project-alist is in
this second form, the elements of the list after `:components'
are taken to be components of the project, which group together
files requiring different publishing options.  When you publish
such a project with \\[org-e-publish], the components all
publish.

When a property is given a value in
`org-e-publish-project-alist', its setting overrides the value of
the corresponding user variable \(if any) during publishing.
However, options set within a file override everything.

Most properties are optional, but some should always be set:

  `:base-directory'

    Directory containing publishing source files.

  `:base-extension'

    Extension \(without the dot!) of source files.  This can be
    a regular expression.  If not given, \"org\" will be used as
    default extension.

  `:publishing-directory'

    Directory \(possibly remote) where output files will be
    published.

The `:exclude' property may be used to prevent certain files from
being published.  Its value may be a string or regexp matching
file names you don't want to be published.

The `:include' property may be used to include extra files.  Its
value may be a list of filenames to include. The filenames are
considered relative to the base directory.

When both `:include' and `:exclude' properties are given values,
the exclusion step happens first.

One special property controls which back-end function to use for
publishing files in the project.  This can be used to extend the
set of file types publishable by `org-e-publish', as well as the
set of output formats.

  `:publishing-function'

    Function to publish file.  The default is
    `org-e-publish-org-to-ascii', but other values are possible.
    May also be a list of functions, in which case each function
    in the list is invoked in turn.

Another property allows you to insert code that prepares
a project for publishing.  For example, you could call GNU Make
on a certain makefile, to ensure published files are built up to
date.

  `:preparation-function'

    Function to be called before publishing this project.  This
    may also be a list of functions.

  `:completion-function'

    Function to be called after publishing this project.  This
    may also be a list of functions.

Some properties control details of the Org publishing process,
and are equivalent to the corresponding user variables listed in
the right column.  Back-end specific properties may also be
included.  See the back-end documentation for more information.

  :author                       `user-full-name'
  :creator                      `org-export-creator-string'
  :email                        `user-mail-address'
  :exclude-tags                 `org-export-exclude-tags'
  :headline-levels              `org-export-headline-levels'
  :language                     `org-export-default-language'
  :preserve-breaks              `org-export-preserve-breaks'
  :section-numbers              `org-export-with-section-numbers'
  :select-tags                  `org-export-select-tags'
  :time-stamp-file              `org-export-time-stamp-file'
  :with-archived-trees          `org-export-with-archived-trees'
  :with-author                  `org-export-with-author'
  :with-creator                 `org-export-with-creator'
  :with-drawers                 `org-export-with-drawers'
  :with-email                   `org-export-with-email'
  :with-emphasize               `org-export-with-emphasize'
  :with-entities                `org-export-with-entities'
  :with-fixed-width             `org-export-with-fixed-width'
  :with-footnotes               `org-export-with-footnotes'
  :with-priority                `org-export-with-priority'
  :with-special-strings         `org-export-with-special-strings'
  :with-sub-superscript         `org-export-with-sub-superscripts'
  :with-toc                     `org-export-with-toc'
  :with-tables                  `org-export-with-tables'
  :with-tags                    `org-export-with-tags'
  :with-tasks                   `org-export-with-tasks'
  :with-timestamps              `org-export-with-timestamps'
  :with-todo-keywords           `org-export-with-todo-keywords'

The following properties may be used to control publishing of
a site-map of files or summary page for a given project.

  `:auto-sitemap'

    Whether to publish a site-map during
    `org-e-publish-current-project' or `org-e-publish-all'.

  `:sitemap-filename'

    Filename for output of sitemap.  Defaults to \"sitemap.org\".

  `:sitemap-title'

    Title of site-map page.  Defaults to name of file.

  `:sitemap-function'

    Plugin function to use for generation of site-map.  Defaults to
    `org-e-publish-org-sitemap', which generates a plain list of
    links to all files in the project.

  `:sitemap-style'

    Can be `list' \(site-map is just an itemized list of the
    titles of the files involved) or `tree' \(the directory
    structure of the source files is reflected in the site-map).
    Defaults to `tree'.

  `:sitemap-sans-extension'

    Remove extension from site-map's file-names.  Useful to have
    cool URIs \(see http://www.w3.org/Provider/Style/URI).
    Defaults to nil.

If you create a site-map file, adjust the sorting like this:

  `:sitemap-sort-folders'

    Where folders should appear in the site-map.  Set this to
    `first' \(default) or `last' to display folders first or
    last, respectively.  Any other value will mix files and
    folders.

  `:sitemap-sort-files'

    The site map is normally sorted alphabetically.  You can
    change this behaviour setting this to `anti-chronologically',
    `chronologically', or nil.

  `:sitemap-ignore-case'

    Should sorting be case-sensitive?  Default nil.

The following properties control the creation of a concept index.

  `:makeindex'

    Create a concept index.

Other properties affecting publication.

  `:body-only'

    Set this to t to publish only the body of the documents."
  :group 'org-e-publish
  :type 'alist)

(defcustom org-e-publish-use-timestamps-flag t
  "Non-nil means use timestamp checking to publish only changed files.
When nil, do no timestamp checking and always publish all files."
  :group 'org-e-publish
  :type 'boolean)

(defcustom org-e-publish-timestamp-directory
  (convert-standard-filename "~/.org-timestamps/")
  "Name of directory in which to store publishing timestamps."
  :group 'org-e-publish
  :type 'directory)

(defcustom org-e-publish-list-skipped-files t
  "Non-nil means show message about files *not* published."
  :group 'org-e-publish
  :type 'boolean)

(defcustom org-e-publish-sitemap-sort-files 'alphabetically
  "Method to sort files in site-maps.
Possible values are `alphabetically', `chronologically',
`anti-chronologically' and nil.

If `alphabetically', files will be sorted alphabetically.  If
`chronologically', files will be sorted with older modification
time first.  If `anti-chronologically', files will be sorted with
newer modification time first.  nil won't sort files.

You can overwrite this default per project in your
`org-e-publish-project-alist', using `:sitemap-sort-files'."
  :group 'org-e-publish
  :type 'symbol)

(defcustom org-e-publish-sitemap-sort-folders 'first
  "A symbol, denoting if folders are sorted first in sitemaps.
Possible values are `first', `last', and nil.
If `first', folders will be sorted before files.
If `last', folders are sorted to the end after the files.
Any other value will not mix files and folders.

You can overwrite this default per project in your
`org-e-publish-project-alist', using `:sitemap-sort-folders'."
  :group 'org-e-publish
  :type 'symbol)

(defcustom org-e-publish-sitemap-sort-ignore-case nil
  "Non-nil when site-map sorting should ignore case.

You can overwrite this default per project in your
`org-e-publish-project-alist', using `:sitemap-ignore-case'."
  :group 'org-e-publish
  :type 'boolean)

(defcustom org-e-publish-sitemap-date-format "%Y-%m-%d"
  "Format for `format-time-string' which is used to print a date
in the sitemap."
  :group 'org-e-publish
  :type 'string)

(defcustom org-e-publish-sitemap-file-entry-format "%t"
  "Format string for site-map file entry.
You could use brackets to delimit on what part the link will be.

%t is the title.
%a is the author.
%d is the date formatted using `org-e-publish-sitemap-date-format'."
  :group 'org-e-publish
  :type 'string)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timestamp-related functions

(defun org-e-publish-timestamp-filename (filename &optional pub-dir pub-func)
  "Return path to timestamp file for filename FILENAME."
  (setq filename (concat filename "::" (or pub-dir "") "::"
			 (format "%s" (or pub-func ""))))
  (concat "X" (if (fboundp 'sha1) (sha1 filename) (md5 filename))))

(defun org-e-publish-needed-p (filename &optional pub-dir pub-func true-pub-dir)
  "Return t if FILENAME should be published in PUB-DIR using PUB-FUNC.
TRUE-PUB-DIR is where the file will truly end up.  Currently we
are not using this - maybe it can eventually be used to check if
the file is present at the target location, and how old it is.
Right now we cannot do this, because we do not know under what
file name the file will be stored - the publishing function can
still decide about that independently."
  (let ((rtn (if (not org-e-publish-use-timestamps-flag) t
	       (org-e-publish-cache-file-needs-publishing
		filename pub-dir pub-func))))
    (if rtn (message "Publishing file %s using `%s'" filename pub-func)
      (when org-e-publish-list-skipped-files
	(message   "Skipping unmodified file %s" filename)))
    rtn))

(defun org-e-publish-update-timestamp (filename &optional pub-dir pub-func)
  "Update publishing timestamp for file FILENAME.
If there is no timestamp, create one."
  (let ((key (org-e-publish-timestamp-filename filename pub-dir pub-func))
	(stamp (org-e-publish-cache-ctime-of-src filename)))
    (org-e-publish-cache-set key stamp)))

(defun org-e-publish-remove-all-timestamps ()
  "Remove all files in the timestamp directory."
  (let ((dir org-e-publish-timestamp-directory)
	files)
    (when (and (file-exists-p dir) (file-directory-p dir))
      (mapc 'delete-file (directory-files dir 'full "[^.]\\'"))
      (org-e-publish-reset-cache))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting project information out of `org-e-publish-project-alist'

(defun org-e-publish-expand-projects (projects-alist)
  "Expand projects in PROJECTS-ALIST.
This splices all the components into the list."
  (let ((rest projects-alist) rtn p components)
    (while (setq p (pop rest))
      (if (setq components (plist-get (cdr p) :components))
	  (setq rest (append
		      (mapcar (lambda (x) (assoc x org-e-publish-project-alist))
			      components)
		      rest))
	(push p rtn)))
    (nreverse (delete-dups (delq nil rtn)))))

(defvar org-sitemap-sort-files)
(defvar org-sitemap-sort-folders)
(defvar org-sitemap-ignore-case)
(defvar org-sitemap-requested)
(defvar org-sitemap-date-format)
(defvar org-sitemap-file-entry-format)
(defun org-e-publish-compare-directory-files (a b)
  "Predicate for `sort', that sorts folders and files for sitemap."
  (let ((retval t))
    (when (or org-sitemap-sort-files org-sitemap-sort-folders)
      ;; First we sort files:
      (when org-sitemap-sort-files
	(case org-sitemap-sort-files
	  (alphabetically
	   (let* ((adir (file-directory-p a))
		  (aorg (and (string-match "\\.org$" a) (not adir)))
		  (bdir (file-directory-p b))
		  (borg (and (string-match "\\.org$" b) (not bdir)))
		  (A (if aorg (concat (file-name-directory a)
				      (org-e-publish-find-title a)) a))
		  (B (if borg (concat (file-name-directory b)
				      (org-e-publish-find-title b)) b)))
	     (setq retval (if org-sitemap-ignore-case
			      (not (string-lessp (upcase B) (upcase A)))
			    (not (string-lessp B A))))))
	  ((anti-chronologically chronologically)
	   (let* ((adate (org-e-publish-find-date a))
		  (bdate (org-e-publish-find-date b))
		  (A (+ (lsh (car adate) 16) (cadr adate)))
		  (B (+ (lsh (car bdate) 16) (cadr bdate))))
	     (setq retval
		   (if (eq org-sitemap-sort-files 'chronologically) (<= A B)
		     (>= A B)))))))
      ;; Directory-wise wins:
      (when org-sitemap-sort-folders
        ;; a is directory, b not:
        (cond
         ((and (file-directory-p a) (not (file-directory-p b)))
          (setq retval (equal org-sitemap-sort-folders 'first)))
	 ;; a is not a directory, but b is:
         ((and (not (file-directory-p a)) (file-directory-p b))
          (setq retval (equal org-sitemap-sort-folders 'last))))))
    retval))

(defun org-e-publish-get-base-files-1
  (base-dir &optional recurse match skip-file skip-dir)
  "Set `org-e-publish-temp-files' with files from BASE-DIR directory.
If RECURSE is non-nil, check BASE-DIR recursively.  If MATCH is
non-nil, restrict this list to the files matching the regexp
MATCH.  If SKIP-FILE is non-nil, skip file matching the regexp
SKIP-FILE.  If SKIP-DIR is non-nil, don't check directories
matching the regexp SKIP-DIR when recursing through BASE-DIR."
  (mapc (lambda (f)
	  (let ((fd-p (file-directory-p f))
		(fnd (file-name-nondirectory f)))
	    (if (and fd-p recurse
		     (not (string-match "^\\.+$" fnd))
		     (if skip-dir (not (string-match skip-dir fnd)) t))
		(org-e-publish-get-base-files-1
		 f recurse match skip-file skip-dir)
	      (unless (or fd-p ;; this is a directory
			  (and skip-file (string-match skip-file fnd))
			  (not (file-exists-p (file-truename f)))
			  (not (string-match match fnd)))

		(pushnew f org-e-publish-temp-files)))))
	(if org-sitemap-requested
	    (sort (directory-files base-dir t (unless recurse match))
		  'org-e-publish-compare-directory-files)
	  (directory-files base-dir t (unless recurse match)))))

(defun org-e-publish-get-base-files (project &optional exclude-regexp)
  "Return a list of all files in PROJECT.
If EXCLUDE-REGEXP is set, this will be used to filter out
matching filenames."
  (let* ((project-plist (cdr project))
	 (base-dir (file-name-as-directory
		    (plist-get project-plist :base-directory)))
	 (include-list (plist-get project-plist :include))
	 (recurse (plist-get project-plist :recursive))
	 (extension (or (plist-get project-plist :base-extension) "org"))
	 ;; sitemap-... variables are dynamically scoped for
	 ;; org-e-publish-compare-directory-files:
	 (org-sitemap-requested
	  (plist-get project-plist :auto-sitemap))
	 (sitemap-filename
	  (or (plist-get project-plist :sitemap-filename) "sitemap.org"))
	 (org-sitemap-sort-folders
	  (if (plist-member project-plist :sitemap-sort-folders)
	      (plist-get project-plist :sitemap-sort-folders)
	    org-e-publish-sitemap-sort-folders))
	 (org-sitemap-sort-files
	  (cond ((plist-member project-plist :sitemap-sort-files)
		 (plist-get project-plist :sitemap-sort-files))
		;; For backward compatibility:
		((plist-member project-plist :sitemap-alphabetically)
		 (if (plist-get project-plist :sitemap-alphabetically)
		     'alphabetically nil))
		(t org-e-publish-sitemap-sort-files)))
	 (org-sitemap-ignore-case
	  (if (plist-member project-plist :sitemap-ignore-case)
	      (plist-get project-plist :sitemap-ignore-case)
	    org-e-publish-sitemap-sort-ignore-case))
	 (match (if (eq extension 'any) "^[^\\.]"
		  (concat "^[^\\.].*\\.\\(" extension "\\)$"))))
    ;; Make sure `org-sitemap-sort-folders' has an accepted value
    (unless (memq org-sitemap-sort-folders '(first last))
      (setq org-sitemap-sort-folders nil))

    (setq org-e-publish-temp-files nil)
    (if org-sitemap-requested
	(pushnew (expand-file-name (concat base-dir sitemap-filename))
		  org-e-publish-temp-files))
    (org-e-publish-get-base-files-1 base-dir recurse match
				  ;; FIXME distinguish exclude regexp
				  ;; for skip-file and skip-dir?
				  exclude-regexp exclude-regexp)
    (mapc (lambda (f)
	    (pushnew
	     (expand-file-name (concat base-dir f))
	     org-e-publish-temp-files))
	  include-list)
    org-e-publish-temp-files))

(defun org-e-publish-get-project-from-filename (filename &optional up)
  "Return the project that FILENAME belongs to."
  (let* ((filename (expand-file-name filename))
	 project-name)

    (catch 'p-found
      (dolist (prj org-e-publish-project-alist)
	(unless (plist-get (cdr prj) :components)
	  ;; [[info:org:Selecting%20files]] shows how this is supposed to work:
	  (let* ((r (plist-get (cdr prj) :recursive))
		 (b (expand-file-name (file-name-as-directory
				       (plist-get (cdr prj) :base-directory))))
		 (x (or (plist-get (cdr prj) :base-extension) "org"))
		 (e (plist-get (cdr prj) :exclude))
		 (i (plist-get (cdr prj) :include))
		 (xm (concat "^" b (if r ".+" "[^/]+") "\\.\\(" x "\\)$")))
	    (when
		(or (and i
			 (member filename
				 (mapcar (lambda (file)
					   (expand-file-name file b))
					 i)))
		    (and (not (and e (string-match e filename)))
			 (string-match xm filename)))
	      (setq project-name (car prj))
	      (throw 'p-found project-name))))))
    (when up
      (dolist (prj org-e-publish-project-alist)
	(if (member project-name (plist-get (cdr prj) :components))
	    (setq project-name (car prj)))))
    (assoc project-name org-e-publish-project-alist)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pluggable publishing back-end functions

(defun org-e-publish-org-to (backend filename extension plist pub-dir)
  "Publish an Org file to a specified back-end.

BACKEND is a symbol representing the back-end used for
transcoding.  FILENAME is the filename of the Org file to be
published.  EXTENSION is the extension used for the output
string, with the leading dot.  PLIST is the property list for the
given project.  PUB-DIR is the publishing directory.

Return output file name."
  (unless (file-exists-p pub-dir) (make-directory pub-dir t))
  ;; Check if a buffer visiting FILENAME is already open.
  (let* ((visitingp (find-buffer-visiting filename))
	 (work-buffer (or visitingp (find-file-noselect filename))))
    (prog1 (with-current-buffer work-buffer
	     (let ((output-file
		    (org-export-output-file-name extension nil pub-dir))
		   (body-p (plist-get plist :body-only)))
	       (org-export-to-file
		backend output-file nil nil body-p
		;; Install `org-e-publish-collect-index' in parse tree
		;; filters.  It isn't dependent on `:makeindex', since
		;; we want to keep it up-to-date in cache anyway.
		(org-combine-plists
		 plist `(:filter-parse-tree
			 (org-e-publish-collect-index
			  ,@(plist-get plist :filter-parse-tree)))))))
      ;; Remove opened buffer in the process.
      (unless visitingp (kill-buffer work-buffer)))))

(defvar project-plist)
(defun org-e-publish-org-to-latex (plist filename pub-dir)
  "Publish an Org file to LaTeX.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-e-publish-org-to 'e-latex filename ".tex" plist pub-dir))

(defun org-e-publish-org-to-pdf (plist filename pub-dir)
  "Publish an Org file to PDF \(via LaTeX).

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-e-latex-compile
   (org-e-publish-org-to 'e-latex filename ".tex" plist pub-dir)))

(defun org-e-publish-org-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-e-publish-org-to 'e-html filename "html" plist pub-dir))

;; TODO: Not implemented yet.
;; (defun org-e-publish-org-to-org (plist filename pub-dir)
;;   "Publish an org file to HTML.
;;
;; FILENAME is the filename of the Org file to be published.  PLIST
;; is the property list for the given project.  PUB-DIR is the
;; publishing directory.
;;
;; Return output file name."
;;   (org-e-publish-org-to "org" plist filename pub-dir))

(defun org-e-publish-org-to-ascii (plist filename pub-dir)
  "Publish an Org file to ASCII.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-e-publish-org-to
   'e-ascii filename ".txt" `(:ascii-charset ascii ,@plist) pub-dir))

(defun org-e-publish-org-to-latin1 (plist filename pub-dir)
  "Publish an Org file to Latin-1.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-e-publish-org-to
   'e-ascii filename ".txt" `(:ascii-charset latin1 ,@plist) pub-dir))

(defun org-e-publish-org-to-utf8 (plist filename pub-dir)
  "Publish an org file to UTF-8.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-e-publish-org-to
   'e-ascii filename ".txt" `(:ascii-charset utf-8 ,@plist) pub-dir))

(defun org-e-publish-attachment (plist filename pub-dir)
  "Publish a file with no transformation of any kind.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (unless (file-directory-p pub-dir)
    (make-directory pub-dir t))
  (or (equal (expand-file-name (file-name-directory filename))
	     (file-name-as-directory (expand-file-name pub-dir)))
      (copy-file filename
		 (expand-file-name (file-name-nondirectory filename) pub-dir)
		 t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Publishing files, sets of files, and indices

(defun org-e-publish-file (filename &optional project no-cache)
  "Publish file FILENAME from PROJECT.
If NO-CACHE is not nil, do not initialize org-e-publish-cache and
write it to disk.  This is needed, since this function is used to
publish single files, when entire projects are published.
See `org-e-publish-projects'."
  (let* ((project
	  (or project
	      (or (org-e-publish-get-project-from-filename filename)
		  (error "File %s not part of any known project"
			 (abbreviate-file-name filename)))))
	 (project-plist (cdr project))
	 (ftname (expand-file-name filename))
	 (publishing-function
	  (or (plist-get project-plist :publishing-function)
	      'org-e-publish-org-to-ascii))
	 (base-dir
	  (file-name-as-directory
	   (expand-file-name
	    (or (plist-get project-plist :base-directory)
		(error "Project %s does not have :base-directory defined"
		       (car project))))))
	 (pub-dir
	  (file-name-as-directory
	   (file-truename
	    (or (eval (plist-get project-plist :publishing-directory))
		(error "Project %s does not have :publishing-directory defined"
		       (car project))))))
	 tmp-pub-dir)

    (unless no-cache (org-e-publish-initialize-cache (car project)))

    (setq tmp-pub-dir
	  (file-name-directory
	   (concat pub-dir
		   (and (string-match (regexp-quote base-dir) ftname)
			(substring ftname (match-end 0))))))
    (if (listp publishing-function)
	;; allow chain of publishing functions
	(mapc (lambda (f)
		(when (org-e-publish-needed-p filename pub-dir f tmp-pub-dir)
		  (funcall f project-plist filename tmp-pub-dir)
		  (org-e-publish-update-timestamp filename pub-dir f)))
	      publishing-function)
      (when (org-e-publish-needed-p filename pub-dir publishing-function
				  tmp-pub-dir)
	(funcall publishing-function project-plist filename tmp-pub-dir)
	(org-e-publish-update-timestamp
	 filename pub-dir publishing-function)))
    (unless no-cache (org-e-publish-write-cache-file))))

(defun org-e-publish-projects (projects)
  "Publish all files belonging to the PROJECTS alist.
If `:auto-sitemap' is set, publish the sitemap too.  If
`:makeindex' is set, also produce a file theindex.org."
  (mapc
   (lambda (project)
     ;; Each project uses its own cache file:
     (org-e-publish-initialize-cache (car project))
     (let* ((project-plist (cdr project))
	    (exclude-regexp (plist-get project-plist :exclude))
	    (sitemap-p (plist-get project-plist :auto-sitemap))
	    (sitemap-filename (or (plist-get project-plist :sitemap-filename)
				  "sitemap.org"))
	    (sitemap-function (or (plist-get project-plist :sitemap-function)
				  'org-e-publish-org-sitemap))
	    (org-sitemap-date-format
	     (or (plist-get project-plist :sitemap-date-format)
		 org-e-publish-sitemap-date-format))
	    (org-sitemap-file-entry-format
	     (or (plist-get project-plist :sitemap-file-entry-format)
		 org-e-publish-sitemap-file-entry-format))
	    (preparation-function
	     (plist-get project-plist :preparation-function))
	    (completion-function (plist-get project-plist :completion-function))
	    (files (org-e-publish-get-base-files project exclude-regexp)) file)
       (when preparation-function (run-hooks 'preparation-function))
       (if sitemap-p (funcall sitemap-function project sitemap-filename))
       (dolist (file files) (org-e-publish-file file project t))
       (when (plist-get project-plist :makeindex)
	 (org-e-publish-index-generate-theindex
	  project (plist-get project-plist :base-directory))
	 (org-e-publish-file
	  (expand-file-name
	   "theindex.org" (plist-get project-plist :base-directory))
	  project t))
       (when completion-function (run-hooks 'completion-function))
       (org-e-publish-write-cache-file)))
   (org-e-publish-expand-projects projects)))

(defun org-e-publish-org-sitemap (project &optional sitemap-filename)
  "Create a sitemap of pages in set defined by PROJECT.
Optionally set the filename of the sitemap with SITEMAP-FILENAME.
Default for SITEMAP-FILENAME is 'sitemap.org'."
  (let* ((project-plist (cdr project))
	 (dir (file-name-as-directory
	       (plist-get project-plist :base-directory)))
	 (localdir (file-name-directory dir))
	 (indent-str (make-string 2 ?\ ))
	 (exclude-regexp (plist-get project-plist :exclude))
	 (files (nreverse
		 (org-e-publish-get-base-files project exclude-regexp)))
	 (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
	 (sitemap-title (or (plist-get project-plist :sitemap-title)
			  (concat "Sitemap for project " (car project))))
	 (sitemap-style (or (plist-get project-plist :sitemap-style)
			    'tree))
	 (sitemap-sans-extension
	  (plist-get project-plist :sitemap-sans-extension))
	 (visiting (find-buffer-visiting sitemap-filename))
	 (ifn (file-name-nondirectory sitemap-filename))
	 file sitemap-buffer)
    (with-current-buffer (setq sitemap-buffer
			       (or visiting (find-file sitemap-filename)))
      (erase-buffer)
      (insert (concat "#+TITLE: " sitemap-title "\n\n"))
      (while (setq file (pop files))
	(let ((fn (file-name-nondirectory file))
	      (link (file-relative-name file dir))
	      (oldlocal localdir))
	  (when sitemap-sans-extension
	    (setq link (file-name-sans-extension link)))
	  ;; sitemap shouldn't list itself
	  (unless (equal (file-truename sitemap-filename)
			 (file-truename file))
	    (if (eq sitemap-style 'list)
		(message "Generating list-style sitemap for %s" sitemap-title)
	      (message "Generating tree-style sitemap for %s" sitemap-title)
	      (setq localdir (concat (file-name-as-directory dir)
				     (file-name-directory link)))
	      (unless (string= localdir oldlocal)
		(if (string= localdir dir)
		    (setq indent-str (make-string 2 ?\ ))
		  (let ((subdirs
			 (split-string
			  (directory-file-name
			   (file-name-directory
			    (file-relative-name localdir dir))) "/"))
			(subdir "")
			(old-subdirs (split-string
				      (file-relative-name oldlocal dir) "/")))
		    (setq indent-str (make-string 2 ?\ ))
		    (while (string= (car old-subdirs) (car subdirs))
		      (setq indent-str (concat indent-str (make-string 2 ?\ )))
		      (pop old-subdirs)
		      (pop subdirs))
		    (dolist (d subdirs)
		      (setq subdir (concat subdir d "/"))
		      (insert (concat indent-str " + " d "\n"))
		      (setq indent-str (make-string
					(+ (length indent-str) 2) ?\ )))))))
	    ;; This is common to 'flat and 'tree
	    (let ((entry
		   (org-e-publish-format-file-entry
		    org-sitemap-file-entry-format file project-plist))
		  (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
	      (cond ((string-match-p regexp entry)
		     (string-match regexp entry)
		     (insert (concat indent-str " + " (match-string 1 entry)
				     "[[file:" link "]["
				     (match-string 2 entry)
				     "]]" (match-string 3 entry) "\n")))
		    (t
		     (insert (concat indent-str " + [[file:" link "]["
				     entry
				     "]]\n"))))))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

(defun org-e-publish-format-file-entry (fmt file project-plist)
  (format-spec fmt
	     `((?t . ,(org-e-publish-find-title file t))
	       (?d . ,(format-time-string org-sitemap-date-format
					  (org-e-publish-find-date file)))
	       (?a . ,(or (plist-get project-plist :author) user-full-name)))))

(defun org-e-publish-find-title (file &optional reset)
  "Find the title of FILE in project."
  (or
   (and (not reset) (org-e-publish-cache-get-file-property file :title nil t))
   (let* ((visiting (find-buffer-visiting file))
	  (buffer (or visiting (find-file-noselect file)))
	  title)
     (with-current-buffer buffer
       (org-mode)
       (setq title
	     (or (plist-get (org-export-get-environment) :title)
		 (file-name-nondirectory (file-name-sans-extension file)))))
     (unless visiting (kill-buffer buffer))
     (org-e-publish-cache-set-file-property file :title title)
     title)))

(defun org-e-publish-find-date (file)
  "Find the date of FILE in project.
If FILE provides a #+date keyword use it else use the file
system's modification time.

It returns time in `current-time' format."
  (let* ((visiting (find-buffer-visiting file))
	 (file-buf (or visiting (find-file-noselect file nil)))
	 (date (plist-get
		(with-current-buffer file-buf
		  (org-mode)
		  (org-export-get-inbuffer-options))
		:date)))
    (unless visiting (kill-buffer file-buf))
    (if date (org-time-string-to-time date)
      (when (file-exists-p file)
	(nth 5 (file-attributes file))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive publishing functions

;;;###autoload
(defalias 'org-e-publish-project 'org-e-publish)

;;;###autoload
(defun org-e-publish (project &optional force)
  "Publish PROJECT."
  (interactive
   (list
    (assoc (org-icompleting-read
	    "Publish project: "
	    org-e-publish-project-alist nil t)
	   org-e-publish-project-alist)
    current-prefix-arg))
  (setq org-e-publish-initial-buffer (current-buffer))
  (save-window-excursion
    (let* ((org-e-publish-use-timestamps-flag
	    (if force nil org-e-publish-use-timestamps-flag)))
      (org-e-publish-projects
       (if (stringp project)
	   ;; If this function is called in batch mode, project is
	   ;; still a string here.
	   (list (assoc project org-e-publish-project-alist))
	 (list project))))))

;;;###autoload
(defun org-e-publish-all (&optional force)
  "Publish all projects.
With prefix argument, remove all files in the timestamp
directory and force publishing all files."
  (interactive "P")
  (when force (org-e-publish-remove-all-timestamps))
  (save-window-excursion
    (let ((org-e-publish-use-timestamps-flag
	   (if force nil org-e-publish-use-timestamps-flag)))
      (org-e-publish-projects org-e-publish-project-alist))))


;;;###autoload
(defun org-e-publish-current-file (&optional force)
  "Publish the current file.
With prefix argument, force publish the file."
  (interactive "P")
  (save-window-excursion
    (let ((org-e-publish-use-timestamps-flag
	   (if force nil org-e-publish-use-timestamps-flag)))
      (org-e-publish-file (buffer-file-name (buffer-base-buffer))))))

;;;###autoload
(defun org-e-publish-current-project (&optional force)
  "Publish the project associated with the current file.
With a prefix argument, force publishing of all files in
the project."
  (interactive "P")
  (save-window-excursion
    (let ((project (org-e-publish-get-project-from-filename
		    (buffer-file-name (buffer-base-buffer)) 'up))
	  (org-e-publish-use-timestamps-flag
	   (if force nil org-e-publish-use-timestamps-flag)))
      (if project (org-e-publish project)
	(error "File %s is not part of any known project"
	       (buffer-file-name (buffer-base-buffer)))))))



;;; Index generation

(defun org-e-publish-collect-index (tree backend info)
  "Update index for a file with TREE in cache.

BACKEND is the back-end being used for transcoding.  INFO is
a plist containing publishing options.

The index relative to current file is stored as an alist.  An
association has the following shape: \(TERM FILE-NAME PARENT),
where TERM is the indexed term, as a string, FILE-NAME is the
original full path of the file where the term in encountered, and
PARENT is the headline element containing the original index
keyword."
  (org-e-publish-cache-set-file-property
   (plist-get info :input-file) :index
   (delete-dups
    (org-element-map
     tree 'keyword
     (lambda (k)
       (when (string= (downcase (org-element-property :key k))
		      "index")
	 (let ((index (org-element-property :value k))
	       (parent (org-export-get-parent-headline k info)))
	   (list index (plist-get info :input-file) parent))))
     info)))
  ;; Return parse-tree to avoid altering output.
  tree)

(defun org-e-publish-index-generate-theindex (project directory)
  "Retrieve full index from cache and build \"theindex.org\".
PROJECT is the project the index relates to.  DIRECTORY is the
publishing directory."
  (let ((all-files (org-e-publish-get-base-files
		    project (plist-get (cdr project) :exclude)))
	full-index)
    ;; Compile full index.
    (mapc
     (lambda (file)
       (let ((index (org-e-publish-cache-get-file-property file :index)))
	 (dolist (term index)
	   (unless (member term full-index) (push term full-index)))))
     all-files)
    ;; Sort it alphabetically.
    (setq full-index
	  (sort full-index (lambda (a b) (string< (downcase (car a))
					     (downcase (car b))))))
    ;; Fill "theindex.org".
    (with-temp-buffer
      (insert "#+TITLE: Index\n#+OPTIONS: num:nil author:nil\n")
      (let ((current-letter nil) (last-entry nil))
	(dolist (idx full-index)
	  (let* ((entry (org-split-string (car idx) "!"))
		 (letter (upcase (substring (car entry) 0 1)))
		 ;; Transform file into a path relative to publishing
		 ;; directory.
		 (file (file-relative-name
			(nth 1 idx)
			(plist-get (cdr project) :base-directory))))
	    ;; Check if another letter has to be inserted.
	    (unless (string= letter current-letter)
	      (insert (format "* %s\n" letter)))
	    ;; Compute the first difference between last entry and
	    ;; current one: it tells the level at which new items
	    ;; should be added.
	    (let* ((rank (loop for n from 0 to (length entry)
			       unless (equal (nth n entry) (nth n last-entry))
			       return n))
		   (len (length (nthcdr rank entry))))
	      ;; For each term after the first difference, create
	      ;; a new sub-list with the term as body.  Moreover,
	      ;; linkify the last term.
	      (dotimes (n len)
		(insert
		 (concat
		  (make-string (* (+ rank n) 2) ? ) "  - "
		  (if (not (= (1- len) n)) (nth (+ rank n) entry)
		    ;; Last term: Link it to TARGET, if possible.
		    (let ((target (nth 2 idx)))
		      (format
		       "[[%s][%s]]"
		       ;; Destination.
		       (cond
			((not target) (format "file:%s" file))
			((let ((id (org-element-property :id target)))
			   (and id (format "id:%s" id))))
			((let ((id (org-element-property :custom-id target)))
			   (and id (format "file:%s::#%s" file id))))
			(t (format "file:%s::*%s" file
				   (org-element-property :raw-value target))))
		       ;; Description.
		       (car (last entry)))))
		  "\n"))))
	    (setq current-letter letter last-entry entry))))
      ;; Write index.
      (write-file (expand-file-name "theindex.org" directory)))))



;;; Caching functions

(defun org-e-publish-write-cache-file (&optional free-cache)
  "Write `org-e-publish-cache' to file.
If FREE-CACHE, empty the cache."
  (unless org-e-publish-cache
    (error "`org-e-publish-write-cache-file' called, but no cache present"))

  (let ((cache-file (org-e-publish-cache-get ":cache-file:")))
    (unless cache-file
      (error "Cannot find cache-file name in `org-e-publish-write-cache-file'"))
    (with-temp-file cache-file
      (let (print-level print-length)
	(insert "(setq org-e-publish-cache (make-hash-table :test 'equal :weakness nil :size 100))\n")
	(maphash (lambda (k v)
		   (insert
		    (format (concat "(puthash %S "
				    (if (or (listp v) (symbolp v))
					"'" "")
				    "%S org-e-publish-cache)\n") k v)))
		 org-e-publish-cache)))
    (when free-cache (org-e-publish-reset-cache))))

(defun org-e-publish-initialize-cache (project-name)
  "Initialize the projects cache if not initialized yet and return it."

  (unless project-name
    (error "%s%s" "Cannot initialize `org-e-publish-cache' without projects name"
	   " in `org-e-publish-initialize-cache'"))

  (unless (file-exists-p org-e-publish-timestamp-directory)
    (make-directory org-e-publish-timestamp-directory t))
  (unless (file-directory-p org-e-publish-timestamp-directory)
    (error "Org publish timestamp: %s is not a directory"
	   org-e-publish-timestamp-directory))

  (unless (and org-e-publish-cache
	       (string= (org-e-publish-cache-get ":project:") project-name))
    (let* ((cache-file
	    (concat
	     (expand-file-name org-e-publish-timestamp-directory)
	     project-name ".cache"))
	   (cexists (file-exists-p cache-file)))

      (when org-e-publish-cache (org-e-publish-reset-cache))

      (if cexists (load-file cache-file)
	(setq org-e-publish-cache
	      (make-hash-table :test 'equal :weakness nil :size 100))
	(org-e-publish-cache-set ":project:" project-name)
	(org-e-publish-cache-set ":cache-file:" cache-file))
      (unless cexists (org-e-publish-write-cache-file nil))))
  org-e-publish-cache)

(defun org-e-publish-reset-cache ()
  "Empty org-e-publish-cache and reset it nil."
  (message "%s" "Resetting org-e-publish-cache")
  (when (hash-table-p org-e-publish-cache)
    (clrhash org-e-publish-cache))
  (setq org-e-publish-cache nil))

(defun org-e-publish-cache-file-needs-publishing
  (filename &optional pub-dir pub-func)
  "Check the timestamp of the last publishing of FILENAME.
Return `t', if the file needs publishing.  The function also
checks if any included files have been more recently published,
so that the file including them will be republished as well."
  (unless org-e-publish-cache
    (error
     "`org-e-publish-cache-file-needs-publishing' called, but no cache present"))
  (let* ((key (org-e-publish-timestamp-filename filename pub-dir pub-func))
	 (pstamp (org-e-publish-cache-get key))
	 (visiting (find-buffer-visiting filename))
	 included-files-ctime buf)

    (when (equal (file-name-extension filename) "org")
      (setq buf (find-file (expand-file-name filename)))
      (with-current-buffer buf
	(goto-char (point-min))
	(while (re-search-forward
		"^#\\+INCLUDE:[ \t]+\"?\\([^ \t\n\r\"]*\\)\"?[ \t]*.*$" nil t)
	  (let* ((included-file (expand-file-name (match-string 1))))
	    (add-to-list 'included-files-ctime
			 (org-e-publish-cache-ctime-of-src included-file) t))))
      ;; FIXME: don't kill current buffer.
      (unless visiting (kill-buffer buf)))
    (if (null pstamp)
	t
      (let ((ctime (org-e-publish-cache-ctime-of-src filename)))
	(or (< pstamp ctime)
	    (when included-files-ctime
	      (not (null (delq nil (mapcar (lambda(ct) (< ctime ct))
					   included-files-ctime))))))))))

(defun org-e-publish-cache-set-file-property
  (filename property value &optional project-name)
  "Set the VALUE for a PROPERTY of file FILENAME in publishing cache to VALUE.
Use cache file of PROJECT-NAME.  If the entry does not exist, it
will be created.  Return VALUE."
  ;; Evtl. load the requested cache file:
  (if project-name (org-e-publish-initialize-cache project-name))
  (let ((pl (org-e-publish-cache-get filename)))
    (if pl (progn (plist-put pl property value) value)
      (org-e-publish-cache-get-file-property
       filename property value nil project-name))))

(defun org-e-publish-cache-get-file-property
  (filename property &optional default no-create project-name)
  "Return the value for a PROPERTY of file FILENAME in publishing cache.
Use cache file of PROJECT-NAME. Return the value of that PROPERTY
or DEFAULT, if the value does not yet exist.  If the entry will
be created, unless NO-CREATE is not nil."
  ;; Evtl. load the requested cache file:
  (if project-name (org-e-publish-initialize-cache project-name))
  (let ((pl (org-e-publish-cache-get filename)) retval)
    (if pl
	(if (plist-member pl property)
	    (setq retval (plist-get pl property))
	  (setq retval default))
      ;; no pl yet:
      (unless no-create
	(org-e-publish-cache-set filename (list property default)))
      (setq retval default))
    retval))

(defun org-e-publish-cache-get (key)
  "Return the value stored in `org-e-publish-cache' for key KEY.
Returns nil, if no value or nil is found, or the cache does not
exist."
  (unless org-e-publish-cache
    (error "`org-e-publish-cache-get' called, but no cache present"))
  (gethash key org-e-publish-cache))

(defun org-e-publish-cache-set (key value)
  "Store KEY VALUE pair in `org-e-publish-cache'.
Returns value on success, else nil."
  (unless org-e-publish-cache
    (error "`org-e-publish-cache-set' called, but no cache present"))
  (puthash key value org-e-publish-cache))

(defun org-e-publish-cache-ctime-of-src (filename)
  "Get the FILENAME ctime as an integer."
  (let* ((symlink-maybe (or (file-symlink-p filename) filename))
	 (src-attr
	  (file-attributes
	   (if (file-name-absolute-p symlink-maybe) symlink-maybe
	     (expand-file-name symlink-maybe (file-name-directory filename))))))
    (+ (lsh (car (nth 5 src-attr)) 16)
       (cadr (nth 5 src-attr)))))


(provide 'org-e-publish)

;;; org-e-publish.el ends here

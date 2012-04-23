;;; org-e-latex.el --- LaTeX Back-End For Org Export Engine

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a LaTeX back-end for Org generic exporter.

;; To test it, run
;;
;;   M-: (org-export-to-buffer 'e-latex "*Test e-LaTeX*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the LaTeX
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.

;; It introduces three new buffer keywords: "LATEX_CLASS",
;; "LATEX_CLASS_OPTIONS" and "LATEX_HEADER".

;;; Code:

(eval-when-compile (require 'cl))

(defvar org-export-latex-default-packages-alist)
(defvar org-export-latex-packages-alist)

(declare-function org-element-property "org-element" (property element))
(declare-function org-element-normalize-string "org-element" (s))
(declare-function org-element-parse-secondary-string
		  "org-element" (string restriction &optional buffer))
(defvar org-element-string-restrictions)
(defvar org-element-object-restrictions)

(declare-function org-export-clean-table "org-export" (table specialp))
(declare-function org-export-data "org-export" (data backend info))
(declare-function org-export-directory "org-export" (type plist))
(declare-function org-export-expand-macro "org-export" (macro info))
(declare-function org-export-first-sibling-p "org-export" (headline info))
(declare-function org-export-footnote-first-reference-p "org-export"
		  (footnote-reference info))
(declare-function org-export-format-code "org-export"
		  (code fun &optional num-lines ref-alist))
(declare-function org-export-format-code-default "org-export" (element info))
(declare-function org-export-get-coderef-format "org-export" (path desc))
(declare-function org-export-get-footnote-definition "org-export"
		  (footnote-reference info))
(declare-function org-export-get-footnote-number "org-export" (footnote info))
(declare-function org-export-get-previous-element "org-export" (blob info))
(declare-function org-export-get-relative-level "org-export" (headline info))
(declare-function org-export-unravel-code "org-export" (element))
(declare-function org-export-included-file "org-export" (keyword backend info))
(declare-function org-export-inline-image-p "org-export"
		  (link &optional extensions))
(declare-function org-export-last-sibling-p "org-export" (headline info))
(declare-function org-export-low-level-p "org-export" (headline info))
(declare-function org-export-output-file-name
		  "org-export" (extension &optional subtreep pub-dir))
(declare-function org-export-resolve-coderef "org-export" (ref info))
(declare-function org-export-resolve-fuzzy-link "org-export" (link info))
(declare-function org-export-secondary-string "org-export"
		  (secondary backend info))
(declare-function org-export-solidify-link-text "org-export" (s))
(declare-function org-export-table-format-info "org-export" (table))
(declare-function
 org-export-to-buffer "org-export"
 (backend buffer &optional subtreep visible-only body-only ext-plist))
(declare-function
 org-export-to-file "org-export"
 (backend file &optional subtreep visible-only body-only ext-plist))



;;; Internal Variables

(defconst org-e-latex-option-alist
  '((:date "DATE" nil org-e-latex-date-format t)
    (:latex-class "LATEX_CLASS" nil org-e-latex-default-class t)
    (:latex-class-options "LATEX_CLASS_OPTIONS" nil nil t)
    (:latex-header-extra "LATEX_HEADER" nil nil newline))
  "Alist between LaTeX export properties and ways to set them.
See `org-export-option-alist' for more information on the
structure of the value.")



;;; User Configurable Variables

(defgroup org-export-e-latex nil
  "Options for exporting Org mode files to LaTeX."
  :tag "Org Export LaTeX"
  :group 'org-export)


;;;; Preamble

(defcustom org-e-latex-default-class "article"
  "The default LaTeX class."
  :group 'org-export-e-latex
  :type '(string :tag "LaTeX class"))

(defcustom org-e-latex-classes
  '(("article"
     "\\documentclass[11pt]{article}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ("report"
     "\\documentclass[11pt]{report}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ("book"
     "\\documentclass[11pt]{book}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  "Alist of LaTeX classes and associated header and structure.
If #+LaTeX_CLASS is set in the buffer, use its value and the
associated information.  Here is the structure of each cell:

  \(class-name
    header-string
    \(numbered-section . unnumbered-section\)
    ...\)

The header string
-----------------

The HEADER-STRING is the header that will be inserted into the
LaTeX file.  It should contain the \\documentclass macro, and
anything else that is needed for this setup.  To this header, the
following commands will be added:

- Calls to \\usepackage for all packages mentioned in the
  variables `org-export-latex-default-packages-alist' and
  `org-export-latex-packages-alist'.  Thus, your header
  definitions should avoid to also request these packages.

- Lines specified via \"#+LaTeX_HEADER:\"

If you need more control about the sequence in which the header
is built up, or if you want to exclude one of these building
blocks for a particular class, you can use the following
macro-like placeholders.

 [DEFAULT-PACKAGES]      \\usepackage statements for default packages
 [NO-DEFAULT-PACKAGES]   do not include any of the default packages
 [PACKAGES]              \\usepackage statements for packages
 [NO-PACKAGES]           do not include the packages
 [EXTRA]                 the stuff from #+LaTeX_HEADER
 [NO-EXTRA]              do not include #+LaTeX_HEADER stuff
 [BEAMER-HEADER-EXTRA]   the beamer extra headers

So a header like

  \\documentclass{article}
  [NO-DEFAULT-PACKAGES]
  [EXTRA]
  \\providecommand{\\alert}[1]{\\textbf{#1}}
  [PACKAGES]

will omit the default packages, and will include the
#+LaTeX_HEADER lines, then have a call to \\providecommand, and
then place \\usepackage commands based on the content of
`org-export-latex-packages-alist'.

If your header, `org-export-latex-default-packages-alist' or
`org-export-latex-packages-alist' inserts
\"\\usepackage[AUTO]{inputenc}\", AUTO will automatically be
replaced with a coding system derived from
`buffer-file-coding-system'.  See also the variable
`org-e-latex-inputenc-alist' for a way to influence this
mechanism.

The sectioning structure
------------------------

The sectioning structure of the class is given by the elements
following the header string.  For each sectioning level, a number
of strings is specified.  A %s formatter is mandatory in each
section string and will be replaced by the title of the section.

Instead of a cons cell \(numbered . unnumbered\), you can also
provide a list of 2 or 4 elements,

  \(numbered-open numbered-close\)

or

  \(numbered-open numbered-close unnumbered-open unnumbered-close\)

providing opening and closing strings for a LaTeX environment
that should represent the document section.  The opening clause
should have a %s to represent the section title.

Instead of a list of sectioning commands, you can also specify
a function name.  That function will be called with two
parameters, the \(reduced) level of the headline, and a predicate
non-nil when the headline should be numbered.  It must return
a format string in which the section title will be added."
  :group 'org-export-e-latex
  :type '(repeat
	  (list (string :tag "LaTeX class")
		(string :tag "LaTeX header")
		(repeat :tag "Levels" :inline t
			(choice
			 (cons :tag "Heading"
			       (string :tag "  numbered")
			       (string :tag "unnumbered"))
			 (list :tag "Environment"
			       (string :tag "Opening   (numbered)")
			       (string :tag "Closing   (numbered)")
			       (string :tag "Opening (unnumbered)")
			       (string :tag "Closing (unnumbered)"))
			 (function :tag "Hook computing sectioning"))))))

(defcustom org-e-latex-inputenc-alist nil
  "Alist of inputenc coding system names, and what should really be used.
For example, adding an entry

      (\"utf8\" . \"utf8x\")

will cause \\usepackage[utf8x]{inputenc} to be used for buffers that
are written as utf8 files."
  :group 'org-export-e-latex
  :type '(repeat
	  (cons
	   (string :tag "Derived from buffer")
	   (string :tag "Use this instead"))))

(defcustom org-e-latex-date-format
  "\\today"
  "Format string for \\date{...}."
  :group 'org-export-e-latex
  :type 'boolean)

(defcustom org-e-latex-title-command "\\maketitle"
  "The command used to insert the title just after \\begin{document}.
If this string contains the formatting specification \"%s\" then
it will be used as a formatting string, passing the title as an
argument."
  :group 'org-export-e-latex
  :type 'string)


;;;; Headline

(defcustom org-e-latex-format-headline-function nil
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword \(string or nil\).
TODO-TYPE the type of todo \(symbol: `todo', `done', nil\)
PRIORITY  the priority of the headline \(integer or nil\)
TEXT      the main headline text \(string\).
TAGS      the tags string, separated with colons \(string or nil\).

The function result will be used in the section format string.

As an example, one could set the variable to the following, in
order to reproduce the default set-up:

\(defun org-e-latex-format-headline \(todo todo-type priority text tags\)
  \"Default format function for an headline.\"
  \(concat \(when todo
            \(format \"\\\\textbf{\\\\textsc{\\\\textsf{%s}}} \" todo\)\)
	  \(when priority
            \(format \"\\\\framebox{\\\\#%c} \" priority\)\)
	  text
	  \(when tags \(format \"\\\\hfill{}\\\\textsc{%s}\" tags\)\)\)\)"
  :group 'org-export-e-latex
  :type 'function)


;;;; Emphasis

(defcustom org-e-latex-emphasis-alist
  '(("*" . "\\textbf{%s}")
    ("/" . "\\emph{%s}")
    ("_" . "\\underline{%s}")
    ("+" . "\\st{%s}")
    ("=" . protectedtexttt)
    ("~" . verb))
  "Alist of LaTeX expressions to convert emphasis fontifiers.

The key is the character used as a marker for fontification.  The
value is a formatting string to wrap fontified text with.

Value can also be set to the following symbols: `verb' and
`protectedtexttt'.  For the former, Org will use \"\\verb\" to
create a format string and select a delimiter character that
isn't in the string.  For the latter, Org will use \"\\texttt\"
to typeset and try to protect special characters."
  :group 'org-export-e-latex
  :type 'alist)


;;;; Footnotes

(defcustom org-e-latex-footnote-separator "\\textsuperscript{,}\\,"
  "Text used to separate footnotes."
  :group 'org-export-e-latex
  :type 'string)


;;;; Time-stamps

(defcustom org-e-latex-active-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to active time-stamps."
  :group 'org-export-e-latex
  :type 'string)

(defcustom org-e-latex-inactive-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to inactive time-stamps."
  :group 'org-export-e-latex
  :type 'string)

(defcustom org-e-latex-diary-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to diary time-stamps."
  :group 'org-export-e-latex
  :type 'string)


;;;; Links

(defcustom org-e-latex-image-default-option "width=.9\\linewidth"
  "Default option for images."
  :group 'org-export-e-latex
  :type 'string)

(defcustom org-e-latex-default-figure-position "htb"
  "Default position for latex figures."
  :group 'org-export-e-latex
  :type 'string)

(defcustom org-e-latex-inline-image-rules
  '(("file" . "\\.\\(pdf\\|jpeg\\|jpg\\|png\\|ps\\|eps\\)\\'"))
  "Rules characterizing image files that can be inlined into LaTeX.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path.

Note that, by default, the image extension *actually* allowed
depend on the way the LaTeX file is processed.  When used with
pdflatex, pdf, jpg and png images are OK.  When processing
through dvi to Postscript, only ps and eps are allowed.  The
default we use here encompasses both."
  :group 'org-export-e-latex
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))


;;;; Tables

(defcustom org-e-latex-default-table-environment "tabular"
  "Default environment used to build tables."
  :group 'org-export-e-latex
  :type 'string)

(defcustom org-e-latex-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-e-latex
  :type 'boolean)

(defcustom org-e-latex-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-e-latex
  :type 'boolean)

(defcustom org-e-latex-tables-booktabs nil
  "When non-nil, display tables in a formal \"booktabs\" style.
This option assumes that the \"booktabs\" package is properly
loaded in the header of the document.  This value can be ignored
locally with \"booktabs=yes\" and \"booktabs=no\" LaTeX
attributes."
  :group 'org-export-e-latex
  :type 'boolean)

(defcustom org-e-latex-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-e-latex
  :type 'boolean)

(defcustom org-e-latex-table-scientific-notation "%s\\,(%s)"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-e-latex
  :type '(choice
	  (string :tag "Format string")
	  (const :tag "No formatting")))


;;;; Drawers

(defcustom org-e-latex-format-drawer-function nil
  "Function called to format a drawer in LaTeX code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-latex-format-drawer-default \(name contents\)
  \"Format a drawer element for LaTeX export.\"
  contents\)"
  :group 'org-export-e-latex
  :type 'function)


;;;; Inlinetasks

(defcustom org-e-latex-format-inlinetask-function nil
  "Function called to format an inlinetask in LaTeX code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a string.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-latex-format-inlinetask \(todo type priority name tags contents\)
\"Format an inline task element for LaTeX export.\"
  \(let \(\(full-title
	 \(concat
	  \(when todo
            \(format \"\\\\textbf{\\\\textsf{\\\\textsc{%s}}} \" todo\)\)
	  \(when priority \(format \"\\\\framebox{\\\\#%c} \" priority\)\)
	  title
	  \(when tags \(format \"\\\\hfill{}\\\\textsc{%s}\" tags\)\)\)\)\)
    \(format \(concat \"\\\\begin{center}\\n\"
		    \"\\\\fbox{\\n\"
		    \"\\\\begin{minipage}[c]{.6\\\\textwidth}\\n\"
		    \"%s\\n\\n\"
		    \"\\\\rule[.8em]{\\\\textwidth}{2pt}\\n\\n\"
		    \"%s\"
		    \"\\\\end{minipage}}\"
		    \"\\\\end{center}\"\)
	    full-title contents\)\)"
  :group 'org-export-e-latex
  :type 'function)


;; Src blocks

(defcustom org-e-latex-listings nil
  "Non-nil means export source code using the listings package.
This package will fontify source code, possibly even with color.
If you want to use this, you also need to make LaTeX use the
listings package, and if you want to have color, the color
package.  Just add these to `org-export-latex-packages-alist',
for example using customize, or with something like:

  \(require 'org-e-latex)
  \(add-to-list 'org-export-latex-packages-alist '\(\"\" \"listings\"))
  \(add-to-list 'org-export-latex-packages-alist '\(\"\" \"color\"))

Alternatively,

  \(setq org-e-latex-listings 'minted)

causes source code to be exported using the minted package as
opposed to listings.  If you want to use minted, you need to add
the minted package to `org-export-latex-packages-alist', for
example using customize, or with

  \(require 'org-e-latex)
  \(add-to-list 'org-export-latex-packages-alist '\(\"\" \"minted\"))

In addition, it is necessary to install pygments
\(http://pygments.org), and to configure the variable
`org-e-latex-pdf-process' so that the -shell-escape option is
passed to pdflatex."
  :group 'org-export-e-latex
  :type '(choice
	  (const :tag "Use listings" t)
	  (const :tag "Use minted" 'minted)
	  (const :tag "Export verbatim" nil)))

(defcustom org-e-latex-listings-langs
  '((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp")
    (c "C") (cc "C++")
    (fortran "fortran")
    (perl "Perl") (cperl "Perl") (python "Python") (ruby "Ruby")
    (html "HTML") (xml "XML")
    (tex "TeX") (latex "TeX")
    (shell-script "bash")
    (gnuplot "Gnuplot")
    (ocaml "Caml") (caml "Caml")
    (sql "SQL") (sqlite "sql"))
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the listings package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present."
  :group 'org-export-e-latex
  :type '(repeat
	  (list
	   (symbol :tag "Major mode       ")
	   (string :tag "Listings language"))))

(defcustom org-e-latex-listings-options nil
  "Association list of options for the latex listings package.

These options are supplied as a comma-separated list to the
\\lstset command.  Each element of the association list should be
a list containing two strings: the name of the option, and the
value.  For example,

  (setq org-e-latex-listings-options
    '((\"basicstyle\" \"\\small\")
      (\"keywordstyle\" \"\\color{black}\\bfseries\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages."
  :group 'org-export-e-latex
  :type '(repeat
	  (list
	   (string :tag "Listings option name ")
	   (string :tag "Listings option value"))))

(defcustom org-e-latex-minted-langs
  '((emacs-lisp "common-lisp")
    (cc "c++")
    (cperl "perl")
    (shell-script "bash")
    (caml "ocaml"))
  "Alist mapping languages to their minted language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the minted package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present.

Note that minted uses all lower case for language identifiers,
and that the full list of language identifiers can be obtained
with:

  pygmentize -L lexers"
  :group 'org-export-e-latex
  :type '(repeat
	  (list
	   (symbol :tag "Major mode     ")
	   (string :tag "Minted language"))))

(defcustom org-e-latex-minted-options nil
  "Association list of options for the latex minted package.

These options are supplied within square brackets in
\\begin{minted} environments.  Each element of the alist should
be a list containing two strings: the name of the option, and the
value.  For example,

  \(setq org-e-latex-minted-options
    '\((\"bgcolor\" \"bg\") \(\"frame\" \"lines\")))

will result in src blocks being exported with

\\begin{minted}[bgcolor=bg,frame=lines]{<LANG>}

as the start of the minted environment. Note that the same
options will be applied to blocks of all languages."
  :group 'org-export-e-latex
  :type '(repeat
	  (list
	   (string :tag "Minted option name ")
	   (string :tag "Minted option value"))))

(defvar org-e-latex-custom-lang-environments nil
  "Alist mapping languages to language-specific LaTeX environments.

It is used during export of src blocks by the listings and minted
latex packages.  For example,

  \(setq org-e-latex-custom-lang-environments
     '\(\(python \"pythoncode\"\)\)\)

would have the effect that if org encounters begin_src python
during latex export it will output

  \\begin{pythoncode}
  <src block body>
  \\end{pythoncode}")


;;;; Plain text

(defcustom org-e-latex-quotes
  '(("fr"
     ("\\(\\s-\\|[[(]\\|^\\)\"" . "«~")
     ("\\(\\S-\\)\"" . "~»")
     ("\\(\\s-\\|(\\|^\\)'" . "'"))
    ("en"
     ("\\(\\s-\\|[[(]\\|^\\)\"" . "``")
     ("\\(\\S-\\)\"" . "''")
     ("\\(\\s-\\|(\\|^\\)'" . "`")))
  "Alist for quotes to use when converting english double-quotes.

The CAR of each item in this alist is the language code.
The CDR of each item in this alist is a list of three CONS:
- the first CONS defines the opening quote;
- the second CONS defines the closing quote;
- the last CONS defines single quotes.

For each item in a CONS, the first string is a regexp
for allowed characters before/after the quote, the second
string defines the replacement string for this quote."
  :group 'org-export-e-latex
  :type '(list
	  (cons :tag "Opening quote"
		(string :tag "Regexp for char before")
		(string :tag "Replacement quote     "))
	  (cons :tag "Closing quote"
		(string :tag "Regexp for char after ")
		(string :tag "Replacement quote     "))
	  (cons :tag "Single quote"
		(string :tag "Regexp for char before")
		(string :tag "Replacement quote     "))))


;;;; Compilation

(defcustom org-e-latex-pdf-process
  '("pdflatex -interaction nonstopmode -output-directory %o %f"
    "pdflatex -interaction nonstopmode -output-directory %o %f"
    "pdflatex -interaction nonstopmode -output-directory %o %f")
  "Commands to process a LaTeX file to a PDF file.
This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
full file name, %b by the file base name \(i.e. without
extension) and %o by the base directory of the file.

The reason why this is a list is that it usually takes several
runs of `pdflatex', maybe mixed with a call to `bibtex'.  Org
does not have a clever mechanism to detect which of these
commands have to be run to get to a stable result, and it also
does not do any error checking.

By default, Org uses 3 runs of `pdflatex' to do the processing.
If you have texi2dvi on your system and if that does not cause
the infamous egrep/locale bug:

     http://lists.gnu.org/archive/html/bug-texinfo/2010-03/msg00031.html

then `texi2dvi' is the superior choice.  Org does offer it as one
of the customize options.

Alternatively, this may be a Lisp function that does the
processing, so you could use this to apply the machinery of
AUCTeX or the Emacs LaTeX mode.  This function should accept the
file name as its single argument."
  :group 'org-export-pdf
  :type '(choice
	  (repeat :tag "Shell command sequence"
		  (string :tag "Shell command"))
	  (const :tag "2 runs of pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "3 runs of pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "pdflatex,bibtex,pdflatex,pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "bibtex %b"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "texi2dvi"
		 ("texi2dvi -p -b -c -V %f"))
	  (const :tag "rubber"
		 ("rubber -d --into %o %f"))
	  (function)))

(defcustom org-e-latex-logfiles-extensions
  '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
  "The list of file extensions to consider as LaTeX logfiles."
  :group 'org-export-e-latex
  :type '(repeat (string :tag "Extension")))

(defcustom org-e-latex-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-e-latex
  :type 'boolean)



;;; Internal Functions

(defun org-e-latex--caption/label-string (caption label info)
  "Return caption and label LaTeX string for floats.

CAPTION is a cons cell of secondary strings, the car being the
standard caption and the cdr its short form.  LABEL is a string
representing the label.  INFO is a plist holding contextual
information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-e-latex--wrap-label'."
  (let ((label-str (if label (format "\\label{%s}" label) "")))
    (cond
     ((and (not caption) (not label)) "")
     ((not caption) (format "\\label{%s}\n" label))
     ;; Option caption format with short name.
     ((cdr caption)
      (format "\\caption[%s]{%s%s}\n"
	      (org-export-secondary-string (cdr caption) 'e-latex info)
	      label-str
	      (org-export-secondary-string (car caption) 'e-latex info)))
     ;; Standard caption format.
     (t (format "\\caption{%s%s}\n"
		label-str
		(org-export-secondary-string (car caption) 'e-latex info))))))

(defun org-e-latex--guess-inputenc (header)
  "Set the coding system in inputenc to what the buffer is.

HEADER is the LaTeX header string.

Return the new header."
  (let* ((cs (or (ignore-errors
		   (latexenc-coding-system-to-inputenc
		    buffer-file-coding-system))
		 "utf8")))
    (if (not cs) header
      ;; First translate if that is requested.
      (setq cs (or (cdr (assoc cs org-e-latex-inputenc-alist)) cs))
      ;; Then find the \usepackage statement and replace the option.
      (replace-regexp-in-string "\\\\usepackage\\[\\(AUTO\\)\\]{inputenc}"
				cs header t nil 1))))

(defun org-e-latex--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (loop for c across ll
	  when (not (string-match (regexp-quote (char-to-string c)) s))
	  return (char-to-string c))))

(defun org-e-latex--make-option-string (options)
  "Return a comma separated string of keywords and values.
OPTIONS is an alist where the key is the options keyword as
a string, and the value a list containing the keyword value, or
nil."
  (mapconcat (lambda (pair)
	       (concat (first pair)
		       (when (> (length (second pair)) 0)
			 (concat "=" (second pair)))))
	     options
	     ","))

(defun org-e-latex--quotation-marks (text info)
  "Export quotation marks depending on language conventions.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
	  (let ((start 0))
	    (while (setq start (string-match (car l) text start))
	      (let ((new-quote (concat (match-string 1 text) (cdr l))))
		(setq text (replace-match new-quote  t t text))))))
	(cdr (or (assoc (plist-get info :language) org-e-latex-quotes)
		 ;; Falls back on English.
		 (assoc "en" org-e-latex-quotes))))
  text)

(defun org-e-latex--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-e-latex--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
	output
      (concat (format "\\label{%s}\n" label) output))))



;;; Template

(defun org-e-latex-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-secondary-string
		(plist-get info :title) 'e-latex info)))
    (concat
     ;; 1. Time-stamp.
     (and (plist-get info :time-stamp-file)
	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; 2. Document class and packages.
     (let ((class (plist-get info :latex-class))
	   (class-options (plist-get info :latex-class-options)))
       (org-element-normalize-string
	(let* ((header (nth 1 (assoc class org-e-latex-classes)))
	       (document-class-string
		(and (stringp header)
		     (if class-options
			 (replace-regexp-in-string
			  "^[ \t]*\\\\documentclass\\(\\[.*?\\]\\)"
			  class-options header t nil 1)
		       header))))
	  (org-e-latex--guess-inputenc
	   (org-splice-latex-header
	    document-class-string
	    org-export-latex-default-packages-alist ; defined in org.el
	    org-export-latex-packages-alist nil ; defined in org.el
	    (plist-get info :latex-header-extra))))))
     ;; 3. Define alert if not yet defined.
     "\\providecommand{\\alert}[1]{\\textbf{#1}}\n"
     ;; 4. Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
	 (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; 5. Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-secondary-string
				     auth 'e-latex info)))))
	   (email (and (plist-get info :with-email)
		       (org-export-secondary-string
			(plist-get info :email) 'e-latex info))))
       (cond ((and author email (not (string= "" email)))
	      (format "\\author{%s\\thanks{%s}}\n" author email))
	     (author (format "\\author{%s}\n" author))
	     (t "\\author{}\n")))
     ;; 6. Date.
     (let ((date (plist-get info :date)))
       (and date (format "\\date{%s}\n" date)))
     ;; 7. Title
     (format "\\title{%s}\n" title)
     ;; 8. Hyperref options.
     (format "\\hypersetup{\n  pdfkeywords={%s},\n  pdfsubject={%s},\n  pdfcreator={%s}}\n"
	     (or (plist-get info :keywords) "")
	     (or (plist-get info :description) "")
	     (if (not (plist-get info :with-creator)) ""
	       (plist-get info :creator)))
     ;; 9. Document start.
     "\\begin{document}\n\n"
     ;; 10. Title command.
     (org-element-normalize-string
      (cond ((string= "" title) nil)
	    ((not (stringp org-e-latex-title-command)) nil)
	    ((string-match "\\(?:[^%]\\|^\\)%s"
			   org-e-latex-title-command)
	     (format org-e-latex-title-command title))
	    (t org-e-latex-title-command)))
     ;; 11. Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
	 (concat (when (wholenump depth)
		   (format "\\setcounter{tocdepth}{%d}\n" depth))
		 "\\tableofcontents\n\\vspace*{1cm}\n\n")))
     ;; 12. Document's body.
     contents
     ;; 13. Creator.
     (let ((creator-info (plist-get info :with-creator)))
       (cond
	((not creator-info) "")
	((eq creator-info 'comment)
	 (format "%% %s\n" (plist-get info :creator)))
	(t (concat (plist-get info :creator) "\n"))))
     ;; 14. Document end.
     "\\end{document}")))



;;; Transcode Functions

;;;; Block

(defun org-e-latex-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-latex--wrap-label
   center-block
   (format "\\begin{center}\n%s\\end{center}" contents)))


;;;; Comment

;; Comments are ignored.


;;;; Comment Block

;; Comment Blocks are ignored.


;;;; Drawer

(defun org-e-latex-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
	 (output (if (functionp org-e-latex-format-drawer-function)
		     (funcall org-e-latex-format-drawer-function
			      name contents)
		   ;; If there's no user defined function: simply
		   ;; display contents of the drawer.
		   contents)))
    (org-e-latex--wrap-label drawer output)))


;;;; Dynamic Block

(defun org-e-latex-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See
`org-export-data'."
  (org-e-latex--wrap-label dynamic-block contents))


;;;; Emphasis

(defun org-e-latex-emphasis (emphasis contents info)
  "Transcode EMPHASIS from Org to LaTeX.
CONTENTS is the contents of the emphasized text.  INFO is a plist
holding contextual information.."
  (format (cdr (assoc (org-element-property :marker emphasis)
		      org-e-latex-emphasis-alist))
	  contents))


;;;; Entity

(defun org-e-latex-entity (entity contents info)
  "Transcode an ENTITY object from Org to LaTeX.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let ((ent (org-element-property :latex entity)))
    (if (org-element-property :latex-math-p entity) (format "$%s$" ent) ent)))


;;;; Example Block

(defun org-e-latex-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-e-latex--wrap-label
   example-block
   (format "\\begin{verbatim}\n%s\\end{verbatim}"
	   (org-export-format-code-default example-block info))))


;;;; Export Snippet

(defun org-e-latex-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'e-latex)
    (org-element-property :value export-snippet)))


;;;; Export Block

(defun org-e-latex-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "latex")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Fixed Width

(defun org-e-latex-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((value (org-element-normalize-string
		 (replace-regexp-in-string
		  "^[ \t]*: ?" ""
		  (org-element-property :value fixed-width)))))
    (org-e-latex--wrap-label
     fixed-width (format "\\begin{verbatim}\n%s\\end{verbatim}" value))))


;;;; Footnote Definition

;; Footnote Definitions are ignored.


;;;; Footnote Reference

(defun org-e-latex-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       org-e-latex-footnote-separator))
   (cond
    ;; Use \footnotemark if the footnote has already been defined.
    ((not (org-export-footnote-first-reference-p footnote-reference info))
     (format "\\footnotemark[%s]{}"
	     (org-export-get-footnote-number footnote-reference info)))
    ;; Use also \footnotemark if reference is within another footnote
    ;; reference or footnote definition.
    ((loop for parent in (org-export-get-genealogy footnote-reference info)
	   thereis (memq (org-element-type parent)
			 '(footnote-reference footnote-definition)))
     (let ((num (org-export-get-footnote-number footnote-reference info)))
       (format "\\footnotemark[%s]{}\\setcounter{footnote}{%s}" num num)))
    ;; Otherwise, define it with \footnote command.
    (t
     (let ((def (org-export-get-footnote-definition footnote-reference info)))
       (unless (eq (org-element-type def) 'org-data)
	 (setq def (cons 'org-data (cons nil def))))
       (concat
	(format "\\footnote{%s}" (org-trim (org-export-data def 'e-latex info)))
	;; Retrieve all footnote references within the footnote and
	;; add their definition after it, since LaTeX doesn't support
	;; them inside.
	(let* (all-refs
	       search-refs		; for byte-compiler
	       (search-refs
		(function
		 (lambda (data)
		   ;; Return a list of all footnote references in DATA.
		   (org-element-map
		    data 'footnote-reference
		    (lambda (ref)
		      (when (org-export-footnote-first-reference-p ref info)
			(push ref all-refs)
			(when (eq (org-element-property :type ref) 'standard)
			  (funcall
			   search-refs
			   (org-export-get-footnote-definition ref info)))))
		    info) (reverse all-refs)))))
	  (mapconcat
	   (lambda (ref)
	     (format
	      "\\footnotetext[%s]{%s}"
	      (org-export-get-footnote-number ref info)
	      (org-trim
	       (funcall
		(if (eq (org-element-property :type ref) 'inline)
		    'org-export-secondary-string
		  'org-export-data)
		(org-export-get-footnote-definition ref info) 'e-latex info))))
	   (funcall search-refs def) ""))))))))


;;;; Headline

(defun org-e-latex-headline (headline contents info)
  "Transcode an HEADLINE element from Org to LaTeX.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :latex-class))
	 (level (org-export-get-relative-level headline info))
	 (numberedp (org-export-numbered-headline-p headline info))
	 (class-sectionning (assoc class org-e-latex-classes))
	 ;; Section formatting will set two placeholders: one for the
	 ;; title and the other for the contents.
	 (section-fmt
	  (let ((sec (if (and (symbolp (nth 2 class-sectionning))
			      (fboundp (nth 2 class-sectionning)))
			 (funcall (nth 2 class-sectionning) level numberedp)
		       (nth (1+ level) class-sectionning))))
	    (cond
	     ;; No section available for that LEVEL.
	     ((not sec) nil)
	     ;; Section format directly returned by a function.
	     ((stringp sec) sec)
	     ;; (numbered-section . unnumbered-section)
	     ((not (consp (cdr sec)))
	      (concat (funcall (if numberedp #'car #'cdr) sec) "\n%s"))
	     ;; (numbered-open numbered-close)
	     ((= (length sec) 2)
	      (when numberedp (concat (car sec) "\n%s" (nth 1 sec))))
	     ;; (num-in num-out no-num-in no-num-out)
	     ((= (length sec) 4)
	      (if numberedp (concat (car sec) "\n%s" (nth 1 sec))
		(concat (nth 2 sec) "\n%s" (nth 3 sec)))))))
	 (text (org-export-secondary-string
		(org-element-property :title headline) 'e-latex info))
	 (todo
	  (and (plist-get info :with-todo-keywords)
	       (let ((todo (org-element-property :todo-keyword headline)))
		 (and todo (org-export-secondary-string todo 'e-latex info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-element-property :tags headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 ;; Create the headline text.
	 (full-text (if (functionp org-e-latex-format-headline-function)
			;; User-defined formatting function.
			(funcall org-e-latex-format-headline-function
				 todo todo-type priority text tags)
		      ;; Default formatting.
		      (concat
		       (when todo
			 (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
		       (when priority (format "\\framebox{\\#%c} " priority))
		       text
		       (when tags (format "\\hfill{}\\textsc{%s}" tags)))))
	 ;; Associate some \label to the headline for internal links.
	 (headline-label
	  (format "\\label{sec-%s}\n"
		  (mapconcat 'number-to-string
			     (org-export-get-headline-number headline info)
			     "-")))
	 (pre-blanks
	  (make-string (org-element-property :pre-blank headline) 10)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((or (not section-fmt) (org-export-low-level-p headline info))
      ;; Build the real contents of the sub-tree.
      (let ((low-level-body
	     (concat
	      ;; If the headline is the first sibling, start a list.
	      (when (org-export-first-sibling-p headline info)
		(format "\\begin{%s}\n" (if numberedp 'enumerate 'itemize)))
	      ;; Itemize headline
	      "\\item " full-text "\n" headline-label pre-blanks contents)))
	;; If headline is not the last sibling simply return
	;; LOW-LEVEL-BODY.  Otherwise, also close the list, before any
	;; blank line.
	(if (not (org-export-last-sibling-p headline info)) low-level-body
	  (replace-regexp-in-string
	   "[ \t\n]*\\'"
	   (format "\n\\\\end{%s}" (if numberedp 'enumerate 'itemize))
	   low-level-body))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t (format section-fmt full-text
		(concat headline-label pre-blanks contents))))))


;;;; Horizontal Rule

(defun org-e-latex-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE  object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((attr (mapconcat #'identity
			 (org-element-property :attr_latex horizontal-rule)
			 " ")))
    (org-e-latex--wrap-label horizontal-rule (concat "\\hrule " attr))))


;;;; Inline Babel Call

;; Inline Babel Calls are ignored.


;;;; Inline Src Block

(defun org-e-latex-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block))
	 (separator (org-e-latex--find-verb-separator code)))
    (cond
     ;; Do not use a special package: transcode it verbatim.
     ((not org-e-latex-listings)
      (concat "\\verb" separator code separator))
     ;; Use minted package.
     ((eq org-e-latex-listings 'minted)
      (let* ((org-lang (org-element-property :language inline-src-block))
	     (mint-lang (or (cadr (assq (intern org-lang)
					org-e-latex-minted-langs))
			    org-lang))
	     (options (org-e-latex--make-option-string
		       org-e-latex-minted-options)))
	(concat (format "\\mint%s{%s}"
			(if (string= options "") "" (format "[%s]" options))
			mint-lang)
		separator code separator)))
     ;; Use listings package.
     (t
      ;; Maybe translate language's name.
      (let* ((org-lang (org-element-property :language inline-src-block))
	     (lst-lang (or (cadr (assq (intern org-lang)
				       org-e-latex-listings-langs))
			   org-lang))
	     (options (org-e-latex--make-option-string
		       (append org-e-latex-listings-options
			       `(("language" ,lst-lang))))))
	(concat (format "\\lstinline[%s]" options)
		separator code separator))))))


;;;; Inlinetask

(defun org-e-latex-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((title (org-export-secondary-string
	       (org-element-property :title inlinetask) 'e-latex info))
	(todo (and (plist-get info :with-todo-keywords)
		   (let ((todo (org-element-property
				:todo-keyword inlinetask)))
		     (and todo
			  (org-export-secondary-string todo 'e-latex info)))))
	(todo-type (org-element-property :todo-type inlinetask))
	(tags (and (plist-get info :with-tags)
		   (org-element-property :tags inlinetask)))
	(priority (and (plist-get info :with-priority)
		       (org-element-property :priority inlinetask))))
    ;; If `org-e-latex-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-e-latex-format-inlinetask-function)
	(funcall org-e-latex-format-inlinetask-function
		 todo todo-type priority title tags contents)
      ;; Otherwise, use a default template.
      (org-e-latex--wrap-label
       inlinetask
       (let ((full-title
	      (concat
	       (when todo (format "\\textbf{\\textsf{\\textsc{%s}}} " todo))
	       (when priority (format "\\framebox{\\#%c} " priority))
	       title
	       (when tags (format "\\hfill{}\\textsc{%s}" tags)))))
	 (format (concat "\\begin{center}\n"
			 "\\fbox{\n"
			 "\\begin{minipage}[c]{.6\\textwidth}\n"
			 "%s\n\n"
			 "\\rule[.8em]{\\textwidth}{2pt}\n\n"
			 "%s"
			 "\\end{minipage}\n"
			 "}\n"
			 "\\end{center}")
		 full-title contents))))))


;;;; Item

(defun org-e-latex-item (item contents info)
  "Transcode an ITEM element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  ;; Grab `:level' from plain-list properties, which is always the
  ;; first element above current item.
  (let* ((level (org-element-property :level (org-export-get-parent item info)))
	 (counter (let ((count (org-element-property :counter item)))
		    (and count
			 (< level 4)
			 (format "\\setcounter{enum%s}{%s}\n"
				 (nth level '("i" "ii" "iii" "iv"))
				 (1- count)))))
	 (checkbox (let ((checkbox (org-element-property :checkbox item)))
		     (cond ((eq checkbox 'on) "$\\boxtimes$ ")
			   ((eq checkbox 'off) "$\\Box$ ")
			   ((eq checkbox 'trans) "$\\boxminus$ "))))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag
		     (format "[%s]" (org-export-secondary-string
				     tag 'e-latex info))))))
    (concat counter "\\item" tag " " checkbox contents)))


;;;; Keyword

(defun org-e-latex-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "LATEX") value)
     ((string= key "INDEX") (format "\\index{%s}" value))
     ;; Invisible targets.
     ((string= key "TARGET") nil)
     ((string= key "TOC")
      (let ((value (downcase value)))
	(cond
	 ((string-match "\\<headlines\\>" value)
	  (let ((depth (or (and (string-match "[0-9]+" value)
				(string-to-number (match-string 0 value)))
			   (plist-get info :with-toc))))
	    (concat
	     (when (wholenump depth)
	       (format "\\setcounter{tocdepth}{%s}\n" depth))
	     "\\tableofcontents")))
	 ((string= "tables" value) "\\listoftables")
	 ((string= "figures" value) "\\listoffigures")
	 ((string= "listings" value)
	  (cond
	   ((eq org-e-latex-listings 'minted) "\\listoflistings")
	   (org-e-latex-listings "\\lstlistoflistings")
	   ;; At the moment, src blocks with a caption are wrapped
	   ;; into a figure environment.
	   (t "\\listoffigures")))))))))


;;;; Latex Environment

(defun org-e-latex-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((label (org-element-property :name latex-environment))
	(value (org-remove-indentation
		(org-element-property :value latex-environment))))
    (if (not (org-string-nw-p label)) value
      ;; Environment is labelled: label must be within the environment
      ;; (otherwise, a reference pointing to that element will count
      ;; the section instead).
      (with-temp-buffer
	(insert value)
	(goto-char (point-min))
	(forward-line)
	(insert (format "\\label{%s}\n" label))
	(buffer-string)))))


;;;; Latex Fragment

(defun org-e-latex-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value latex-fragment))


;;;; Line Break

(defun org-e-latex-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "\\\\")


;;;; Link

(defun org-e-latex-link--inline-image (link info)
  "Return LaTeX code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (let* ((parent (org-export-get-parent-paragraph link info))
	 (path (let ((raw-path (org-element-property :path link)))
		 (if (not (file-name-absolute-p raw-path)) raw-path
		   (expand-file-name raw-path))))
	 (caption (org-e-latex--caption/label-string
		   (org-element-property :caption parent)
		   (org-element-property :name parent)
		   info))
	 ;; Retrieve latex attributes from the element around.
	 (attr (let ((raw-attr
		      (mapconcat #'identity
				 (org-element-property :attr_latex parent)
				 " ")))
		 (unless (string= raw-attr "") raw-attr)))
	 (disposition
	  (cond
	   ((and attr (string-match "\\<wrap\\>" attr)) 'wrap)
	   ((and attr (string-match "\\<multicolumn\\>" attr)) 'multicolumn)
	   ((or (and attr (string-match "\\<float\\>" attr))
		(not (string= caption "")))
	    'float)))
	 (placement
	  (cond
	   ((and attr (string-match "\\<placement=\\(\\S-+\\)" attr))
	    (org-match-string-no-properties 1 attr))
	   ((eq disposition 'wrap) "{l}{0.5\\textwidth}")
	   ((eq disposition 'float)
	    (concat "[" org-e-latex-default-figure-position "]"))
	   (t ""))))
    ;; Now clear ATTR from any special keyword and set a default
    ;; value if nothing is left.
    (setq attr
	  (if (not attr) ""
	    (org-trim
	     (replace-regexp-in-string
	      "\\(wrap\\|multicolumn\\|float\\|placement=\\S-+\\)" "" attr))))
    (setq attr (cond ((not (string= attr "")) attr)
		     ((eq disposition 'float) "width=0.7\\textwidth")
		     ((eq disposition 'wrap) "width=0.48\\textwidth")
		     (t (or org-e-latex-image-default-option ""))))
    ;; Return proper string, depending on DISPOSITION.
    (case disposition
      (wrap (format "\\begin{wrapfigure}%s
\\centering
\\includegraphics[%s]{%s}
%s\\end{wrapfigure}" placement attr path caption))
      (mulicolumn (format "\\begin{figure*}%s
\\centering
\\includegraphics[%s]{%s}
%s\\end{figure*}" placement attr path caption))
      (float (format "\\begin{figure}%s
\\centering
\\includegraphics[%s]{%s}
%s\\end{figure}" placement attr path caption))
      (t (format "\\includegraphics[%s]{%s}" attr path)))))

(defun org-e-latex-link (link desc info)
  "Transcode a LINK object from Org to LaTeX.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (imagep (org-export-inline-image-p
		  link org-e-latex-inline-image-rules))
	 (path (cond
		((member type '("http" "https" "ftp" "mailto"))
		 (concat type ":" raw-path))
		((string= type "file")
		 (when (string-match "\\(.+\\)::.+" raw-path)
		   (setq raw-path (match-string 1 raw-path)))
		 (if (file-name-absolute-p raw-path)
		     (concat "file://" (expand-file-name raw-path))
		   ;; TODO: Not implemented yet.  Concat also:
		   ;; (org-export-directory :LaTeX info)
		   (concat "file://" raw-path)))
		(t raw-path)))
	 protocol)
    (cond
     ;; Image file.
     (imagep (org-e-latex-link--inline-image link info))
     ;; Radioed target: Target's name is obtained from original raw
     ;; link.  Path is parsed and transcoded in order to have a proper
     ;; display of the contents.
     ((string= type "radio")
      (format "\\hyperref[%s]{%s}"
	      (org-export-solidify-link-text path)
	      (org-export-secondary-string
	       (org-element-parse-secondary-string
		path (cdr (assq 'radio-target org-element-object-restrictions)))
	       'e-latex info)))
     ;; Links pointing to an headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  ;; Fuzzy link points nowhere.
	  ('nil
	   (format "\\texttt{%s}"
		   (or desc
		       (org-export-secondary-string
			(org-element-property :raw-link link)
			'e-latex info))))
	  ;; Fuzzy link points to an invisible target.
	  (keyword nil)
	  ;; LINK points to an headline.  If headlines are numbered
	  ;; and the link has no description, display headline's
	  ;; number.  Otherwise, display description or headline's
	  ;; title.
	  (headline
	   (let ((label
		  (format "sec-%s"
			  (mapconcat
			   'number-to-string
			   (org-export-get-headline-number destination info)
			   "-"))))
	     (if (and (plist-get info :section-numbers) (not desc))
		 (format "\\ref{%s}" label)
	       (format "\\hyperref[%s]{%s}" label
		       (or desc
			   (org-export-secondary-string
			    (org-element-property :title destination)
			    'e-latex info))))))
          ;; Fuzzy link points to a target.  Do as above.
	  (otherwise
	   (let ((path (org-export-solidify-link-text path)))
	     (if (not desc) (format "\\ref{%s}" path)
	       (format "\\hyperref[%s]{%s}" path desc)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (format (org-export-get-coderef-format path (or desc ""))
	      (org-export-resolve-coderef path info)))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'latex))
     ;; External link with a description part.
     ((and path desc) (format "\\href{%s}{%s}" path desc))
     ;; External link without a description part.
     (path (format "\\url{%s}" path))
     ;; No path, only description.  Try to do something useful.
     (t (format "\\texttt{%s}" desc)))))


;;;; Babel Call

;; Babel Calls are ignored.


;;;; Macro

(defun org-e-latex-macro (macro contents info)
  "Transcode a MACRO element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))


;;;; Paragraph

(defun org-e-latex-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to LaTeX.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  contents)


;;;; Plain List

(defun org-e-latex-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to LaTeX.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
	 (paralist-types '("inparaenum" "asparaenum" "inparaitem" "asparaitem"
			   "inparadesc" "asparadesc"))
	 (paralist-regexp (concat
			   "\\("
			   (mapconcat 'identity paralist-types "\\|")
			   "\\)"))
	 (attr (mapconcat #'identity
			  (org-element-property :attr_latex plain-list)
			  " "))
	 (latex-type (cond
		      ((and attr
			    (string-match
			     (format "\\<%s\\>" paralist-regexp) attr))
		       (match-string 1 attr))
		      ((eq type 'ordered) "enumerate")
		      ((eq type 'unordered) "itemize")
		      ((eq type 'descriptive) "description"))))
    (org-e-latex--wrap-label
     plain-list
     (format "\\begin{%s}%s\n%s\\end{%s}"
	     latex-type
	     ;; Once special environment, if any, has been removed, the
	     ;; rest of the attributes will be optional arguments.
	     ;; They will be put inside square brackets if necessary.
	     (let ((opt (replace-regexp-in-string
			 (format " *%s *" paralist-regexp) "" attr)))
	       (cond ((string= opt "") "")
		     ((string-match "\\`\\[[^][]+\\]\\'" opt) opt)
		     (t (format "[%s]" opt))))
	     contents
	     latex-type))))


;;;; Plain Text

(defun org-e-latex-plain-text (text info)
  "Transcode a TEXT string from Org to LaTeX.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect %, #, &, $, ~, ^, _,  { and }.
  (while (string-match "\\([^\\]\\|^\\)\\([%$#&{}~^_]\\)" text)
    (setq text
	  (replace-match (format "\\%s" (match-string 2 text)) nil t text 2)))
  ;; Protect \
  (setq text (replace-regexp-in-string
	      "\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%$#&{}~^_\\]\\|$\\)"
	      "$\\backslash$" text nil t 1))
  ;; LaTeX into \LaTeX{} and TeX into \TeX{}.
  (let ((case-fold-search nil)
	(start 0))
    (while (string-match "\\<\\(\\(?:La\\)?TeX\\)\\>" text start)
      (setq text (replace-match
		  (format "\\%s{}" (match-string 1 text)) nil t text)
	    start (match-end 0))))
  ;; Handle quotation marks
  (setq text (org-e-latex--quotation-marks text info))
  ;; Convert special strings.
  (when (plist-get info :with-special-strings)
    (while (string-match (regexp-quote "...") text)
      (setq text (replace-match "\\ldots{}" nil t text))))
  ;; Handle break preservation if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n"
					 text)))
  ;; Return value.
  text)


;;;; Property Drawer

(defun org-e-latex-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")


;;;; Quote Block

(defun org-e-latex-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-latex--wrap-label
   quote-block
   (format "\\begin{quote}\n%s\\end{quote}" contents)))


;;;; Quote Section

(defun org-e-latex-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-property :value quote-section))))
    (when value (format "\\begin{verbatim}\n%s\\end{verbatim}" value))))


;;;; Section

(defun org-e-latex-section (section contents info)
  "Transcode a SECTION element from Org to LaTeX.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;;; Radio Target

(defun org-e-latex-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to LaTeX.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "\\label{%s}%s"
	  (org-export-solidify-link-text
	   (org-element-property :value radio-target))
	  text))


;;;; Special Block

(defun org-e-latex-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block))))
    (org-e-latex--wrap-label
     special-block
     (format "\\begin{%s}\n%s\\end{%s}" type contents type))))


;;;; Src Block

(defun org-e-latex-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language src-block))
	 (caption (org-element-property :caption src-block))
	 (label (org-element-property :name src-block))
	 (custom-env (and lang
			  (cadr (assq (intern lang)
				      org-e-latex-custom-lang-environments))))
	 (num-start (case (org-element-property :number-lines src-block)
		      (continued (org-export-get-loc src-block info))
		      (new 0)))
	 (retain-labels (org-element-property :retain-labels src-block)))
    (cond
     ;; Case 1.  No source fontification.
     ((not org-e-latex-listings)
      (let ((caption-str (org-e-latex--caption/label-string caption label info))
	    (float-env (when caption "\\begin{figure}[H]\n%s\n\\end{figure}")))
	(format
	 (or float-env "%s")
	 (concat caption-str
		 (format "\\begin{verbatim}\n%s\\end{verbatim}"
			 (org-export-format-code-default src-block info))))))
     ;; Case 2.  Custom environment.
     (custom-env (format "\\begin{%s}\n%s\\end{%s}\n"
			 custom-env
			 (org-export-format-code-default src-block info)
			 custom-env))
     ;; Case 3.  Use minted package.
     ((eq org-e-latex-listings 'minted)
      (let ((float-env (when (or label caption)
			 (format "\\begin{listing}[H]\n%%s\n%s\\end{listing}"
				 (org-e-latex--caption/label-string
				  caption label info))))
	    (body
	     (format
	      "\\begin{minted}[%s]{%s}\n%s\\end{minted}"
	      ;; Options.
	      (org-e-latex--make-option-string
	       (if (not num-start) org-e-latex-minted-options
		 (append `(("linenos")
			   ("firstnumber" ,(number-to-string (1+ num-start))))
			 org-e-latex-minted-options)))
	      ;; Language.
	      (or (cadr (assq (intern lang) org-e-latex-minted-langs)) lang)
	      ;; Source code.
	      (let* ((code-info (org-export-unravel-code src-block))
		     (max-width
		      (apply 'max
			     (mapcar 'length
				     (org-split-string (car code-info) "\n")))))
		(org-export-format-code
		 (car code-info)
		 (lambda (loc num ref)
		   (concat
		    loc
		    (when ref
		      ;; Ensure references are flushed to the right,
		      ;; separated with 6 spaces from the widest line
		      ;; of code.
		      (concat (make-string (+ (- max-width (length loc)) 6) ? )
			      (format "(%s)" ref)))))
		 nil (and retain-labels (cdr code-info)))))))
	;; Return value.
	(if float-env (format float-env body) body)))
     ;; Case 4.  Use listings package.
     (t
      (let ((lst-lang
	     (or (cadr (assq (intern lang) org-e-latex-listings-langs)) lang))
	    (caption-str
	     (when caption
	       (let ((main (org-export-secondary-string
			    (car caption) 'e-latex info)))
		 (if (not (cdr caption)) (format "{%s}" main)
		   (format
		    "{[%s]%s}"
		    (org-export-secondary-string (cdr caption) 'e-latex info)
		    main))))))
	(concat
	 ;; Options.
	 (format "\\lstset{%s}\n"
		 (org-e-latex--make-option-string
		  (append org-e-latex-listings-options
			  `(("language" ,lst-lang))
			  (when label `(("label" ,label)))
			  (when caption-str `(("caption" ,caption-str)))
			  (cond ((not num-start) '(("numbers" "none")))
				((zerop num-start) '(("numbers" "left")))
				(t `(("numbers" "left")
				     ("firstnumber"
				      ,(number-to-string (1+ num-start)))))))))
	 ;; Source code.
	 (format
	  "\\begin{lstlisting}\n%s\\end{lstlisting}"
	  (let* ((code-info (org-export-unravel-code src-block))
		 (max-width
		  (apply 'max
			 (mapcar 'length
				 (org-split-string (car code-info) "\n")))))
	    (org-export-format-code
	     (car code-info)
	     (lambda (loc num ref)
	       (concat
		loc
		(when ref
		  ;; Ensure references are flushed to the right,
		  ;; separated with 6 spaces from the widest line of
		  ;; code
		  (concat (make-string (+ (- max-width (length loc)) 6) ? )
			  (format "(%s)" ref)))))
	     nil (and retain-labels (cdr code-info)))))))))))


;;;; Statistics Cookie

(defun org-e-latex-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;;; Subscript

(defun org-e-latex-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to LaTeX.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format (if (= (length contents) 1) "$_%s$" "$_{\\mathrm{%s}}$") contents))


;;;; Superscript

(defun org-e-latex-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to LaTeX.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format (if (= (length contents) 1) "$^%s$" "$^{\\mathrm{%s}}$") contents))


;;;; Table

(defun org-e-latex-table--format-string (table info)
  "Return an appropriate format string for TABLE.

TABLE-INFO is the plist containing format info about the table,
as returned by `org-export-table-format-info'.  INFO is a plist
used as a communication channel.

The format string leaves one placeholder for the body of the
table."
  (let* ((label (org-element-property :name table))
	 (caption (org-e-latex--caption/label-string
		   (org-element-property :caption table) label info))
	 (attr (mapconcat 'identity
			  (org-element-property :attr_latex table)
			  " "))
	 ;; Determine alignment string.
	 (alignment (org-e-latex-table--align-string table info))
	 ;; Determine environment for the table: longtable, tabular...
	 (table-env (cond
		     ((not attr) org-e-latex-default-table-environment)
		     ((string-match "\\<longtable\\>" attr) "longtable")
		     ((string-match "\\<tabular.?\\>" attr)
		      (org-match-string-no-properties 0 attr))
		     (t org-e-latex-default-table-environment)))
	 ;; If table is a float, determine environment: table, table*
	 ;; or sidewaystable.
	 (float-env (cond
		     ((string= "longtable" table-env) nil)
		     ((and attr (string-match "\\<sidewaystable\\>" attr))
		      "sidewaystables")
		     ((and attr
			   (or (string-match (regexp-quote "table*") attr)
			       (string-match "\\<multicolumn\\>" attr)))
		      "table*")
		     ((or (not (string= caption "")) label) "table")))
	 ;; Extract others display options.
	 (width (and attr (string-match "\\<width=\\(\\S-+\\)" attr)
		     (org-match-string-no-properties 1 attr)))
	 (placement
	  (if (and attr (string-match "\\<placement=\\(\\S-+\\)" attr))
	      (org-match-string-no-properties 1 attr)
	    (format "[%s]" org-e-latex-default-figure-position))))
    ;; Prepare the final format string for the table.
    (cond
     ;; Longtable.
     ((string= "longtable" table-env)
      (format
       "\\begin{longtable}{%s}\n%s%%s%s\\end{longtable}"
       alignment
       (if (or (not org-e-latex-table-caption-above) (string= "" caption)) ""
	 (concat (org-trim caption) "\\\\\n"))
       (if (or org-e-latex-table-caption-above (string= "" caption)) ""
	 (concat (org-trim caption) "\\\\\n"))))
     ;; Others.
     (t (concat (when float-env
		  (concat
		   (format "\\begin{%s}%s\n" float-env placement)
		   (if org-e-latex-table-caption-above caption "")))
		(when org-e-latex-tables-centered "\\begin{center}\n")
		(format "\\begin{%s}%s{%s}\n%%s\\end{%s}"
			table-env
			(if width (format "{%s}" width) "") alignment table-env)
		(when org-e-latex-tables-centered "\n\\end{center}")
		(when float-env
		  (concat (if org-e-latex-table-caption-above "" caption)
			  (format "\n\\end{%s}" float-env))))))))

(defun org-e-latex-table--align-string (table info)
  "Return an appropriate LaTeX alignment string.
TABLE is the considered table.  INFO is a plist used as
a communication channel."
  (let ((attr (mapconcat 'identity
			 (org-element-property :attr_latex table)
			 " ")))
    (if (and attr (string-match "\\<align=\\(\\S-+\\)" attr))
	(match-string 1 attr)
      (let (alignment)
	;; Extract column groups and alignment from first (non-rule)
	;; row.
	(org-element-map
	 (org-element-map table 'table-row 'identity info 'first-match)
	 'table-cell
	 (lambda (cell)
	   (let ((borders (org-export-table-cell-borders cell info)))
	     ;; Check left border for the first cell only.
	     (when (and (memq 'left borders) (not alignment))
	       (push "|" alignment))
	     (push (case (org-export-table-cell-alignment cell info)
		     (left "l")
		     (right "r")
		     (center "c"))
		   alignment)
	     (when (memq 'right borders) (push "|" alignment))))
	 info)
	(apply 'concat (reverse alignment))))))

(defun org-e-latex-table (table contents info)
  "Transcode a TABLE element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (cond
   ;; Case 1: verbatim table.
   ((or org-e-latex-tables-verbatim
	(let ((attr (mapconcat 'identity
			       (org-element-property :attr_latex table)
			       " ")))
	  (and attr (string-match "\\<verbatim\\>" attr))))
    (format "\\begin{verbatim}\n%s\n\\end{verbatim}"
	    ;; Re-create table, without affiliated keywords.
	    (org-trim
	     (org-element-interpret-data
	      `(org-data nil (table nil ,@(org-element-contents table)))))))
   ;; Case 2: table.el table.  Convert it using appropriate tools.
   ((eq (org-element-property :type table) 'table.el)
    (require 'table)
    ;; Ensure "*org-export-table*" buffer is empty.
    (with-current-buffer (get-buffer-create "*org-export-table*")
      (erase-buffer))
    (let ((output (with-temp-buffer
		    (insert (org-element-property :value table))
		    (goto-char 1)
		    (re-search-forward "^[ \t]*|[^|]" nil t)
		    (table-generate-source 'latex "*org-export-table*")
		    (with-current-buffer "*org-export-table*"
		      (org-trim (buffer-string))))))
      (kill-buffer (get-buffer "*org-export-table*"))
      ;; Remove left out comments.
      (while (string-match "^%.*\n" output)
	(setq output (replace-match "" t t output)))
      ;; When the "rmlines" attribute is provided, remove all hlines
      ;; but the the one separating heading from the table body.
      (let ((attr (mapconcat 'identity
			     (org-element-property :attr_latex table)
			     " ")))
	(when (and attr (string-match "\\<rmlines\\>" attr))
	  (let ((n 0) (pos 0))
	    (while (and (< (length output) pos)
			(setq pos (string-match "^\\\\hline\n?" output pos)))
	      (incf n)
	      (unless (= n 2)
		(setq output (replace-match "" nil nil output)))))))
      (if (not org-e-latex-tables-centered) output
	(format "\\begin{center}\n%s\n\\end{center}" output))))
   ;; Case 3: Standard table.
   (t (format (org-e-latex-table--format-string table info) contents))))


;;;; Table Cell

(defun org-e-latex-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to LaTeX.
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (concat (if (and contents
		   org-e-latex-table-scientific-notation
		   (string-match orgtbl-exp-regexp contents))
	      ;; Use appropriate format string for scientific
	      ;; notation.
	      (format org-e-latex-table-scientific-notation
		      (match-string 1 contents)
		      (match-string 2 contents))
	    contents)
	  (when (org-export-get-next-element table-cell info) " & ")))


;;;; Table Row

(defun org-e-latex-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to LaTeX.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((attr (mapconcat 'identity
			    (org-element-property
			     :attr_latex (org-export-get-parent table-row info))
			    " "))
	   (longtablep (and attr (string-match "\\<longtable\\>" attr)))
	   (booktabsp
	    (or (and attr (string-match "\\<booktabs=\\(yes\\|t\\)\\>" attr))
		org-e-latex-tables-booktabs))
	   ;; TABLE-ROW's borders are extracted from its first cell.
	   (borders
	    (org-export-table-cell-borders
	     (car (org-element-contents table-row)) info)))
      (concat
       ;; When BOOKTABS are activated enforce top-rule even when no
       ;; hline was specifically marked.
       (cond ((and booktabsp (memq 'top borders)) "\\toprule\n")
	     ((and (memq 'top borders) (memq 'above borders)) "\\hline\n"))
       contents "\\\\\n"
       (cond
	;; Special case for long tables. Define header and footers.
	((and longtablep (org-export-table-row-ends-header-p table-row info))
	 (format "%s
\\endhead
%s\\multicolumn{%d}{r}{Continued on next page} \\\\
\\endfoot
\\endlastfoot"
		 (if booktabsp "\\midrule" "\\hline")
		 (if booktabsp "\\midrule" "\\hline")
		 ;; Number of columns.
		 (cdr (org-export-table-dimensions
		       (org-export-get-parent-table table-row info) info))))
	;; When BOOKTABS are activated enforce bottom rule even when
	;; no hline was specifically marked.
	((and booktabsp (memq 'bottom borders)) "\\bottomrule")
	((and (memq 'bottom borders) (memq 'below borders)) "\\hline")
	((memq 'below borders) (if booktabsp "\\midrule" "\\hline")))))))


;;;; Target

(defun org-e-latex-target (target contents info)
  "Transcode a TARGET object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\label{%s}"
	  (org-export-solidify-link-text (org-element-property :value target))))


;;;; Time-stamp

(defun org-e-latex-time-stamp (time-stamp contents info)
  "Transcode a TIME-STAMP object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-element-property :value time-stamp))
	(type (org-element-property :type time-stamp))
	(appt-type (org-element-property :appt-type time-stamp)))
    (concat (cond ((eq appt-type 'scheduled)
		   (format "\\textbf{\\textsc{%s}} " org-scheduled-string))
		  ((eq appt-type 'deadline)
		   (format "\\textbf{\\textsc{%s}} " org-deadline-string))
		  ((eq appt-type 'closed)
		   (format "\\textbf{\\textsc{%s}} " org-closed-string)))
	    (cond ((memq type '(active active-range))
		   (format org-e-latex-active-timestamp-format value))
		  ((memq type '(inactive inactive-range))
		   (format org-e-latex-inactive-timestamp-format value))
		  (t
		   (format org-e-latex-diary-timestamp-format value))))))


;;;; Verbatim

(defun org-e-latex-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((fmt (cdr (assoc (org-element-property :marker verbatim)
			 org-e-latex-emphasis-alist)))
	(value (org-element-property :value verbatim)))
    (cond
     ;; Handle the `verb' special case.
     ((eq 'verb fmt)
      (let ((separator (org-e-latex--find-verb-separator value)))
	(concat "\\verb" separator value separator)))
     ;; Handle the `protectedtexttt' special case.
     ((eq 'protectedtexttt fmt)
      (let ((start 0)
	    (trans '(("\\" . "\\textbackslash{}")
		     ("~" . "\\textasciitilde{}")
		     ("^" . "\\textasciicircum{}")))
	    (rtn "")
	    char)
	(while (string-match "[\\{}$%&_#~^]" value)
	  (setq char (match-string 0 value))
	  (if (> (match-beginning 0) 0)
	      (setq rtn (concat rtn (substring value 0 (match-beginning 0)))))
	  (setq value (substring value (1+ (match-beginning 0))))
	  (setq char (or (cdr (assoc char trans)) (concat "\\" char))
		rtn (concat rtn char)))
	(setq value (concat rtn value)
	      fmt "\\texttt{%s}")
	(while (string-match "--" value)
	  (setq value (replace-match "-{}-" t t value)))
	(format fmt value)))
     ;; Else use format string.
     (t (format fmt value)))))


;;;; Verse Block

(defun org-e-latex-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-latex--wrap-label
   verse-block
   ;; In a verse environment, add a line break to each newline
   ;; character and change each white space at beginning of a line
   ;; into a space of 1 em.  Also change each blank line with
   ;; a vertical space of 1 em.
   (progn
     (setq contents (replace-regexp-in-string
		     "^ *\\\\\\\\$" "\\\\vspace*{1em}"
		     (replace-regexp-in-string
		      "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n"
		      (org-remove-indentation
		       (org-export-secondary-string
			(org-element-property :value verse-block)
			'e-latex info)))))
     (while (string-match "^[ \t]+" contents)
       (let ((new-str (format "\\hspace*{%dem}"
			      (length (match-string 0 contents)))))
	 (setq contents (replace-match new-str nil t contents))))
     (format "\\begin{verse}\n%s\\end{verse}" contents))))



;;; Interactive functions

(defun org-e-latex-export-to-latex
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to a LaTeX file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep pub-dir)))
    (org-export-to-file
     'e-latex outfile subtreep visible-only body-only ext-plist)))

(defun org-e-latex-export-to-pdf
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to LaTeX then process through to PDF.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return PDF file's name."
  (interactive)
  (org-e-latex-compile
   (org-e-latex-export-to-latex
    subtreep visible-only body-only ext-plist pub-dir)))

(defun org-e-latex-compile (texfile)
  "Compile a TeX file.

TEXFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-e-latex-pdf-process'.

Return PDF file name or an error if it couldn't be produced."
  (let* ((wconfig (current-window-configuration))
	 (texfile (file-truename texfile))
	 (base (file-name-sans-extension texfile))
	 errors)
    (message (format "Processing LaTeX file %s ..." texfile))
    (unwind-protect
	(progn
	  (cond
	   ;; A function is provided: Apply it.
	   ((functionp org-e-latex-pdf-process)
	    (funcall org-e-latex-pdf-process (shell-quote-argument texfile)))
	   ;; A list is provided: Replace %b, %f and %o with appropriate
	   ;; values in each command before applying it.  Output is
	   ;; redirected to "*Org PDF LaTeX Output*" buffer.
	   ((consp org-e-latex-pdf-process)
	    (let* ((out-dir (or (file-name-directory texfile) "./"))
		   (outbuf (get-buffer-create "*Org PDF LaTeX Output*")))
	      (mapc
	       (lambda (command)
		 (shell-command
		  (replace-regexp-in-string
		   "%b" (shell-quote-argument base)
		   (replace-regexp-in-string
		    "%f" (shell-quote-argument texfile)
		    (replace-regexp-in-string
		     "%o" (shell-quote-argument out-dir) command t t) t t) t t)
		  outbuf))
	       org-e-latex-pdf-process)
	      ;; Collect standard errors from output buffer.
	      (setq errors (org-e-latex-collect-errors outbuf))))
	   (t (error "No valid command to process to PDF")))
	  (let ((pdffile (concat base ".pdf")))
	    ;; Check for process failure.  Provide collected errors if
	    ;; possible.
	    (if (not (file-exists-p pdffile))
		(error (concat (format "PDF file %s wasn't produced" pdffile)
			       (when errors (concat ": " errors))))
	      ;; Else remove log files, when specified, and signal end of
	      ;; process to user, along with any error encountered.
	      (when org-e-latex-remove-logfiles
		(dolist (ext org-e-latex-logfiles-extensions)
		  (let ((file (concat base "." ext)))
		    (when (file-exists-p file) (delete-file file)))))
	      (message (concat "Process completed"
			       (if (not errors) "."
				 (concat " with errors: " errors)))))
	    ;; Return output file name.
	    pdffile))
      (set-window-configuration wconfig))))

(defun org-e-latex-collect-errors (buffer)
  "Collect some kind of errors from \"pdflatex\" command output.

BUFFER is the buffer containing output.

Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Find final "pdflatex" run.
      (when (re-search-backward "^[ \t]*This is pdf.*?TeX.*?Version" nil t)
	(let ((case-fold-search t)
	      (errors ""))
	  (when (save-excursion
		  (re-search-forward "Reference.*?undefined" nil t))
	    (setq errors (concat errors " [undefined reference]")))
	  (when (save-excursion
		  (re-search-forward "Citation.*?undefined" nil t))
	    (setq errors (concat errors " [undefined citation]")))
	  (when (save-excursion
		  (re-search-forward "Undefined control sequence" nil t))
	    (setq errors (concat errors " [undefined control sequence]")))
	  (when (save-excursion
		  (re-search-forward "^! LaTeX.*?Error" nil t))
	    (setq errors (concat errors " [LaTeX error]")))
	  (when (save-excursion
		  (re-search-forward "^! Package.*?Error" nil t))
	    (setq errors (concat errors " [package error]")))
	  (and (org-string-nw-p errors) (org-trim errors)))))))


(provide 'org-e-latex)
;;; org-e-latex.el ends here

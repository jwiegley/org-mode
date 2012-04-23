;;; org-e-html.el --- HTML Back-End For Org Export Engine

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

;; Author: Jambunathan K <kjambunathan at gmail dot com>
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

;; This library implements a HTML back-end for Org generic exporter.

;; To test it, run
;;
;;   M-: (org-export-to-buffer 'e-html "*Test e-HTML*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the HTML
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.

;;; Code:

;;; org-e-html.el
;;; Dependencies

(require 'format-spec)
(eval-when-compile (require 'cl) (require 'table))



;;; Function Declarations

(declare-function org-element-get-property "org-element" (property element))
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
(declare-function org-export-get-coderef-format "org-export" (path desc))
(declare-function org-export-get-footnote-definition "org-export"
		  (footnote-reference info))
(declare-function org-export-get-footnote-number "org-export" (footnote info))
(declare-function org-export-get-previous-element "org-export" (blob info))
(declare-function org-export-get-relative-level "org-export" (headline info))
(declare-function org-export-handle-code
		  "org-export" (element info &optional num-fmt ref-fmt delayed))
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

(declare-function org-id-find-id-file "org-id" (id))
(declare-function htmlize-region "ext:htmlize" (beg end))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))




;;; Internal Variables

(defconst org-e-html-option-alist
  '((:agenda-style nil nil org-agenda-export-html-style)
    (:convert-org-links nil nil org-e-html-link-org-files-as-html)
    ;; FIXME Use (org-xml-encode-org-text-skip-links s) ??
    ;; (:expand-quoted-html nil "@" org-e-html-expand)
    (:inline-images nil nil org-e-html-inline-images)
    ;; (:link-home nil nil org-e-html-link-home) FIXME
    ;; (:link-up nil nil org-e-html-link-up) FIXME
    (:style nil nil org-e-html-style)
    (:style-extra nil nil org-e-html-style-extra)
    (:style-include-default nil nil org-e-html-style-include-default)
    (:style-include-scripts nil nil org-e-html-style-include-scripts)
    ;; (:timestamp nil nil org-e-html-with-timestamp)
    (:html-extension nil nil org-e-html-extension)
    (:html-postamble nil nil org-e-html-postamble)
    (:html-preamble nil nil org-e-html-preamble)
    (:html-table-tag nil nil org-e-html-table-tag)
    (:xml-declaration nil nil org-e-html-xml-declaration)
    (:LaTeX-fragments nil "LaTeX" org-export-with-LaTeX-fragments)
    (:mathjax "MATHJAX" nil "" space))
  "Alist between export properties and ways to set them.

The car of the alist is the property name, and the cdr is a list
like \(KEYWORD OPTION DEFAULT BEHAVIOUR\) where:

KEYWORD is a string representing a buffer keyword, or nil.
OPTION is a string that could be found in an #+OPTIONS: line.
DEFAULT is the default value for the property.
BEHAVIOUR determine how Org should handle multiple keywords for
the same property.  It is a symbol among:
  nil       Keep old value and discard the new one.
  t         Replace old value with the new one.
  `space'   Concatenate the values, separating them with a space.
  `newline' Concatenate the values, separating them with
            a newline.
  `split'   Split values at white spaces, and cons them to the
            previous list.

KEYWORD and OPTION have precedence over DEFAULT.

All these properties should be back-end agnostic.  For back-end
specific properties, define a similar variable named
`org-BACKEND-option-alist', replacing BACKEND with the name of
the appropriate back-end.  You can also redefine properties
there, as they have precedence over these.")

;; FIXME: it already exists in org-e-html.el
(defconst org-e-html-cvt-link-fn
   nil
   "Function to convert link URLs to exportable URLs.
Takes two arguments, TYPE and PATH.
Returns exportable url as (TYPE PATH), or nil to signal that it
didn't handle this case.
Intended to be locally bound around a call to `org-export-as-html'." )




(defvar org-e-html-format-table-no-css)
(defvar htmlize-buffer-places)  ; from htmlize.el
(defvar body-only) ; dynamically scoped into this.



;;; User Configuration Variables

(defgroup org-export-e-html nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export HTML"
  :group 'org-export)

;;;; Debugging

(defcustom org-e-html-pretty-output nil
  "Enable this to generate pretty HTML."
  :group 'org-export-e-html
  :type 'boolean)


;;;; Document

(defcustom org-e-html-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-xml-declaration
  '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
    ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
  "The extension for exported HTML files.
%s will be replaced with the charset of the exported file.
This may be a string, or an alist with export extensions
and corresponding declarations."
  :group 'org-export-e-html
  :type '(choice
	  (string :tag "Single declaration")
	  (repeat :tag "Dependent on extension"
		  (cons (string :tag "Extension")
			(string :tag "Declaration")))))

;; Use `org-export-coding-system' instead
;; (defcustom org-e-html-coding-system nil
;;   "Coding system for HTML export, defaults to `buffer-file-coding-system'."
;;   :group 'org-export-e-html
;;   :type 'coding-system)

(defvar org-e-html-content-div "content"
  "The name of the container DIV that holds all the page contents.

This variable is obsolete since Org version 7.7.
Please set `org-e-html-divs' instead.")

(defcustom org-e-html-divs '("preamble" "content" "postamble")
  "The name of the main divs for HTML export.
This is a list of three strings, the first one for the preamble
DIV, the second one for the content DIV and the third one for the
postamble DIV."
  :group 'org-export-e-html
  :type '(list
	  (string :tag " Div for the preamble:")
	  (string :tag "  Div for the content:")
	  (string :tag "Div for the postamble:")))


;;;; Document Header (Styles)

(defconst org-e-html-style-default
"<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  html { font-family: Times, serif; font-size: 12pt; }
  .title  { text-align: center; }
  .todo   { color: red; }
  .done   { color: green; }
  .tag    { background-color: #add8e6; font-weight:normal }
  .target { }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .right  {margin-left:auto; margin-right:0px;  text-align:right;}
  .left   {margin-left:0px;  margin-right:auto; text-align:left;}
  .center {margin-left:auto; margin-right:auto; text-align:center;}
  p.verse { margin-left: 3% }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
        font-size: 90%;
        overflow:auto;
  }
  table { border-collapse: collapse; }
  td, th { vertical-align: top;  }
  th.right  { text-align:center;  }
  th.left   { text-align:center;   }
  th.center { text-align:center; }
  td.right  { text-align:right;  }
  td.left   { text-align:left;   }
  td.center { text-align:center; }
  dt { font-weight: bold; }
  div.figure { padding: 0.5em; }
  div.figure p { text-align: center; }
  div.inlinetask {
    padding:10px;
    border:2px solid gray;
    margin:10px;
    background: #ffffcc;
  }
  textarea { overflow-x: auto; }
  .linenr { font-size:smaller }
  .code-highlighted {background-color:#ffff00;}
  .org-info-js_info-navigation { border-style:none; }
  #org-info-js_console-label { font-size:10px; font-weight:bold;
                               white-space:nowrap; }
  .org-info-js_search-highlight {background-color:#ffff00; color:#000000;
                                 font-weight:bold; }
  /*]]>*/-->
</style>"
  "The default style specification for exported HTML files.
Please use the variables `org-e-html-style' and
`org-e-html-style-extra' to add to this style.  If you wish to not
have the default style included, customize the variable
`org-e-html-style-include-default'.")

(defcustom org-e-html-style-include-default t
  "Non-nil means include the default style in exported HTML files.
The actual style is defined in `org-e-html-style-default' and should
not be modified.  Use the variables `org-e-html-style' to add
your own style information."
  :group 'org-export-e-html
  :type 'boolean)
;;;###autoload
(put 'org-e-html-style-include-default 'safe-local-variable 'booleanp)

(defcustom org-e-html-style ""
  "Org-wide style definitions for exported HTML files.

This variable needs to contain the full HTML structure to provide a style,
including the surrounding HTML tags.  If you set the value of this variable,
you should consider to include definitions for the following classes:
 title, todo, done, timestamp, timestamp-kwd, tag, target.

For example, a valid value would be:

   <style type=\"text/css\">
    <![CDATA[
       p { font-weight: normal; color: gray; }
       h1 { color: black; }
      .title { text-align: center; }
      .todo, .timestamp-kwd { color: red; }
      .done { color: green; }
    ]]>
   </style>

If you'd like to refer to an external style file, use something like

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">

As the value of this option simply gets inserted into the HTML <head> header,
you can \"misuse\" it to add arbitrary text to the header.
See also the variable `org-e-html-style-extra'."
  :group 'org-export-e-html
  :type 'string)
;;;###autoload
(put 'org-e-html-style 'safe-local-variable 'stringp)

(defcustom org-e-html-style-extra ""
  "Additional style information for HTML export.
The value of this variable is inserted into the HTML buffer right after
the value of `org-e-html-style'.  Use this variable for per-file
settings of style information, and do not forget to surround the style
settings with <style>...</style> tags."
  :group 'org-export-e-html
  :type 'string)
;;;###autoload
(put 'org-e-html-style-extra 'safe-local-variable 'stringp)

(defcustom org-e-html-mathjax-options
  '((path  "http://orgmode.org/mathjax/MathJax.js")
    (scale "100")
    (align "center")
    (indent "2em")
    (mathml nil))
  "Options for MathJax setup.

path        The path where to find MathJax
scale       Scaling for the HTML-CSS backend, usually between 100 and 133
align       How to align display math: left, center, or right
indent      If align is not center, how far from the left/right side?
mathml      Should a MathML player be used if available?
            This is faster and reduces bandwidth use, but currently
            sometimes has lower spacing quality.  Therefore, the default is
            nil.  When browsers get better, this switch can be flipped.

You can also customize this for each buffer, using something like

#+MATHJAX: scale:\"133\" align:\"right\" mathml:t path:\"/MathJax/\""
  :group 'org-export-e-html
  :type '(list :greedy t
	      (list :tag "path   (the path from where to load MathJax.js)"
		    (const :format "       " path) (string))
	      (list :tag "scale  (scaling for the displayed math)"
		    (const :format "       " scale) (string))
	      (list :tag "align  (alignment of displayed equations)"
		    (const :format "       " align) (string))
	      (list :tag "indent (indentation with left or right alignment)"
		    (const :format "       " indent) (string))
	      (list :tag "mathml (should MathML display be used is possible)"
		    (const :format "       " mathml) (boolean))))


;;;; Document Header (Scripts)

(defcustom org-e-html-style-include-scripts t
  "Non-nil means include the JavaScript snippets in exported HTML files.
The actual script is defined in `org-e-html-scripts' and should
not be modified."
  :group 'org-export-e-html
  :type 'boolean)

(defconst org-e-html-scripts
"<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
 function CodeHighlightOn(elem, id)
 {
   var target = document.getElementById(id);
   if(null != target) {
     elem.cacheClassElem = elem.className;
     elem.cacheClassTarget = target.className;
     target.className = \"code-highlighted\";
     elem.className   = \"code-highlighted\";
   }
 }
 function CodeHighlightOff(elem, id)
 {
   var target = document.getElementById(id);
   if(elem.cacheClassElem)
     elem.className = elem.cacheClassElem;
   if(elem.cacheClassTarget)
     target.className = elem.cacheClassTarget;
 }
/*]]>*///-->
</script>"
"Basic JavaScript that is needed by HTML files produced by Org-mode.")


;;;; Document Header (Mathjax)

(defcustom org-e-html-mathjax-template
  "<script type=\"text/javascript\" src=\"%PATH\">
<!--/*--><![CDATA[/*><!--*/
    MathJax.Hub.Config({
        // Only one of the two following lines, depending on user settings
        // First allows browser-native MathML display, second forces HTML/CSS
        :MMLYES: config: [\"MMLorHTML.js\"], jax: [\"input/TeX\"],
        :MMLNO: jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [\"STIX\",\"TeX\"],
             preferredFont: \"TeX\",
             webFont: \"TeX\",
             imageFont: \"TeX\",
             showMathMenu: true,
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        }
    });
/*]]>*///-->
</script>"
  "The MathJax setup for XHTML files."
  :group 'org-export-e-html
  :type 'string)


;;;; Preamble

(defcustom org-e-html-preamble t
  "Non-nil means insert a preamble in HTML export.

When `t', insert a string as defined by one of the formatting
strings in `org-e-html-preamble-format'.  When set to a
string, this string overrides `org-e-html-preamble-format'.
When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting :html-preamble in publishing projects will take
precedence over this variable."
  :group 'org-export-e-html
  :type '(choice (const :tag "No preamble" nil)
		 (const :tag "Default preamble" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-e-html-preamble-format '(("en" ""))
  "The format for the HTML preamble.

%t stands for the title.
%a stands for the author's name.
%e stands for the author's email.
%d stands for the date.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-home/up-format
  "<div id=\"org-div-home-and-up\" style=\"text-align:right;font-size:70%%;white-space:nowrap;\">
 <a accesskey=\"h\" href=\"%s\"> UP </a>
 |
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
  "Snippet used to insert the HOME and UP links.
This is a format string, the first %s will receive the UP link,
the second the HOME link.  If both `org-e-html-link-up' and
`org-e-html-link-home' are empty, the entire snippet will be
ignored."
  :group 'org-export-e-html
  :type 'string)

;;;; Postamble

(defcustom org-e-html-postamble 'auto
  "Non-nil means insert a postamble in HTML export.

When `t', insert a string as defined by the formatting string in
`org-e-html-postamble-format'.  When set to a string, this
string overrides `org-e-html-postamble-format'.  When set to
'auto, discard `org-e-html-postamble-format' and honor
`org-export-author/email/creator-info' variables.  When set to a
function, apply this function and insert the returned string.
The function takes the property list of export options as its
only argument.

Setting :html-postamble in publishing projects will take
precedence over this variable."
  :group 'org-export-e-html
  :type '(choice (const :tag "No postamble" nil)
		 (const :tag "Auto preamble" 'auto)
		 (const :tag "Default formatting string" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-e-html-postamble-format
  '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">Generated by %c</p>
<p class=\"xhtml-validation\">%v</p>
"))
  "The format for the HTML postamble.

%a stands for the author's name.
%e stands for the author's email.
%d stands for the date.
%c will be replaced by information about Org/Emacs versions.
%v will be replaced by `org-e-html-validation-link'.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-validation-link
  "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"
  "Link to HTML validation service."
  :group 'org-export-e-html
  :type 'string)

;; FIXME Obsolete since Org 7.7
;; Use the :timestamp option or `org-export-time-stamp-file' instead
;;;; Emphasis

(defcustom org-e-html-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-e-html-protect'."
  :group 'org-export-e-html
  :type '(repeat (cons (string :tag "Character")
		       (string :tag "HTML equivalent"))))

(defconst org-e-html-special-string-regexps
  '(("\\\\-" . "&shy;")
    ("---\\([^-]\\)" . "&mdash;\\1")
    ("--\\([^-]\\)" . "&ndash;\\1")
    ("\\.\\.\\." . "&hellip;"))
  "Regular expressions for special string conversion.")


;;;; Todos

(defcustom org-e-html-todo-kwd-class-prefix ""
  "Prefix to class names for TODO keywords.
Each TODO keyword gets a class given by the keyword itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-e-html
  :type 'string)


;;;; Tags

(defcustom org-e-html-tag-class-prefix ""
  "Prefix to class names for TODO keywords.
Each tag gets a class given by the tag itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-e-html
  :type 'string)

;;;; Time-stamps
;;;; Statistics Cookie
;;;; Subscript
;;;; Superscript

;;;; Inline images

(defcustom org-e-html-inline-images 'maybe
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image.  If this option is `maybe', then images in links with
an empty description will be inlined, while images with a description will
be linked only."
  :group 'org-export-e-html
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When there is no description" maybe)))

(defcustom org-e-html-inline-image-extensions
  '("png" "jpeg" "jpg" "gif" "svg")
  "Extensions of image files that can be inlined into HTML."
  :group 'org-export-e-html
  :type '(repeat (string :tag "Extension")))


;;;; Block
;;;; Comment
;;;; Comment Block
;;;; Drawer
;;;; Dynamic Block
;;;; Emphasis
;;;; Entity
;;;; Example Block
;;;; Export Snippet
;;;; Export Block
;;;; Fixed Width
;;;; Footnotes

(defcustom org-e-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'org-export-e-html
  :type 'string)


;;;; Headline
;;;; Horizontal Rule
;;;; Inline Babel Call
;;;; Inline Src Block
;;;; Inlinetask
;;;; Item
;;;; Keyword
;;;; Latex Environment
;;;; Latex Fragment
;;;; Line Break
;;;; Link
;;;; Babel Call
;;;; Macro
;;;; Paragraph
;;;; Plain List
;;;; Plain Text
;;;; Property Drawer
;;;; Quote Block
;;;; Quote Section
;;;; Section
;;;; Radio Target
;;;; Special Block
;;;; Src Block

(defgroup org-export-e-htmlize nil
  "Options for processing examples with htmlize.el."
  :tag "Org Export Htmlize"
  :group 'org-export-e-html)

(defcustom org-export-e-htmlize-output-type 'inline-css
  "Output type to be used by htmlize when formatting code snippets.
Choices are `css', to export the CSS selectors only, or `inline-css', to
export the CSS attribute values inline in the HTML.  We use as default
`inline-css', in order to make the resulting HTML self-containing.

However, this will fail when using Emacs in batch mode for export, because
then no rich font definitions are in place.  It will also not be good if
people with different Emacs setup contribute HTML files to a website,
because the fonts will represent the individual setups.  In these cases,
it is much better to let Org/Htmlize assign classes only, and to use
a style file to define the look of these classes.
To get a start for your css file, start Emacs session and make sure that
all the faces you are interested in are defined, for example by loading files
in all modes you want.  Then, use the command
\\[org-export-e-htmlize-generate-css] to extract class definitions."
  :group 'org-export-e-htmlize
  :type '(choice (const css) (const inline-css)))

(defcustom org-export-e-htmlize-css-font-prefix "org-"
  "The prefix for CSS class names for htmlize font specifications."
  :group 'org-export-e-htmlize
  :type 'string)

(defcustom org-export-e-htmlized-org-css-url nil
  "URL pointing to a CSS file defining text colors for htmlized Emacs buffers.
Normally when creating an htmlized version of an Org buffer, htmlize will
create CSS to define the font colors.  However, this does not work when
converting in batch mode, and it also can look bad if different people
with different fontification setup work on the same website.
When this variable is non-nil, creating an htmlized version of an Org buffer
using `org-export-as-org' will remove the internal CSS section and replace it
with a link to this URL."
  :group 'org-export-e-htmlize
  :type '(choice
	  (const :tag "Keep internal css" nil)
	  (string :tag "URL or local href")))


;;;; Table

(defcustom org-e-html-table-tag
  "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">"
  "The HTML tag that is used to start a table.
This must be a <table> tag, but you may change the options like
borders and spacing."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-table-header-tags '("<th scope=\"%s\"%s>" . "</th>")
  "The opening tag for table header fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-e-html-table-use-header-tags-for-first-column'.
See also the variable `org-e-html-table-align-individual-fields'."
  :group 'org-export-tables		; FIXME: change group?
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-e-html-table-data-tags '("<td%s>" . "</td>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-e-html-table-align-individual-fields'."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-e-html-table-row-tags '("<tr>" . "</tr>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
Instead of strings, these can be Lisp forms that will be evaluated
for each row in order to construct the table row tags.  During evaluation,
the variable `head' will be true when this is a header line, nil when this
is a body line.  And the variable `nline' will contain the line number,
starting from 1 in the first header line.  For example

  (setq org-e-html-table-row-tags
        (cons '(if head
                   \"<tr>\"
                 (if (= (mod nline 2) 1)
                     \"<tr class=\\\"tr-odd\\\">\"
                   \"<tr class=\\\"tr-even\\\">\"))
              \"</tr>\"))

will give even lines the class \"tr-even\" and odd lines the class \"tr-odd\"."
  :group 'org-export-tables
  :type '(cons
	  (choice :tag "Opening tag"
		  (string :tag "Specify")
		  (sexp))
	  (choice :tag "Closing tag"
		  (string :tag "Specify")
		  (sexp))))

(defcustom org-e-html-table-align-individual-fields t
  "Non-nil means attach style attributes for alignment to each table field.
When nil, alignment will only be specified in the column tags, but this
is ignored by some browsers (like Firefox, Safari).  Opera does it right
though."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-e-html-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'org-export-tables
  :type 'boolean)


;;;; Target
;;;; Time-stamp

;;;; Verbatim
;;;; Verse Block
;;;; Headline

(defcustom org-e-html-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title."
  :group 'org-export-e-html
  :type 'string)


;;;; Links
;;;; Drawers
;;;; Inlinetasks
;;;; Publishing

(defcustom org-e-html-link-org-files-as-html t
  "Non-nil means make file links to `file.org' point to `file.html'.
When org-mode is exporting an org-mode file to HTML, links to
non-html files are directly put into a href tag in HTML.
However, links to other Org-mode files (recognized by the
extension `.org.) should become links to the corresponding html
file, assuming that the linked org-mode file will also be
converted to HTML.
When nil, the links still point to the plain `.org' file."
  :group 'org-export-e-html
  :type 'boolean)


;;;; Compilation



;;; User Configurable Variables (MAYBE)

;;;; Preamble

(defcustom org-e-html-date-format
  "\\today"
  "Format string for \\date{...}."
  :group 'org-export-e-html
  :type 'boolean)

;;;; Headline

(defcustom org-e-html-format-headline-function nil
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

\(defun org-e-html-format-headline \(todo todo-type priority text tags\)
  \"Default format function for an headline.\"
  \(concat \(when todo
            \(format \"\\\\textbf{\\\\textsc{\\\\textsf{%s}}} \" todo\)\)
	  \(when priority
            \(format \"\\\\framebox{\\\\#%c} \" priority\)\)
	  text
	  \(when tags \(format \"\\\\hfill{}\\\\textsc{%s}\" tags\)\)\)\)"
  :group 'org-export-e-html
  :type 'function)

;;;; Emphasis

(defcustom org-e-html-emphasis-alist
  '(("*" . "<b>%s</b>")
    ("/" . "<i>%s</i>")
    ("_" . "<span style=\"text-decoration:underline;\">%s</span>")
    ("+" . "<del>%s</del>")
    ("=" . "<code>%s</code>")
    ("~" . "<code>%s</code>"))
  "Alist of HTML expressions to convert emphasis fontifiers.

The key is the character used as a marker for fontification.  The
value is a formatting string to wrap fontified text with.

Value can also be set to the following symbols: `verb' and
`protectedtexttt'.  For the former, Org will use \"\\verb\" to
create a format string and select a delimiter character that
isn't in the string.  For the latter, Org will use \"\\texttt\"
to typeset and try to protect special characters."
  :group 'org-export-e-html
  :type 'alist)


;;;; Footnotes

(defcustom org-e-html-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'org-export-e-html
  :type 'string)


;;;; Time-stamps

(defcustom org-e-html-active-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to active time-stamps."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-inactive-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to inactive time-stamps."
  :group 'org-export-e-html
  :type 'string)

(defcustom org-e-html-diary-timestamp-format "\\textit{%s}"
  "A printf format string to be applied to diary time-stamps."
  :group 'org-export-e-html
  :type 'string)


;;;; Links

(defcustom org-e-html-inline-image-rules
  '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
    ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
    ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'"))
  "Rules characterizing image files that can be inlined into HTML.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path.

Note that, by default, the image extension *actually* allowed
depend on the way the HTML file is processed.  When used with
pdflatex, pdf, jpg and png images are OK.  When processing
through dvi to Postscript, only ps and eps are allowed.  The
default we use here encompasses both."
  :group 'org-export-e-html
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))

;;;; Tables

(defcustom org-e-html-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-e-html
  :type 'boolean)

;;;; Drawers

(defcustom org-e-html-format-drawer-function nil
  "Function called to format a drawer in HTML code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-html-format-drawer-default \(name contents\)
  \"Format a drawer element for HTML export.\"
  contents\)"
  :group 'org-export-e-html
  :type 'function)


;;;; Inlinetasks

(defcustom org-e-html-format-inlinetask-function nil
  "Function called to format an inlinetask in HTML code.

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

\(defun org-e-html-format-inlinetask \(todo type priority name tags contents\)
\"Format an inline task element for HTML export.\"
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
  :group 'org-export-e-html
  :type 'function)


;; Src blocks

;;;; Plain text

(defcustom org-e-html-quotes
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
  :group 'org-export-e-html
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



;;; Internal Functions (HTML)

(defun org-e-html-cvt-org-as-html (opt-plist type path)
  "Convert an org filename to an equivalent html filename.
If TYPE is not file, just return `nil'.
See variable `org-e-html-link-org-files-as-html'."
   (save-match-data
      (and
	 org-e-html-link-org-files-as-html
	 (string= type "file")
	 (string-match "\\.org$" path)
	 (progn
	    (list
	       "file"
	       (concat
		  (substring path 0 (match-beginning 0))
		  "." (plist-get opt-plist :html-extension)))))))

(defun org-e-html-format-org-link (opt-plist type-1 path fragment desc attr
					     descp)
  "Make an HTML link.
OPT-PLIST is an options list.
TYPE is the device-type of the link (THIS://foo.html).
PATH is the path of the link (http://THIS#location).
FRAGMENT is the fragment part of the link, if any (foo.html#THIS).
DESC is the link description, if any.
ATTR is a string of other attributes of the \"a\" element."
  (declare (special org-lparse-par-open))
  (save-match-data
    (let* ((may-inline-p
	    (and (member type-1 '("http" "https" "file"))
		 (org-lparse-should-inline-p path descp)
		 (not fragment)))
	   (type (if (equal type-1 "id") "file" type-1))
	   (filename path)
	   ;;First pass.  Just sanity stuff.
	   (components-1
	    (cond
	     ((string= type "file")
	      (list
	       type
	       ;;Substitute just if original path was absolute.
	       ;;(Otherwise path must remain relative)
	       (if (file-name-absolute-p path)
		   (concat "file://" (expand-file-name path))
		 path)))
	     ((string= type "")
	      (list nil path))
	     (t (list type path))))

	   ;;Second pass.  Components converted so they can refer
	   ;;to a remote site.
	   (components-2
	    (or
	     (and org-e-html-cvt-link-fn
		  (apply org-e-html-cvt-link-fn
			 opt-plist components-1))
	     (apply #'org-e-html-cvt-org-as-html
		    opt-plist components-1)
	     components-1))
	   (type    (first  components-2))
	   (thefile (second components-2)))


      ;;Third pass.  Build final link except for leading type
      ;;spec.
      (cond
       ((or
	 (not type)
	 (string= type "http")
	 (string= type "https")
	 (string= type "file")
	 (string= type "coderef"))
	(if fragment
	    (setq thefile (concat thefile "#" fragment))))

       (t))

      ;;Final URL-build, for all types.
      (setq thefile
	    (let
		((str (org-xml-format-href thefile)))
	      (if (and type (not (or (string= "file" type)
				     (string= "coderef" type))))
		  (concat type ":" str)
		str)))

      (if may-inline-p
	  (ignore) ;; (org-e-html-format-image thefile)
	(org-lparse-format
	 'LINK (org-xml-format-desc desc) thefile attr)))))

;; (caption (and caption (org-xml-encode-org-text caption)))
;; alt = (file-name-nondirectory path)

(defun org-e-html-format-inline-image (src &optional
					   caption label attr standalone-p)
  (let* ((id (if (not label) ""
	       (format " id=\"%s\"" (org-export-solidify-link-text label))))
	 (attr (concat attr
		       (cond
			((string-match "\\<alt=" (or attr "")) "")
			((string-match "^ltxpng/" src)
			 (format " alt=\"%s\""
				 (org-e-html-encode-plain-text
				  (org-find-text-property-in-string
				   'org-latex-src src))))
			(t (format " alt=\"%s\""
				   (file-name-nondirectory src)))))))
    (cond
     (standalone-p
      (let ((img (format "<img src=\"%s\" %s/>" src attr)))
	(format "\n<div%s class=\"figure\">%s%s\n</div>"
		id (format "\n<p>%s</p>" img)
		(when caption (format "\n<p>%s</p>" caption)))))
     (t (format "<img src=\"%s\" %s/>" src (concat attr id))))))

;;;; Bibliography

(defun org-e-html-bibliography ()
  "Find bibliography, cut it out and return it."
  (catch 'exit
    (let (beg end (cnt 1) bib)
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward
	       "^[ \t]*<div \\(id\\|class\\)=\"bibliography\"" nil t)
	  (setq beg (match-beginning 0))
	  (while (re-search-forward "</?div\\>" nil t)
	    (setq cnt (+ cnt (if (string= (match-string 0) "<div") +1 -1)))
	    (when (= cnt 0)
	      (and (looking-at ">") (forward-char 1))
	      (setq bib (buffer-substring beg (point)))
	      (delete-region beg (point))
	    (throw 'exit bib))))
	nil))))

;;;; Table

(defun org-e-html-format-table (lines olines)
  (let ((org-e-html-format-table-no-css nil))
    (org-lparse-format-table lines olines)))

(defun org-e-html-splice-attributes (tag attributes)
  "Read attributes in string ATTRIBUTES, add and replace in HTML tag TAG."
  (if (not attributes)
      tag
    (let (oldatt newatt)
      (setq oldatt (org-extract-attributes-from-string tag)
	    tag (pop oldatt)
	    newatt (cdr (org-extract-attributes-from-string attributes)))
      (while newatt
	(setq oldatt (plist-put oldatt (pop newatt) (pop newatt))))
      (if (string-match ">" tag)
	  (setq tag
		(replace-match (concat (org-attributes-to-string oldatt) ">")
			       t t tag)))
      tag)))

(defun org-export-splice-style (style extra)
  "Splice EXTRA into STYLE, just before \"</style>\"."
  (if (and (stringp extra)
	   (string-match "\\S-" extra)
	   (string-match "</style>" style))
      (concat (substring style 0 (match-beginning 0))
	      "\n" extra "\n"
	      (substring style (match-beginning 0)))
    style))

(defun org-export-e-htmlize-region-for-paste (beg end)
  "Convert the region to HTML, using htmlize.el.
This is much like `htmlize-region-for-paste', only that it uses
the settings define in the org-... variables."
  (let* ((htmlize-output-type org-export-e-htmlize-output-type)
	 (htmlize-css-name-prefix org-export-e-htmlize-css-font-prefix)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

;;;###autoload
(defun org-export-e-htmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-export-e-htmlize-output-type' to `css', calls to
the function `org-export-e-htmlize-region-for-paste' will produce code
that uses these same face definitions."
  (interactive)
  (require 'htmlize)
  (and (get-buffer "*html*") (kill-buffer "*html*"))
  (with-temp-buffer
    (let ((fl (face-list))
	  (htmlize-css-name-prefix "org-")
	  (htmlize-output-type 'css)
	  f i)
      (while (setq f (pop fl)
		   i (and f (face-attribute f :inherit)))
	(when (and (symbolp f) (or (not i) (not (listp i))))
	  (insert (org-add-props (copy-sequence "1") nil 'face f))))
      (htmlize-region (point-min) (point-max))))
  (org-pop-to-buffer-same-window "*html*")
  (goto-char (point-min))
  (if (re-search-forward "<style" nil t)
      (delete-region (point-min) (match-beginning 0)))
  (if (re-search-forward "</style>" nil t)
      (delete-region (1+ (match-end 0)) (point-max)))
  (beginning-of-line 1)
  (if (looking-at " +") (replace-match ""))
  (goto-char (point-min)))

(defun org-e-html-make-string (n string)
  (let (out) (dotimes (i n out) (setq out (concat string out)))))

(defun org-e-html-toc-text (toc-entries)
  (let* ((prev-level (1- (nth 1 (car toc-entries))))
	 (start-level prev-level))
    (concat
     (mapconcat
      (lambda (entry)
	(let ((headline (nth 0 entry))
	      (level (nth 1 entry)))
	  (concat
	   (let* ((cnt (- level prev-level))
		  (times (if (> cnt 0) (1- cnt) (- cnt)))
		  rtn)
	     (setq prev-level level)
	     (concat
	      (org-e-html-make-string
	       times (cond ((> cnt 0) "\n<ul>\n<li>")
			   ((< cnt 0) "</li>\n</ul>\n")))
	      (if (> cnt 0) "\n<ul>\n<li>" "</li>\n<li>")))
	   headline)))
      toc-entries "")
     (org-e-html-make-string
      (- prev-level start-level) "</li>\n</ul>\n"))))

(defun* org-e-html-format-toc-headline
    (todo todo-type priority text tags
	  &key level section-number headline-label &allow-other-keys)
  (let ((headline (concat
		   section-number (and section-number ". ")
		   text
		   (and tags "&nbsp;&nbsp;&nbsp;") (org-e-html--tags tags))))
    (format "<a href=\"#%s\">%s</a>"
	    headline-label
	    (if (not nil) headline
	      (format "<span class=\"%s\">%s</span>" todo-type headline)))))

(defun org-e-html-toc (depth info)
  (assert (wholenump depth))
  (let* ((headlines (org-export-collect-headlines info depth))
	 (toc-entries
	  (loop for headline in headlines collect
		(list (org-e-html-format-headline--wrap
		       headline info 'org-e-html-format-toc-headline)
		      (org-export-get-relative-level headline info)))))
    (when toc-entries
      (let* ((lang-specific-heading
	      (nth 3 (or (assoc (plist-get info :language)
				org-export-language-setup)
			 (assoc "en" org-export-language-setup)))))
	(concat
	 "<div id=\"table-of-contents\">\n"
	 (format "<h%d>%s</h%d>\n"
		 org-e-html-toplevel-hlevel
		 lang-specific-heading
		 org-e-html-toplevel-hlevel)
	 "<div id=\"text-table-of-contents\">"
	 (org-e-html-toc-text toc-entries)
	 "</div>\n"
	 "</div>\n")))))

;; (defun org-e-html-format-line (line)
;;   (case org-lparse-dyn-current-environment
;;     ((quote fixedwidth) (concat (org-e-html-encode-plain-text line) "\n"))
;;     (t (concat line "\n"))))

(defun org-e-html-fix-class-name (kwd) 	; audit callers of this function
  "Turn todo keyword into a valid class name.
Replaces invalid characters with \"_\"."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" kwd)
      (setq kwd (replace-match "_" t t kwd))))
  kwd)

(defun org-e-html-format-footnote-reference (n def refcnt)
  (let ((extra (if (= refcnt 1) "" (format ".%d"  refcnt))))
    (format org-e-html-footnote-format
	    (format
	     "<a class=\"footref\" name=\"fnr.%s%s\" href=\"#fn.%s\">%s</a>"
	     n extra n n))))

(defun org-e-html-format-footnotes-section (section-name definitions)
  (if (not definitions) ""
    (format org-e-html-footnotes-section section-name definitions)))

(defun org-e-html-format-footnote-definition (fn)
  (let ((n (car fn)) (def (cdr fn)))
    (format
     "<tr>\n<td>%s</td>\n<td>%s</td>\n</tr>\n"
     (format
      (format org-e-html-footnote-format
	      "<a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a>")
      n n n) def)))

(defun org-e-html-footnote-section (info)
  (let* ((fn-alist (org-export-collect-footnote-definitions
		    (plist-get info :parse-tree) info))

	 (fn-alist
	  (loop for (n type raw) in fn-alist collect
		(cons n (if (equal (org-element-type raw) 'org-data)
			    (org-trim (org-export-data raw 'e-html info))
			  (format "<p>%s</p>"
				  (org-trim (org-export-secondary-string
					     raw 'e-html info))))))))
    (when fn-alist
      (org-e-html-format-footnotes-section
       (nth 4 (or (assoc (plist-get info :language)
			 org-export-language-setup)
		  (assoc "en" org-export-language-setup)))
       (format
	"<table>\n%s\n</table>\n"
	(mapconcat 'org-e-html-format-footnote-definition fn-alist "\n"))))))

(defun org-e-html-format-date (info)
  (let ((date (plist-get info :date)))
    (cond
     ((and date (string-match "%" date))
      (format-time-string date))
     (date date)
     (t (format-time-string "%Y-%m-%d %T %Z")))))



;;; Internal Functions (Ngz)

(defun org-e-html--caption/label-string (caption label info)
  "Return caption and label HTML string for floats.

CAPTION is a cons cell of secondary strings, the car being the
standard caption and the cdr its short form.  LABEL is a string
representing the label.  INFO is a plist holding contextual
information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-e-html--wrap-label'."
  (setq label nil) ;; FIXME

  (let ((label-str (if label (format "\\label{%s}" label) "")))
    (cond
     ((and (not caption) (not label)) "")
     ((not caption) (format "\\label{%s}\n" label))
     ;; Option caption format with short name.
     ((cdr caption)
      (format "\\caption[%s]{%s%s}\n"
	      (org-export-secondary-string (cdr caption) 'e-html info)
	      label-str
	      (org-export-secondary-string (car caption) 'e-html info)))
     ;; Standard caption format.
     ;; (t (format "\\caption{%s%s}\n"
     ;; 		label-str
     ;; 		(org-export-secondary-string (car caption) 'e-html info)))

     (t (org-export-secondary-string (car caption) 'e-html info)))))

(defun org-e-html--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (loop for c across ll
	  when (not (string-match (regexp-quote (char-to-string c)) s))
	  return (char-to-string c))))

(defun org-e-html--quotation-marks (text info)
  "Export quotation marks depending on language conventions.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
	  (let ((start 0))
	    (while (setq start (string-match (car l) text start))
	      (let ((new-quote (concat (match-string 1 text) (cdr l))))
		(setq text (replace-match new-quote  t t text))))))
	(cdr (or (assoc (plist-get info :language) org-e-html-quotes)
		 ;; Falls back on English.
		 (assoc "en" org-e-html-quotes))))
  text)

(defun org-e-html--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-e-html--caption/label-string'."
  ;; (let ((label (org-element-property :name element)))
  ;;   (if (or (not output) (not label) (string= output "") (string= label ""))
  ;; 	output
  ;;     (concat (format "\\label{%s}\n" label) output)))
  output)



;;; Template

(defun org-e-html-meta-info (info)
  (let* ((title (org-export-secondary-string
		 (plist-get info :title) 'e-html info))
	 (author (and (plist-get info :with-author)
		      (let ((auth (plist-get info :author)))
			(and auth (org-export-secondary-string
				   auth 'e-html info)))))
	 (description (plist-get info :description))
	 (keywords (plist-get info :keywords)))
    (concat
     (format "\n<title>%s</title>\n" title)
     (format
      "\n<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>"
      (or (and org-export-coding-system
	       (fboundp 'coding-system-get)
	       (coding-system-get org-export-coding-system
				  'mime-charset))
	  "iso-8859-1"))
     (format "\n<meta name=\"title\" content=\"%s\"/>" title)
     (format "\n<meta name=\"generator\" content=\"Org-mode\"/>")
     (format "\n<meta name=\"generated\" content=\"%s\"/>"
	     (org-e-html-format-date info))
     (format "\n<meta name=\"author\" content=\"%s\"/>" author)
     (format "\n<meta name=\"description\" content=\"%s\"/>" description)
     (format "\n<meta name=\"keywords\" content=\"%s\"/>" keywords))))

(defun org-e-html-style (info)
  (concat
   "\n" (when (plist-get info :style-include-default) org-e-html-style-default)
   (plist-get info :style)
   (plist-get info :style-extra)
   "\n"
   (when (plist-get info :style-include-scripts)
     org-e-html-scripts)))

(defun org-e-html-mathjax-config (info)
  "Insert the user setup into the matchjax template."
  (when (member (plist-get info :LaTeX-fragments) '(mathjax t))
    (let ((template org-e-html-mathjax-template)
	  (options org-e-html-mathjax-options)
	  (in-buffer (or (plist-get info :mathjax) ""))
	  name val (yes "   ") (no "// ") x)
      (mapc
       (lambda (e)
	 (setq name (car e) val (nth 1 e))
	 (if (string-match (concat "\\<" (symbol-name name) ":") in-buffer)
	     (setq val (car (read-from-string
			     (substring in-buffer (match-end 0))))))
	 (if (not (stringp val)) (setq val (format "%s" val)))
	 (if (string-match (concat "%" (upcase (symbol-name name))) template)
	     (setq template (replace-match val t t template))))
       options)
      (setq val (nth 1 (assq 'mathml options)))
      (if (string-match (concat "\\<mathml:") in-buffer)
	  (setq val (car (read-from-string
			  (substring in-buffer (match-end 0))))))
      ;; Exchange prefixes depending on mathml setting
      (if (not val) (setq x yes yes no no x))
      ;; Replace cookies to turn on or off the config/jax lines
      (if (string-match ":MMLYES:" template)
	  (setq template (replace-match yes t t template)))
      (if (string-match ":MMLNO:" template)
	  (setq template (replace-match no t t template)))
      ;; Return the modified template
      template)))

(defun org-e-html-preamble (info)
  (when (plist-get info :html-preamble)
    (let* ((title (org-export-secondary-string
		   (plist-get info :title) 'e-html info))
	   (date (org-e-html-format-date info))
	   (author (org-export-secondary-string
		    (plist-get info :author) 'e-html info))
	   (lang-words (or (assoc (plist-get info :language)
				  org-export-language-setup)
			   (assoc "en" org-export-language-setup)))
	   (email (plist-get info :email))
	   (html-pre-real-contents
	    (cond
	     ((functionp (plist-get info :html-preamble))
	      (with-temp-buffer
		(funcall (plist-get info :html-preamble))
		(buffer-string)))
	     ((stringp (plist-get info :html-preamble))
	      (format-spec (plist-get info :html-preamble)
			   `((?t . ,title) (?a . ,author)
			     (?d . ,date) (?e . ,email))))
	     (t
	      (format-spec
	       (or (cadr (assoc (nth 0 lang-words)
				org-e-html-preamble-format))
		   (cadr (assoc "en" org-e-html-preamble-format)))
	       `((?t . ,title) (?a . ,author)
		 (?d . ,date) (?e . ,email)))))))
      (when (not (equal html-pre-real-contents ""))
	(concat
	 (format "
<div id=\"%s\"> "  (nth 0 org-e-html-divs))
	 "
"
	 html-pre-real-contents
	 "
</div>")))))

(defun org-e-html-postamble (info)
  (concat
   (when (and (not body-only)
	      (plist-get info :html-postamble))
     (let* ((html-post (plist-get info :html-postamble))
	    (date (org-e-html-format-date info))
	    (author (plist-get info :author))
	    (email  (plist-get info :email))
	    (lang-words (or (assoc (plist-get info :language)
				   org-export-language-setup)
			    (assoc "en" org-export-language-setup)))
	    (email
	     (mapconcat (lambda(e)
			  (format "<a href=\"mailto:%s\">%s</a>" e e))
			(split-string email ",+ *")
			", "))
	    (html-validation-link (or org-e-html-validation-link ""))
	    (creator-info org-export-creator-string))
       (concat
	;; begin postamble
	"
<div id=\"" (nth 2 org-e-html-divs) "\">"
	(cond
	 ;; auto postamble
	 ((eq (plist-get info :html-postamble) 'auto)
	  (concat
	   (when (plist-get info :time-stamp-file)
	     (format "
<p class=\"date\"> %s: %s </p> "  (nth 2 lang-words) date))
	   (when (and (plist-get info :with-author) author)
	     (format "
<p class=\"author\"> %s : %s</p>"  (nth 1 lang-words) author))
	   (when (and (plist-get info :with-email) email)
	     (format "
<p class=\"email\"> %s </p>" email))
	   (when (plist-get info :with-creator)
	     (format "
<p class=\"creator\"> %s </p>"  creator-info))
	   html-validation-link "\n"))
	 ;; postamble from a string
	 ((stringp (plist-get info :html-postamble))
	  (format-spec (plist-get info :html-postamble)
		       `((?a . ,author) (?e . ,email)
			 (?d . ,date)   (?c . ,creator-info)
			 (?v . ,html-validation-link))))

	 ;; postamble from a function
	 ((functionp (plist-get info :html-postamble))
	  (with-temp-buffer
	    (funcall (plist-get info :html-postamble))
	    (buffer-string)))
	 ;; default postamble
	 (t
	  (format-spec
	   (or (cadr (assoc (nth 0 lang-words)
			    org-e-html-postamble-format))
	       (cadr (assoc "en" org-e-html-postamble-format)))
	   `((?a . ,author) (?e . ,email)
	     (?d . ,date)   (?c . ,creator-info)
	     (?v . ,html-validation-link)))))
	"
</div>")))
   ;; org-e-html-html-helper-timestamp
   ))

(defun org-e-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  RAW-DATA is the
original parsed data.  INFO is a plist holding export options."
  (concat
   (format
    (or (and (stringp org-e-html-xml-declaration)
	     org-e-html-xml-declaration)
	(cdr (assoc (plist-get info :html-extension)
		    org-e-html-xml-declaration))
	(cdr (assoc "html" org-e-html-xml-declaration))

	"")
    (or (and coding-system-for-write
	     (fboundp 'coding-system-get)
	     (coding-system-get coding-system-for-write
				'mime-charset))
	"iso-8859-1"))
   "
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
	       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
   (format "
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\"> "
	   (plist-get info :language) (plist-get info :language))
   "
<head>"
   (org-e-html-meta-info info)		; meta
   (org-e-html-style info)		; style
   (org-e-html-mathjax-config info)	; mathjax
   "
</head>"

   "
<body>"
   (let ((link-up (and (plist-get info :link-up)
		       (string-match "\\S-" (plist-get info :link-up))
		       (plist-get info :link-up)))
	 (link-home (and (plist-get info :link-home)
			 (string-match "\\S-" (plist-get info :link-home))
			 (plist-get info :link-home))))
     (when (or link-up link-home)
       (format org-e-html-home/up-format
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; preamble
   (org-e-html-preamble info)
   ;; begin content
   (format "
<div id=\"%s\">" (or org-e-html-content-div
		     (nth 1 org-e-html-divs)))
   ;; document title
   (format "
<h1 class=\"title\">%s</h1>\n" (org-export-secondary-string
				  (plist-get info :title) 'e-html info))
   ;; table of contents
   (let ((depth (plist-get info :with-toc)))
     (when (wholenump depth) (org-e-html-toc depth info)))
   ;; document contents
   contents
   ;; footnotes section
   (org-e-html-footnote-section info)
   ;; bibliography
   (org-e-html-bibliography)
   ;; end content
   (unless body-only
     "
</div>")

   ;; postamble
   (org-e-html-postamble info)

   (unless body-only
     "
</body>")
   "
</html>"))



;;; Transcode Helpers

;;;; Todo

(defun org-e-html--todo (todo)
  (when todo
    (format "<span class=\"%s %s%s\">%s</span>"
	    (if (member todo org-done-keywords) "done" "todo")
	    org-e-html-todo-kwd-class-prefix (org-e-html-fix-class-name todo)
	    todo)))

;;;; Tags

(defun org-e-html--tags (tags)
  (when tags
    (format "<span class=\"tag\">%s</span>"
	    (mapconcat
	     (lambda (tag)
	       (format "<span class=\"%s\">%s</span>"
		       (concat org-e-html-tag-class-prefix
			       (org-e-html-fix-class-name tag))
		       tag))
	     (org-split-string tags ":") "&nbsp;"))))

;;;; Headline

(defun* org-e-html-format-headline
  (todo todo-type priority text tags
	&key level section-number headline-label &allow-other-keys)
  (let ((section-number
	 (when section-number
	   (format "<span class=\"section-number-%d\">%s</span> "
		   level section-number)))
	(todo (org-e-html--todo todo))
	(tags (org-e-html--tags tags)))
    (concat section-number todo (and todo " ") text
	    (and tags "&nbsp;&nbsp;&nbsp;") tags)))

;;;; Src Code

(defun org-e-html-fontify-code (code lang)
  (when code
    (cond
     ;; Case 1: No lang.  Possibly an example block.
     ((not lang)
      ;; Simple transcoding.
      (org-e-html-encode-plain-text code))
     ;; Case 2: No htmlize or an inferior version of htmlize
     ((not (and (require 'htmlize nil t) (fboundp 'htmlize-region-for-paste)))
      ;; Emit a warning.
      (message "Cannot fontify src block (htmlize.el >= 1.34 required)")
      ;; Simple transcoding.
      (org-e-html-encode-plain-text code))
     (t
      ;; Map language
      (setq lang (or (assoc-default lang org-src-lang-modes) lang))
      (let* ((lang-mode (and lang (intern (format "%s-mode" lang)))))
	(cond
	 ;; Case 1: Language is not associated with any Emacs mode
	 ((not (functionp lang-mode))
	  ;; Simple transcoding.
	  (org-e-html-encode-plain-text code))
	 ;; Case 2: Default.  Fotify code.
	 (t
	  ;; htmlize
	  (setq code (with-temp-buffer
		       (insert code)
		       (funcall lang-mode)
		       (font-lock-fontify-buffer)
		       ;; markup each line separately
		       (org-remove-formatting-on-newlines-in-region
			(point-min) (point-max))
		       (org-src-mode)
		       (set-buffer-modified-p nil)
		       (org-export-e-htmlize-region-for-paste
			(point-min) (point-max))))
	  ;; Strip any encolosing <pre></pre> tags
	  (if (string-match "<pre[^>]*>\n*\\([^\000]*\\)</pre>" code)
	      (match-string 1 code)
	    code))))))))

(defun org-e-html-do-format-code
  (code &optional lang refs retain-labels num-start textarea-p)
  (when textarea-p
    (setq num-start nil refs nil lang nil))
  (let* ((code-lines (org-split-string code "\n"))
	 (code-length (length code-lines))
	 (num-fmt
	  (and num-start
	       (format "%%%ds: "
		       (length (number-to-string (+ code-length num-start))))))
	 (code (org-e-html-fontify-code code lang)))
    (assert (= code-length (length (org-split-string code "\n"))))
    (org-export-format-code
     code
     (lambda (loc line-num ref)
       (setq loc
	     (concat
	      ;; Add line number, if needed.
	      (when num-start
		(format "<span class=\"linenr\">%s</span>"
			(format num-fmt line-num)))
	      ;; Transcoded src line.
	      loc
	      ;; Add label, if needed.
	      (when (and ref retain-labels) (format " (%s)" ref))))
       ;; Mark transcoded line as an anchor, if needed.
       (if (not ref) loc
	 (format "<span id=\"coderef-%s\" class=\"coderef-off\">%s</span>"
		 ref loc)))
     num-start refs)))

(defun org-e-html-format-code (element info)
  (let* ((lang (org-element-property :language element))
	 ;; (switches (org-element-property :switches element))
	 (switches nil)			; FIXME
	 (textarea-p (and switches (string-match "-t\\>" switches)))
	 ;; Extract code and references.
	 (code-info (org-export-unravel-code element))
	 (code (car code-info))
	 (refs (cdr code-info))
	 ;; Does the src block contain labels?
	 (retain-labels (org-element-property :retain-labels element))
	 ;; Does it have line numbers?
	 (num-start (case (org-element-property :number-lines element)
		      (continued (org-export-get-loc element info))
		      (new 0))))
    (org-e-html-do-format-code
     code lang refs retain-labels num-start textarea-p)))



;;; Transcode Functions

;;;; Block

(defun org-e-html-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-html--wrap-label
   center-block
   (format "<div style=\"text-align: center\">\n%s</div>" contents)))


;;;; Comment

;; Comments are ignored.


;;;; Comment Block

;; Comment Blocks are ignored.


;;;; Drawer

(defun org-e-html-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
	 (output (if (functionp org-e-html-format-drawer-function)
		     (funcall org-e-html-format-drawer-function
			      name contents)
		   ;; If there's no user defined function: simply
		   ;; display contents of the drawer.
		   contents)))
    (org-e-html--wrap-label drawer output)))


;;;; Dynamic Block

(defun org-e-html-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See
`org-export-data'."
  (org-e-html--wrap-label dynamic-block contents))


;;;; Emphasis

(defun org-e-html-emphasis (emphasis contents info)
  "Transcode EMPHASIS from Org to HTML.
CONTENTS is the contents of the emphasized text.  INFO is a plist
holding contextual information.."
  (let* ((marker (org-element-property :marker emphasis)))
    (format (cdr (assoc marker org-e-html-emphasis-alist)) contents)))


;;;; Entity

(defun org-e-html-entity (entity contents info)
  "Transcode an ENTITY object from Org to HTML.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :html entity))


;;;; Example Block

(defun org-e-html-example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((options (or (org-element-property :options example-block) ""))
	 (lang (org-element-property :language example-block))
	 (caption (org-element-property :caption example-block))
	 (label (org-element-property :name example-block))
	 (caption-str (org-e-html--caption/label-string caption label info))
	 (attr (mapconcat #'identity
			  (org-element-property :attr_html example-block)
			  " "))
	 ;; (switches (org-element-property :switches example-block))
	 (switches nil)			; FIXME
	 (textarea-p (and switches (string-match "-t\\>" switches)))
	 (code (org-e-html-format-code example-block info)))
    (cond
     (textarea-p
      (let ((cols (if (not (string-match "-w[ \t]+\\([0-9]+\\)" switches))
		      80 (string-to-number (match-string 1 switches))))
	    (rows (if (string-match "-h[ \t]+\\([0-9]+\\)" switches)
		      (string-to-number (match-string 1 switches))
		    (org-count-lines code))))
	(format
	 "\n<p>\n<textarea cols=\"%d\" rows=\"%d\">\n%s\n</textarea>\n</p>"
	 cols rows code)))
     (t (format "\n<pre class=\"example\">\n%s\n</pre>" code)))))


;;;; Export Snippet

(defun org-e-html-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'e-html)
    (org-element-property :value export-snippet)))


;;;; Export Block

(defun org-e-html-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "latex")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Fixed Width

(defun org-e-html-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((value (org-element-normalize-string
		 (replace-regexp-in-string
		  "^[ \t]*: ?" ""
		  (org-element-property :value fixed-width)))))
    (org-e-html--wrap-label
     fixed-width (format "\n<pre class=\"example\">\n%s\n</pre>"
			 (org-e-html-do-format-code value)))))


;;;; Footnote Definition

;; Footnote Definitions are ignored.


;;;; Footnote Reference

(defun org-e-html-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       org-e-html-footnote-separator))
   (cond
    ((not (org-export-footnote-first-reference-p footnote-reference info))
     (org-e-html-format-footnote-reference
      (org-export-get-footnote-number footnote-reference info)
      "IGNORED" 100))
    ;; Inline definitions are secondary strings.
    ((eq (org-element-property :type footnote-reference) 'inline)
     (org-e-html-format-footnote-reference
      (org-export-get-footnote-number footnote-reference info)
      "IGNORED" 1))
    ;; Non-inline footnotes definitions are full Org data.
    (t (org-e-html-format-footnote-reference
	(org-export-get-footnote-number footnote-reference info)
	"IGNORED" 1)))))


;;;; Headline

(defun org-e-html-format-headline--wrap (headline info
						  &optional format-function
						  &rest extra-keys)
  "Transcode an HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((level (+ (org-export-get-relative-level headline info)
		   (1- org-e-html-toplevel-hlevel)))
	 (headline-number (org-export-get-headline-number headline info))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 headline-number ".")))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property
				 :todo-keyword headline)))
		      (and todo
			   (org-export-secondary-string todo 'e-html info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-secondary-string
		(org-element-property :title headline) 'e-html info))
	 (tags (and (plist-get info :with-tags)
		    (org-element-property :tags headline)))
	 (headline-label (concat "sec-" (mapconcat 'number-to-string
						   headline-number "-")))
	 (format-function (cond
			   ((functionp format-function) format-function)
			   ((functionp org-e-html-format-headline-function)
			    (function*
			     (lambda (todo todo-type priority text tags
					   &allow-other-keys)
			       (funcall org-e-html-format-headline-function
					todo todo-type priority text tags))))
			   (t 'org-e-html-format-headline))))
    (apply format-function
    	   todo todo-type  priority text tags
    	   :headline-label headline-label :level level
    	   :section-number section-number extra-keys)))

(defun org-e-html-headline (headline contents info)
  "Transcode an HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((numberedp (org-export-numbered-headline-p headline info))
	 (level (org-export-get-relative-level headline info))
	 (text (org-export-secondary-string
		(org-element-property :title headline) 'e-html info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property
				 :todo-keyword headline)))
		      (and todo
			   (org-export-secondary-string todo 'e-html info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-element-property :tags headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 (org-export-get-headline-number
					  headline info) ".")))
	 ;; Create the headline text.
	 (full-text (org-e-html-format-headline--wrap headline info)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((org-export-low-level-p headline info) ; FIXME (or (not section-fmt))
      ;; Build the real contents of the sub-tree.
      (let* ((type (if numberedp 'unordered 'unordered)) ; FIXME
	     (itemized-body (org-e-html-format-list-item
			     contents type nil nil full-text)))
	(concat
	 (and (org-export-first-sibling-p headline info)
	      (org-e-html-begin-plain-list type))
	 itemized-body
	 (and (org-export-last-sibling-p headline info)
	      (org-e-html-end-plain-list type)))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (let* ((extra-class (org-element-property :html-container-class headline))
	     (extra-ids (list (org-element-property :custom-id headline)
			      (org-element-property :id headline)))
	     (extra-ids
	      (mapconcat
	       (lambda (x)
		 (when x
		   (let ((id (org-solidify-link-text
			      (if (org-uuidgen-p x) (concat "ID-" x) x))))
		     (format "<a id=\"%s\" name=\"%s\"/>" id id))))
	       extra-ids ""))
	     (level1 (+ level (1- org-e-html-toplevel-hlevel)))
	     (id (mapconcat 'number-to-string
			    (org-export-get-headline-number headline info) "-")))
	(format "<div id=\"%s\" class=\"%s\">%s%s</div>\n"
		(format "outline-container-%s" id)
		(concat (format "outline-%d" level1) (and extra-class " ")
			extra-class)
		(format "\n<h%d id=\"sec-%s\">%s%s</h%d>\n"
			level1 id extra-ids full-text level1)
		contents))))))


;;;; Horizontal Rule

(defun org-e-html-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE  object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((attr (mapconcat #'identity
			 (org-element-property :attr_html horizontal-rule)
			 " ")))
    (org-e-html--wrap-label horizontal-rule "<hr/>\n")))


;;;; Inline Babel Call

;; Inline Babel Calls are ignored.


;;;; Inline Src Block

(defun org-e-html-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((org-lang (org-element-property :language inline-src-block))
	 (code (org-element-property :value inline-src-block))
	 (separator (org-e-html--find-verb-separator code)))
    (error "FIXME")))


;;;; Inlinetask

(defun org-e-html-format-section (text class &optional id)
  (let ((extra (concat (when id (format " id=\"%s\"" id)))))
    (concat (format "<div class=\"%s\"%s>\n" class extra) text "</div>\n")))

(defun org-e-html-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (cond
   ;; If `org-e-html-format-inlinetask-function' is provided, call it
   ;; with appropriate arguments.
   ((functionp org-e-html-format-inlinetask-function)
    (let ((format-function
	   (function*
	    (lambda (todo todo-type priority text tags
			  &key contents &allow-other-keys)
	      (funcall org-e-html-format-inlinetask-function
		       todo todo-type priority text tags contents)))))
      (org-e-html-format-headline--wrap
       inlinetask info format-function :contents contents)))
   ;; Otherwise, use a default template.
   (t (org-e-html--wrap-label
       inlinetask
       (format
	"\n<div class=\"inlinetask\">\n<b>%s</b><br/>\n%s\n</div>"
	(org-e-html-format-headline--wrap inlinetask info)
	contents)))))


;;;; Item

(defun org-e-html-checkbox (checkbox)
  (case checkbox (on "<code>[X]</code>")
	(off "<code>[&nbsp;]</code>")
	(trans "<code>[-]</code>")
	(t "")))

(defun org-e-html-format-list-item (contents type checkbox
					     &optional term-counter-id
					     headline)
  (concat
   (case type
     (ordered
      (let* ((counter term-counter-id)
	     (extra (if counter (format " value=\"%s\"" counter) "")))
	(format "<li%s>" extra)))
     (unordered
      (let* ((id term-counter-id)
	     (extra (if id (format " id=\"%s\"" id) "")))
	(concat
	 (format "<li%s>" extra)
	 (when headline (concat headline "<br/>")))))
     (descriptive
      (let* ((term term-counter-id))
	(setq term (or term "(no term)"))
	(concat (format "<dt> %s </dt>" term) "<dd>"))))
   (org-e-html-checkbox checkbox) (and checkbox " ")
   contents
   (case type
     (ordered "</li>")
     (unordered "</li>")
     (descriptive "</dd>"))))

(defun org-e-html-item (item contents info)
  "Transcode an ITEM element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  ;; Grab `:level' from plain-list properties, which is always the
  ;; first element above current item.
  (let* ((plain-list (org-export-get-parent item info))
	 (type (org-element-property :type plain-list))
	 (level (org-element-property :level plain-list))
	 (counter (org-element-property :counter item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-secondary-string tag 'e-html info)))))
    (org-e-html-format-list-item
     contents type checkbox (or tag counter))))


;;;; Keyword

(defun org-e-html-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "LATEX") value)
     ((string= key "INDEX") (format "\\index{%s}" value))
     ;; Invisible targets.
     ((string= key "TARGET") nil)	; FIXME
     ((string= key "TOC")
      (let ((value (downcase value)))
	(cond
	 ((string-match "\\<headlines\\>" value)
	  (let ((depth (or (and (string-match "[0-9]+" value)
				(string-to-number (match-string 0 value)))
			   (plist-get info :with-toc))))
	    (when (wholenump depth) (org-e-html-toc depth info))))
	 ((string= "tables" value) "\\listoftables")
	 ((string= "figures" value) "\\listoffigures")
	 ((string= "listings" value)
	  (cond
	   ;; At the moment, src blocks with a caption are wrapped
	   ;; into a figure environment.
	   (t "\\listoffigures")))))))))


;;;; Latex Environment

(defun org-e-html-format-latex (latex-frag processing-type)
  (let* ((cache-relpath
	  (concat "ltxpng/" (file-name-sans-extension
			     (file-name-nondirectory (buffer-file-name)))))
	 (cache-dir (file-name-directory (buffer-file-name )))
	 (display-msg "Creating LaTeX Image..."))

    (with-temp-buffer
      (insert latex-frag)
      (org-format-latex cache-relpath cache-dir nil display-msg
			nil nil processing-type)
      (buffer-string))))

(defun org-e-html-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-html--wrap-label
   latex-environment
   (let ((processing-type (plist-get info :LaTeX-fragments))
	 (latex-frag (org-remove-indentation
		      (org-element-property :value latex-environment)))
	 (caption (org-e-html--caption/label-string
		   (org-element-property :caption latex-environment)
		   (org-element-property :name latex-environment)
		   info))
	 (attr nil)			; FIXME
	 (label (org-element-property :name latex-environment)))
     (cond
      ((member processing-type '(t mathjax))
       (org-e-html-format-latex latex-frag 'mathjax))
      ((equal processing-type 'dvipng)
       (let* ((formula-link (org-e-html-format-latex
			     latex-frag processing-type)))
	 (when (and formula-link
		    (string-match "file:\\([^]]*\\)" formula-link))
	   (org-e-html-format-inline-image
	    (match-string 1 formula-link) caption label attr t))))
      (t latex-frag)))))


;;;; Latex Fragment

(defun org-e-html-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((latex-frag (org-element-property :value latex-fragment))
	(processing-type (plist-get info :LaTeX-fragments)))
    (case processing-type
      ((t mathjax)
       (org-e-html-format-latex latex-frag 'mathjax))
      (dvipng
       (let* ((formula-link (org-e-html-format-latex
			     latex-frag processing-type)))
	 (when (and formula-link
		    (string-match "file:\\([^]]*\\)" formula-link))
	   (org-e-html-format-inline-image
	    (match-string 1 formula-link)))))
      (t latex-frag))))

;;;; Line Break

(defun org-e-html-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "<br/>")


;;;; Link

(defun org-e-html-link--inline-image (link desc info)
  "Return HTML code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 (path (cond ((member type '("http" "https"))
		      (concat type ":" raw-path))
		     ((file-name-absolute-p raw-path)
		      (expand-file-name raw-path))
		     (t raw-path)))
	 (parent (org-export-get-parent-paragraph link info))
	 (caption (org-e-html--caption/label-string
		   (org-element-property :caption parent)
		   (org-element-property :name parent)
		   info))
	 (label (org-element-property :name parent))
	 ;; Retrieve latex attributes from the element around.
	 (attr (let ((raw-attr
		      (mapconcat #'identity
				 (org-element-property :attr_html parent)
				 " ")))
		 (unless (string= raw-attr "") raw-attr))))
    ;; Now clear ATTR from any special keyword and set a default
    ;; value if nothing is left.
    (setq attr (if (not attr) "" (org-trim attr)))
    ;; Return proper string, depending on DISPOSITION.
    (org-e-html-format-inline-image
     path caption label attr (org-e-html-standalone-image-p link info))))

(defvar org-e-html-standalone-image-predicate)
(defun org-e-html-standalone-image-p (element info &optional predicate)
  "Test if ELEMENT is a standalone image for the purpose HTML export.
INFO is a plist holding contextual information.

Return non-nil, if ELEMENT is of type paragraph and it's sole
content, save for whitespaces, is a link that qualifies as an
inline image.

Return non-nil, if ELEMENT is of type link and it's containing
paragraph has no other content save for leading and trailing
whitespaces.

Return nil, otherwise.

Bind `org-e-html-standalone-image-predicate' to constrain
paragraph further.  For example, to check for only captioned
standalone images, do the following.

  \(setq org-e-html-standalone-image-predicate
	\(lambda \(paragraph\)
	  \(org-element-property :caption paragraph\)\)\)
"
  (let ((paragraph (case (org-element-type element)
		     (paragraph element)
		     (link (and (org-export-inline-image-p
				 element org-e-html-inline-image-rules)
				(org-export-get-parent element info)))
		     (t nil))))
    (when paragraph
      (assert (eq (org-element-type paragraph) 'paragraph))
      (when (or (not (and (boundp 'org-e-html-standalone-image-predicate)
			  (functionp org-e-html-standalone-image-predicate)))
		(funcall org-e-html-standalone-image-predicate paragraph))
	(let ((contents (org-element-contents paragraph)))
	  (loop for x in contents
		with inline-image-count = 0
		always (cond
			((eq (org-element-type x) 'plain-text)
			 (not (org-string-nw-p x)))
			((eq (org-element-type x) 'link)
			 (when (org-export-inline-image-p
				x org-e-html-inline-image-rules)
			   (= (incf inline-image-count) 1)))
			(t nil))))))))

(defun org-e-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (path (cond
		((member type '("http" "https" "ftp" "mailto"))
		 (concat type ":" raw-path))
		((string= type "file")
		 (when (string-match "\\(.+\\)::.+" raw-path)
		   (setq raw-path (match-string 1 raw-path)))
		 (if (file-name-absolute-p raw-path)
		     (concat "file://" (expand-file-name raw-path))
		   ;; TODO: Not implemented yet.  Concat also:
		   ;; (org-export-directory :HTML info)
		   (concat "file://" raw-path)))
		(t raw-path)))
	 protocol)
    (cond
     ;; Image file.
     ((and (or (eq t org-e-html-inline-images)
	       (and org-e-html-inline-images (not desc)))
	   (org-export-inline-image-p link org-e-html-inline-image-rules))
      (org-e-html-link--inline-image link desc info))
     ;; Radioed target: Target's name is obtained from original raw
     ;; link.  Path is parsed and transcoded in order to have a proper
     ;; display of the contents.
     ((string= type "radio")
      (format "<a href=\"#%s\">%s</a>"
	      (org-export-solidify-link-text path)
	      (org-export-secondary-string
	       (org-element-parse-secondary-string
		path (org-element-restriction 'radio-target))
	       'e-html info)))
     ;; Links pointing to an headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  ;; Fuzzy link points nowhere.
	  ('nil
	   (format "<i>%s</i>"
		   (or desc (org-export-secondary-string
			     (org-element-property :raw-link link)
			     'e-html info))))
	  ;; Fuzzy link points to an invisible target.
	  (keyword nil)
	  ;; LINK points to an headline.  If headlines are numbered
	  ;; and the link has no description, display headline's
	  ;; number.  Otherwise, display description or headline's
	  ;; title.
	  (headline
	   (let* ((headline-no (org-export-get-headline-number destination info))
		  (label (format "sec-%s" (mapconcat 'number-to-string
						     headline-no "-")))
		  (section-no (mapconcat 'number-to-string headline-no ".")))
	     (setq desc
		   (cond
		    (desc desc)
		    ((plist-get info :section-numbers) section-no)
		    (t (org-export-secondary-string
			(org-element-property :title destination)
			'e-html info))))
	     (format "<a href=\"#%s\">%s</a>" label desc)))
          ;; Fuzzy link points to a target.  Do as above.
	  (otherwise
	   (let ((path (org-export-solidify-link-text path)) number)
	     (unless desc
	       (setq number (cond
			     ((org-e-html-standalone-image-p destination info)
			      (org-export-get-ordinal
			       (assoc 'link (org-element-contents destination))
			       info 'link 'org-e-html-standalone-image-p))
			     (t (org-export-get-ordinal destination info))))
	       (setq desc (when number
			    (if (atom number) (number-to-string number)
			      (mapconcat 'number-to-string number ".")))))
	     (format "<a href=\"#%s\">%s</a>" path (or desc "FIXME")))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" path)))
	(format "<a href=\"#%s\" %s>%s</a>" fragment
		(format (concat "class=\"coderef\""
				" onmouseover=\"CodeHighlightOn(this, '%s');\""
				" onmouseout=\"CodeHighlightOff(this, '%s');\"")
			fragment fragment)
		(format (org-export-get-coderef-format path (or desc "%s"))
			(org-export-resolve-coderef path info)))))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'html))
     ;; External link with a description part.
     ((and path desc) (format "<a href=\"%s\">%s</a>" path desc))
     ;; External link without a description part.
     (path (format "<a href=\"%s\">%s</a>" path path))
     ;; No path, only description.  Try to do something useful.
     (t (format "<i>%s</i>" desc)))))


;;;; Babel Call

;; Babel Calls are ignored.


;;;; Macro

(defun org-e-html-macro (macro contents info)
  "Transcode a MACRO element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))


;;;; Paragraph

(defun org-e-html-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((style nil)			; FIXME
	 (class (cdr (assoc style '((footnote . "footnote")
				    (verse . nil)))))
	 (extra (if class (format " class=\"%s\"" class) ""))
	 (parent (org-export-get-parent paragraph info)))
    (cond
     ((and (equal (car parent) 'item)
	   (= (org-element-property :begin paragraph)
	      (org-element-property :contents-begin parent)))
      ;; leading paragraph in a list item have no tags
      contents)
     ((org-e-html-standalone-image-p paragraph info)
      ;; standalone image
      contents)
     (t (format "\n<p%s>\n%s\n</p>" extra contents)))))


;;;; Plain List

(defun org-e-html-begin-plain-list (type &optional arg1)
  (case type
    (ordered
     (format "<ol%s>" (if arg1		; FIXME
			  (format " start=\"%d\"" arg1)
			"")))
    (unordered "<ul>")
    (descriptive "<dl>")))

(defun org-e-html-end-plain-list (type)
  (case type
    (ordered "</ol>")
    (unordered "</ul>")
    (descriptive "</dl>")))

(defun org-e-html-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* (arg1 ;; FIXME
	 (type (org-element-property :type plain-list))
	 (attr (mapconcat #'identity
			  (org-element-property :attr_html plain-list)
			  " ")))
    (org-e-html--wrap-label
     plain-list (format "%s\n%s%s"
			(org-e-html-begin-plain-list type)
			contents (org-e-html-end-plain-list type)))))

;;;; Plain Text

(defun org-e-html-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (let ((all org-e-html-special-string-regexps)
	e a re rpl start)
    (while (setq a (pop all))
      (setq re (car a) rpl (cdr a) start 0)
      (while (string-match re string start)
	(setq string (replace-match rpl t nil string))))
    string))

(defun org-e-html-encode-plain-text (s)
  "Convert plain text characters to HTML equivalent.
Possible conversions are set in `org-export-html-protect-char-alist'."
  (let ((cl org-e-html-protect-char-alist) c)
    (while (setq c (pop cl))
      (let ((start 0))
	(while (string-match (car c) s start)
	  (setq s (replace-match (cdr c) t t s)
		start (1+ (match-beginning 0))))))
    s))

(defun org-e-html-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (setq text (org-e-html-encode-plain-text text))
  ;; Protect %, #, &, $, ~, ^, _,  { and }.
  ;; (while (string-match "\\([^\\]\\|^\\)\\([%$#&{}~^_]\\)" text)
  ;;   (setq text
  ;; 	  (replace-match (format "\\%s" (match-string 2 text)) nil t text 2)))
  ;; Protect \
  ;; (setq text (replace-regexp-in-string
  ;; 	      "\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%$#&{}~^_\\]\\|$\\)"
  ;; 	      "$\\backslash$" text nil t 1))
  ;; HTML into \HTML{} and TeX into \TeX{}.
  ;; (let ((case-fold-search nil)
  ;; 	(start 0))
  ;;   (while (string-match "\\<\\(\\(?:La\\)?TeX\\)\\>" text start)
  ;;     (setq text (replace-match
  ;; 		  (format "\\%s{}" (match-string 1 text)) nil t text)
  ;; 	    start (match-end 0))))
  ;; Handle quotation marks
  ;; (setq text (org-e-html--quotation-marks text info))
  ;; Convert special strings.
  ;; (when (plist-get info :with-special-strings)
  ;;   (while (string-match (regexp-quote "...") text)
  ;;     (setq text (replace-match "\\ldots{}" nil t text))))
  (when (plist-get info :with-special-strings)
    (setq text (org-e-html-convert-special-strings text)))
  ;; Handle break preservation if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n"
					 text)))
  ;; Return value.
  text)


;;;; Property Drawer

(defun org-e-html-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")


;;;; Quote Block

(defun org-e-html-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-html--wrap-label
   quote-block (format "<blockquote>\n%s</blockquote>" contents)))


;;;; Quote Section

(defun org-e-html-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-property :value quote-section))))
    (when value (format "<pre>\n%s</pre>" value))))


;;;; Section

(defun org-e-html-section (section contents info) ; FIXME
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent-headline section info)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (let ((class-num (+ (org-export-get-relative-level parent info)
			  (1- org-e-html-toplevel-hlevel)))
            (id-num
             (mapconcat
              'number-to-string
	      (org-export-get-headline-number parent info) "-")))
        ;; Build return value.
        (format "<div class=\"outline-text-%d\" id=\"text-%s\">\n%s</div>"
                class-num id-num contents)))))

;;;; Radio Target

(defun org-e-html-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to HTML.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (let ((id (org-export-solidify-link-text
	     (org-element-property :value radio-target))))
    (format "<a id=\"%s\" name=\"%s\">%s</a>" id id text)))


;;;; Special Block

(defun org-e-html-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block))))
    (org-e-html--wrap-label
     special-block
     (format "\\begin{%s}\n%s\\end{%s}" type contents type))))


;;;; Src Block

(defun org-e-html-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language src-block))
	 (caption (org-element-property :caption src-block))
	 (label (org-element-property :name src-block))
	 (caption-str (org-e-html--caption/label-string caption label info))
	 (attr (mapconcat #'identity
			  (org-element-property :attr_html src-block)
			  " "))
	 ;; (switches (org-element-property :switches src-block))
	 (switches nil)			; FIXME
	 (textarea-p (and switches (string-match "-t\\>" switches)))
	 (code (org-e-html-format-code src-block info)))
    (cond
     (lang (format
	    "\n<div class=\"org-src-container\">\n%s%s\n</div>"
	    (if (not caption) ""
	      (format "<label class=\"org-src-name\">%s</label>" caption-str))
	    (format "\n<pre class=\"src src-%s\">%s\n</pre>" lang code)))
     (textarea-p
      (let ((cols (if (not (string-match "-w[ \t]+\\([0-9]+\\)" switches))
		      80 (string-to-number (match-string 1 switches))))
	    (rows (if (string-match "-h[ \t]+\\([0-9]+\\)" switches)
		      (string-to-number (match-string 1 switches))
		    (org-count-lines code))))
	(format
	 "\n<p>\n<textarea cols=\"%d\" rows=\"%d\">\n%s\n</textarea>\n</p>"
	 cols rows code)))
     (t (format "\n<pre class=\"example\">\n%s\n</pre>" code)))))

;;;; Statistics Cookie

(defun org-e-html-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((cookie-value (org-element-property :value statistics-cookie)))
    (format "<code>%s</code>" cookie-value)))


;;;; Subscript

(defun org-e-html-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sub>%s</sub>" contents))


;;;; Superscript

(defun org-e-html-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to HTML.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<sup>%s</sup>" contents))


;;;; Tabel Cell

(defun org-e-html-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-row (org-export-get-parent table-cell info))
	 (table (org-export-get-parent-table table-cell info))
	 (cell-attrs
	  (if (not org-e-html-table-align-individual-fields) ""
	    (format (if (and (boundp 'org-e-html-format-table-no-css)
			     org-e-html-format-table-no-css)
			" align=\"%s\"" " class=\"%s\"")
		    (org-export-table-cell-alignment table-cell info)))))
    (when (or (not contents) (string= "" (org-trim contents)))
      (setq contents "&nbsp;"))
    (cond
     ((and (org-export-table-has-header-p table info)
	   (= 1 (org-export-table-row-group table-row info)))
      (concat "\n" (format (car org-e-html-table-header-tags) "col" cell-attrs)
	      contents (cdr org-e-html-table-header-tags)))
     ((and org-e-html-table-use-header-tags-for-first-column
  	   (zerop (cdr (org-export-table-cell-address table-cell info))))
      (concat "\n" (format (car org-e-html-table-header-tags) "row" cell-attrs)
	      contents (cdr org-e-html-table-header-tags)))
     (t (concat "\n" (format (car org-e-html-table-data-tags) cell-attrs)
		contents (cdr org-e-html-table-data-tags))))))


;;;; Table Row

(defun org-e-html-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to HTML.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((first-rowgroup-p (= 1 (org-export-table-row-group table-row info)))
  	   (rowgroup-tags
	    (cond
	     ;; Case 1: Row belongs to second or subsequent rowgroups.
	     ((not (= 1 (org-export-table-row-group table-row info)))
	      '("\n<tbody>" . "\n</tbody>"))
	     ;; Case 2: Row is from first rowgroup.  Table has >=1 rowgroups.
	     ((org-export-table-has-header-p
	       (org-export-get-parent-table table-row info) info)
	      '("\n<thead>" . "\n</thead>"))
	     ;; Case 2: Row is from first and only row group.
	     (t '("\n<tbody>" . "\n</tbody>")))))
      (concat
       ;; Begin a rowgroup?
       (when (org-export-table-row-starts-rowgroup-p table-row info)
  	 (car rowgroup-tags))
       ;; Actual table row
       (concat "\n" (eval (car org-e-html-table-row-tags))
	       contents (eval (cdr org-e-html-table-row-tags)))
       ;; End a rowgroup?
       (when (org-export-table-row-ends-rowgroup-p table-row info)
  	 (cdr rowgroup-tags))))))


;;;; Table

(defun org-e-html-table-first-row-data-cells (table info)
  (let ((table-row
	 (org-element-map
	  table 'table-row
	  (lambda (row)
	    (unless (eq (org-element-property :type row) 'rule) row))
	  info 'first-match))
	(special-column-p (org-export-table-has-special-column-p table)))
    (if (not special-column-p) (org-element-contents table-row)
      (cdr (org-element-contents table-row)))))

(defun org-e-html-table--table.el-table (table info)
  (when (eq (org-element-property :type table) 'table.el)
    (require 'table)
    (let ((outbuf (with-current-buffer
		      (get-buffer-create "*org-export-table*")
		    (erase-buffer) (current-buffer))))
      (with-temp-buffer
	(insert (org-element-property :value table))
	(goto-char 1)
	(re-search-forward "^[ \t]*|[^|]" nil t)
	(table-generate-source 'html outbuf))
      (with-current-buffer outbuf
	(prog1 (org-trim (buffer-string))
	  (kill-buffer) )))))

(defun org-e-html-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (case (org-element-property :type table)
    ;; Case 1: table.el table.  Convert it using appropriate tools.
    (table.el (org-e-html-table--table.el-table table info))
    ;; Case 2: Standard table.
    (t
     (let* ((label (org-element-property :name table))
  	    (caption (org-e-html--caption/label-string
  		      (org-element-property :caption table) label info))
  	    (attributes (mapconcat #'identity
  				   (org-element-property :attr_html table)
  				   " "))
  	    (alignspec
  	     (if (and (boundp 'org-e-html-format-table-no-css)
  		      org-e-html-format-table-no-css)
  		 "align=\"%s\"" "class=\"%s\""))
  	    (table-column-specs
  	     (function
  	      (lambda (table info)
  		(mapconcat
  		 (lambda (table-cell)
  		   (let ((alignment (org-export-table-cell-alignment
  				     table-cell info)))
  		     (concat
  		      ;; Begin a colgroup?
  		      (when (org-export-table-cell-starts-colgroup-p
  			     table-cell info)
  			"\n<colgroup>")
  		      ;; Add a column.  Also specify it's alignment.
  		      (format "\n<col %s/>" (format alignspec alignment))
  		      ;; End a colgroup?
  		      (when (org-export-table-cell-ends-colgroup-p
  			     table-cell info)
  			"\n</colgroup>"))))
  		 (org-e-html-table-first-row-data-cells table info) "\n"))))
  	    (table-attributes
  	     (let ((table-tag (plist-get info :html-table-tag)))
  	       (concat
  		(and (string-match  "<table\\(.*\\)>" table-tag)
  		     (match-string 1 table-tag))
  		(and label (format " id=\"%s\""
  				   (org-solidify-link-text label)))))))
       ;; Remove last blank line.
       (setq contents (substring contents 0 -1))
       ;; FIXME: splice
       (format "\n<table%s>\n<caption>%s</caption>\n%s\n%s\n</table>"
  	       table-attributes
  	       (or caption "")
  	       (funcall table-column-specs table info)
  	       contents)))))

;;;; Target

(defun org-e-html-target (target contents info)
  "Transcode a TARGET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((id (org-export-solidify-link-text
	     (org-element-property :value target))))
    (format "<a id=\"%s\" name=\"%s\"/>" id id)))


;;;; Time-stamp

(defun org-e-html-time-stamp (time-stamp contents info)
  "Transcode a TIME-STAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-element-property :value time-stamp))
        (type (org-element-property :type time-stamp))
        (appt-type (org-element-property :appt-type time-stamp)))
    (setq value (org-translate-time
		 (org-export-secondary-string value 'e-html info)))
    (setq appt-type (case appt-type
		      (scheduled org-scheduled-string)
		      (deadline org-deadline-string)
		      (closed org-closed-string)))
    (format "<span class=\"timestamp-wrapper\">%s%s</span>"
	    (if (not appt-type) ""
		(format "<span class=\"timestamp-kwd\">%s</span> " appt-type))
	    (format "<span class=\"timestamp\">%s</span>" value))))


;;;; Verbatim

(defun org-e-html-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-html-emphasis
   verbatim (org-element-property :value verbatim) info))


;;;; Verse Block

(defun org-e-html-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Replace each newline character with line break.  Also replace
  ;; each blank line with a line break.
  (setq contents (replace-regexp-in-string
		  "^ *\\\\\\\\$" "<br/>\n"
		  (replace-regexp-in-string
		   "\\(\\\\\\\\\\)?[ \t]*\n" " <br/>\n"
		   (org-remove-indentation
		    (org-export-secondary-string
		     (org-element-property :value verse-block)
		     'e-html info)))))
  ;; Replace each white space at beginning of a line with a
  ;; non-breaking space.
  (while (string-match "^[ \t]+" contents)
    (let* ((num-ws (length (match-string 0 contents)))
	   (ws (let (out) (dotimes (i num-ws out)
			    (setq out (concat out "&nbsp;"))))))
      (setq contents (replace-match ws nil t contents))))
  (org-e-html--wrap-label
   verse-block (format "<p class=\"verse\">\n%s</p>" contents)))




;;; Filter Functions

;;;; Filter Settings

(defconst org-e-html-filters-alist
  '((:filter-final-output . org-e-html-final-function))
  "Alist between filters keywords and back-end specific filters.
See `org-export-filters-alist' for more information.")


;;;; Filters

(defun org-e-html-final-function (contents backend info)
  (if (not org-e-html-pretty-output) contents
    (with-temp-buffer
      (nxml-mode)
      (insert contents)
      (indent-region (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))


;;; Interactive functions

(defun org-e-html-export-to-html
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to a HTML file.

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
  (setq debug-on-error t)		; FIXME
  (let* ((extension (concat "." org-e-html-extension))
	 (file (org-export-output-file-name extension subtreep pub-dir)))
    (org-export-to-file
     'e-html file subtreep visible-only body-only ext-plist)))



;;; FIXMES, TODOS, FOR REVIEW etc

;;;; org-format-table-html
;;;; org-format-org-table-html
;;;; org-format-table-table-html
;;;; org-table-number-fraction
;;;; org-table-number-regexp
;;;; org-e-html-table-caption-above

;;;; org-whitespace
;;;; "<span style=\"visibility:hidden;\">%s</span>"
;;;; Remove display properties

;;;; org-e-html-with-timestamp
;;;; org-e-html-html-helper-timestamp

;;;; org-export-as-html-and-open
;;;; org-export-as-html-batch
;;;; org-export-as-html-to-buffer
;;;; org-replace-region-by-html
;;;; org-export-region-as-html
;;;; org-export-as-html

;;;; (org-export-directory :html opt-plist)
;;;; (plist-get opt-plist :html-extension)
;;;; org-e-html-toplevel-hlevel
;;;; org-e-html-special-string-regexps
;;;; org-e-html-coding-system
;;;; org-e-html-coding-system
;;;; org-e-html-inline-images
;;;; org-e-html-inline-image-extensions
;;;; org-e-html-protect-char-alist
;;;; org-e-html-table-use-header-tags-for-first-column
;;;; org-e-html-todo-kwd-class-prefix
;;;; org-e-html-tag-class-prefix
;;;; org-e-html-footnote-separator

;;;; org-export-preferred-target-alist
;;;; org-solidify-link-text
;;;; class for anchors
;;;; org-export-with-section-numbers, body-only
;;;; org-export-mark-todo-in-toc

(provide 'org-e-html)
;;; org-e-html.el ends here

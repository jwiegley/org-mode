;;; ox-lzw.el --- LZW compression algorithm in Emacs Lisp

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

;;; Commentary:

;; This code has not been optimized, and I welcome any improvements.
;; As it is, the resulting conversion of a large Lisp form to a
;; base64-encoded string, using `org-x-compress-data', achieves a
;; compression ratio near 50% over just turning the data into a string
;; with `prin1-to-string'.

;;; Code:

(eval-when-compile
  (require 'cl))

(defun lzw-compress-string (uncompressed)
  "Compress a string to a list of output symbols."
  ;; Build the dictionary.
  (let* ((dict-size 256)
	 (dictionary
	  (let ((dict (make-hash-table :size dict-size :test 'equal)))
	    (dotimes (i dict-size)
	      (puthash (char-to-string i) (char-to-string i) dict))
	    dict)))
    (with-temp-buffer
      (let ((w ""))
	(dolist (c (string-to-list uncompressed))
	  (let ((wc (concat w (char-to-string c))))
	    (if (gethash wc dictionary)
		(setq w wc)
	      (insert (gethash w dictionary))
	      ;; Add wc to the dictionary.
	      (puthash wc (char-to-string dict-size) dictionary)
	      (setq dict-size (1+ dict-size)
		    w (char-to-string c)))))
	;; Output the code for w.
	(if w
	    (insert (gethash w dictionary))))
      (buffer-string))))
 
(defun lzw-decompress-string (compressed)
  "Decompress a list of output ks to a string."
  ;; Build the dictionary.
  (let* ((dict-size 256)
	 (dictionary
	  (let ((dict (make-hash-table :size dict-size :test 'equal)))
	    (dotimes (i dict-size)
	      (puthash (char-to-string i) (char-to-string i) dict))
	    dict)))
    (with-temp-buffer
      (let* ((compr-list (string-to-list compressed))
	     (w (char-to-string (pop compr-list))))
	(insert w)
	(dolist (k compr-list)
	  (let ((entry
		 (or (gethash (char-to-string k) dictionary)
		     (if (= k dict-size)
			 (concat w (char-to-string (aref w 0)))
		       (error "Bad compressed k: %s" k)))))
	    (insert entry)
	    
	    ;; Add w+entry[0] to the dictionary.
	    (puthash (char-to-string dict-size)
		     (concat w (char-to-string (aref entry 0)))
		     dictionary)
	    (setq dict-size (1+ dict-size)
		  w entry))))
      (buffer-string))))

(defun org-x-compress-data (data)
  (base64-encode-string
   (string-as-unibyte
    (lzw-compress-string
     (prin1-to-string data)))))

(defun org-x-decompress-data (str)
  (read
   (lzw-decompress-string
    (string-as-multibyte
     (base64-decode-string str)))))

(provide 'ox-lzw)

;;; ox-lzw.el ends here

;;; ox-plugin.el --- working with Org data in a modular awy

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

(eval-when-compile
  (require 'cl))

(defgroup org-x-plugin nil
  "Code for plugging Org-X into the overall Org-mode framework."
  :tag "Org-X plugin"
  :group 'org-x)

;;(defadvice org-store-log-note (before org-x-post-note activate)
;;  (unless org-note-abort
;;    (let ((txt (buffer-string)) issue-id)
;;      ;; need to parse the timestamp here
;;      (while (string-match "\\`#.*\n[ \t\n]*" txt)
;;        (setq txt (replace-match "" t t txt)))
;;      (if (string-match "\\s-+\\'" txt)
;;          (setq txt (replace-match "" t t txt)))
;;      (with-current-buffer (marker-buffer org-log-note-marker)
;;        (save-excursion
;;          (goto-char org-log-note-marker)
;;          (let ((heading (nth 4 (org-heading-components))))
;;            (when (string-match "\\[\\[redmine:\\([0-9]+\\)\\]" heading)
;;              (cond
;;               ((eq org-log-note-purpose 'note)
;;                (org-x-redmine-add-note
;;                 (string-to-number (match-string 1 heading)) txt))
;;               ((eq org-log-note-purpose 'state)
;;                (let ((state-id (cddr (assoc org-log-note-state
;;                                             org-x-redmine-statuses))))
;;                  (if state-id
;;                   (org-x-redmine-modify-status
;;                    (string-to-number (match-string 1 heading))
;;                    state-id txt))))))))))))

(provide 'ox-plugin)

;; arch-tag: 

;;; ox-plugin.el ends here

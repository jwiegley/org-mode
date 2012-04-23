;;; test-org-exp.el --- tests for org-exp.el

;; Copyright (c) 2010-2012 Eric Schulte
;; Authors: Eric Schulte

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;; Code:
(ert-deftest test-org-exp/stripping-commas ()
  "Test the stripping of commas from within blocks during export."
  (org-test-at-id "76d3a083-67fa-4506-a41d-837cc48158b5"
    ;; don't strip internal commas
    (org-narrow-to-subtree)
    (should (string-match
             ", 2"
             (org-export-as-ascii nil nil nil 'string)))))

(provide 'test-org-exp)

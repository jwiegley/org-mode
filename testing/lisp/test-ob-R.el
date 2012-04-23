;;; test-ob-R.el --- tests for ob-R.el

;; Copyright (c) 2011-2012 Eric Schulte
;; Authors: Eric Schulte

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;; Code:
(org-test-for-executable "R")
(unless (featurep 'ess)
  (signal 'missing-test-dependency "ESS"))
(unless (featurep 'ob-R)
  (signal 'missing-test-dependency "Support for R code blocks"))

(ert-deftest test-ob-R/simple-session ()
  (org-test-with-temp-text
      "#+begin_src R :session R\n  paste(\"Yep!\")\n#+end_src\n"
    (should (string= "Yep!" (org-babel-execute-src-block)))))

(ert-deftest test-ob-R/colnames-yes-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames yes
#+header: :var x = eg
#+begin_src R
x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") hline ("a") ("b"))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-R/colnames-nil-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames nil
#+header: :var x = eg
#+begin_src R
x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") hline ("a") ("b"))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-R/colnames-no-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames no
#+header: :var x = eg
#+begin_src R
x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") ("a") ("b"))
		   (org-babel-execute-src-block)))))

(provide 'test-ob-R)

;;; test-ob-R.el ends here

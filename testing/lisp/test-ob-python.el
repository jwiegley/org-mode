;;; test-ob-python.el --- tests for ob-python.el

;; Copyright (c) 2011-2012 Eric Schulte
;; Authors: Eric Schulte

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;; Code:
(org-test-for-executable "python")
(unless (featurep 'ob-python)
  (signal 'missing-test-dependency "Support for Python code blocks"))

(ert-deftest test-ob-python/colnames-yes-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames yes
#+header: :var x = eg
#+begin_src python
return x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") hline ("a") ("b"))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/colnames-yes-header-argument-again ()
  (org-test-with-temp-text "#+tblname: less-cols
| a |
|---|
| b |
| c |

#+header: :colnames yes
#+begin_src python :var tab=less-cols
  return [[val + '*' for val in row] for row in tab]
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("a") hline ("b*") ("c*"))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/colnames-nil-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames nil
#+header: :var x = eg
#+begin_src python
return x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") hline ("a") ("b"))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/colnames-no-header-argument-again ()
  (org-test-with-temp-text "#+tblname: less-cols
| a |
|---|
| b |
| c |

#+header: :colnames no
#+begin_src python :var tab=less-cols
  return [[val + '*' for val in row] for row in tab]
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("a*") ("b*") ("c*"))
		   (org-babel-execute-src-block)))))

(ert-deftest test-ob-python/colnames-no-header-argument ()
  (org-test-with-temp-text "#+name: eg
| col |
|-----|
| a   |
| b   |

#+header: :colnames no
#+header: :var x = eg
#+begin_src python
return x
#+end_src"
    (org-babel-next-src-block)
    (should (equal '(("col") ("a") ("b"))
		   (org-babel-execute-src-block)))))

(provide 'test-ob-python)

;;; test-ob-python.el ends here
 

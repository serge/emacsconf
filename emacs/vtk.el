(defun replace-once (regexp to-string)
  (if (re-search-forward regexp (point-at-eol) t)
      (progn 
	(replace-match to-string nil nil)
	t) nil))
    
(defun cpp-vtk-tcl-to-cpp ()
  (interactive)
  (replace-once "\\(\\w+\\) +\\(\\w+\\)" "\\1* \\2 = \\1::New();")
  )

(defun cpp-vtk-tcl-to-method ()
  (interactive)
  (replace-once "\\(\\w+\\) +\\(\\w+\\)" "\\1->\\2(")
  (defun subst-arg (need-comma)
    (defun make-pattern (comma) (if need-comma ", \\1" "\\1"))
    (let ((ptr (make-pattern need-comma)))
      (if (replace-once " +\\([a-zA-Z0-9._]+\\)" ptr)
	(subst-arg t))))
  (subst-arg ())
  (insert ");"))


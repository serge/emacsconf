(defun run-through-files (dir pred &optional rec)
  (defun get-dirs (dir) (directory-files dir t ".*[^/\\\\][^.~]\\{1,2\\}$"))
  (defun run-through-files-h (dir-list pred)
    (if dir-list
        (let ((item (car dir-list))
              (rest (cdr dir-list)))
          (if (file-directory-p item) 
              (if rec (run-through-files-h (get-dirs item) pred))
            (funcall pred item))
          (run-through-files-h rest pred))
      nil))
  (run-through-files-h (get-dirs dir) pred))

(run-through-files "C:\\dev\\expr_copy\\3rdparty\\include\\itk"
(lambda (file) 
  (find-file file)
  (replace-regexp "#include[ ]+\\\"\\(vxl[/\\\\][^\\\"]+\\)"
                  "#include \"itk/\\1")
  (save-buffer)
  (kill-buffer nil)))

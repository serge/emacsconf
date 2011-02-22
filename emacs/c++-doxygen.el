(defvar doxy-big-comment-line "/////////////////////////////////////////////////////////////////////////////////////////")
(defvar doxy-small-comment-line "///")
(defvar doxy-tag-date "\\date")
(defvar doxy-tag-file "\\file")
(defvar doxy-tag-brief "\\brief")

(defun doxy-get-time () (format-time-string "%d/%m/%Y"))
(defun doxy-add-file-header ()
  (interactive)
  (save-excursion 
    (progn 
      (defvar desc-brief "TODO Brief description goes here")
      (defvar desc-detal "TODO Detailled description goes here")
      (beginning-of-buffer)
      (insert doxy-big-comment-line)(newline)
      (insert (format "%s %s %s" doxy-small-comment-line doxy-tag-file (file-name-nondirectory (buffer-file-name))))(newline)
      (insert (format "%s %s %s" doxy-small-comment-line doxy-tag-date (doxy-get-time)))(newline)
      (insert (format "%s %s %s" doxy-small-comment-line doxy-tag-brief desc-brief))(newline)
      (insert doxy-small-comment-line)(newline)
      (insert (format "%s %s" doxy-small-comment-line desc-detal))(newline)
      (insert doxy-big-comment-line)(newline)
)))

(defun doxy-add-class-header ()
  (interactive)
  (save-excursion
    (progn
      (       
       ))))
(doxy-add-file-header)


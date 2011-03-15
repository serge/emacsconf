(defun git-get-version (treespec file)
  (let (
	(git-root 
	 (with-temp-buffer
	   (shell-command "git rev-parse --show-toplevel" t)
	   (goto-char (point-max))
	   (delete-char -1)
	   (buffer-string)))
	)
  (let (
	(new-mode major-mode)
	(cmd (format "git show %s:%s" treespec (file-relative-name (file-truename file) git-root)))
	(buffername (format "%s:%s" treespec (file-name-nondirectory file)))
	)
    (message cmd)
    (message git-root)
    (shell-command cmd buffername)
    (with-current-buffer buffername
      (save-excursion
	(let ((sp (point)))
	  (insert (format "(%s)" new-mode))
	  (eval-region sp (point))
	  (kill-region sp (point))
	  )))
    buffername
    )))
(require 'cl)

(defun git-read-revision (prompt)
  (let (
	(branches (split-string (with-temp-buffer
				  (shell-command "git branch -a" t)
				  (goto-char (point-max))
				  (delete-char -1)
				  (buffer-string)))))
    (list (completing-read prompt branches))))
(defun git-show (treespec)
  (interactive (git-read-revision "Branch: "))
  (switch-to-buffer (git-get-version treespec (buffer-file-name))))


(defun git-diff() 
  (interactive (git-read-revision "Branch: "))
  (ediff-buffers (current-buffer) (git-get-version treespec (buffer-file-name))))

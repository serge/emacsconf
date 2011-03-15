(setq git-bin "/usr/local/bin/git")
(defun git-get-root ()
  (with-temp-buffer
    (shell-command (concat git-bin " rev-parse --show-toplevel") t)
    (goto-char (point-max))
    (delete-char -1)
    (buffer-string)))

(defun git-file-path (file)
  (file-relative-name (file-truename file) (git-get-root)))

(defun git-get-version (treespec file)
  "Obtain revision of TREESPEC for the current file "
    (let (
	  (new-mode major-mode)
	  (cmd (format "%s show %s:%s" git-bin treespec (git-file-path file)))
	  (buffername (format "%s:%s" treespec (file-name-nondirectory file)))
	  )
      (shell-command cmd buffername)
      (with-current-buffer buffername
	(save-excursion
	  (let ((sp (point)))
	    (insert (format "(%s)" new-mode))
	    (eval-region sp (point))
	    (kill-region sp (point))
	    )))
      buffername
      ))

(defun git-read-revision (prompt)
  "Obtain an autocompletion list of branches for current repo"
  (let (
	(branches (split-string (with-temp-buffer
				  (shell-command (concat git-bin " branch -a") t)
				  (goto-char (point-max))
				  (delete-char -1)
				  (buffer-string)))))
    (list (completing-read prompt branches))))

(defun git-show (treespec)
  "Show file version of revision TREESPEC"
  (interactive (git-read-revision "Branch: "))
  (switch-to-buffer (git-get-version treespec (buffer-file-name))))

(defun git-diff (treespec)
  "Compare current file with its version of revision TREESPEC"
  (interactive (git-read-revision "Branch: "))
  (ediff-buffers (current-buffer) (git-get-version treespec (buffer-file-name))))

(defun git-blame ()
   "Show git-blame command output on the current file"
   (interactive)
   (shell-command (format "%s blame %s" git-bin (file-relative-name (file-truename (buffer-file-name))))))

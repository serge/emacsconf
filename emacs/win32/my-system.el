(let ((WM_MAXIMIZE #xf030))
  (global-set-key
   [(C-f2)]
   '(lambda ()
      (interactive)
      (let ((file-name (buffer-file-name)))
	(if file-name (w32-set-clipboard-data file-name)
	  (message "buffer %s isn't visiting any file" (buffer-name))))))
  (global-set-key
   [(M-f2)]
   '(lambda ()
      (interactive)
      (defun remove-commas (str)
	(replace-regexp-in-string "\"" "" str))
      (let ((file-name (remove-commas (w32-get-clipboard-data))))
	(if file-name
	    (if (and
		 file-name
		 (file-exists-p file-name))
		(find-file file-name)
	      (message "File %s doesn't exist" file-name))
	  (message "File name is empty")))))
  (setq w32-enable-caps-lock nil)
  (define-key function-key-map [(capslock)] 'event-apply-control-modifier)
  (w32-send-sys-command WM_MAXIMIZE))

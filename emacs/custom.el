(defun cc-annotate ()
  (interactive)
  (shell-command (format "cleartool annotate -out - %s" (buffer-file-name))))

(defun cc-checkout ()
  (interactive)
  (shell-command (format "cleartool co -unreserved %s" (buffer-file-name))))

(defun cc-uncheckout ()
  (interactive)
  (shell-command (format "cleartool unco -rm %s" (buffer-file-name))))

(defun cc-list-activity ()
  (interactive)
  (shell-command (format "cleartool lsact -s -me")))

(defun cc-list-checked-out ()
  (interactive)
  (setq delim "-----------------------------------------------------------------")
  (shell-command (format "cleartool lsc -fmt \"%%n\\n%%[activity]p\\n%%u\\n%s\\n\"" delim)))

(defun wx-help ()
  (interactive)
  (browse-url (format "http://docs.wxwidgets.org/stable/wx_%s.html" (downcase (current-word)))))
(defun vtk-help ()
  (interactive)
  (browse-url (format "http://www.vtk.org/doc/release/4.0/html/class%s.html" (current-word))))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(max-lisp-eval-depth 1000)
 '(truncate-partial-width-windows nil))

;; redefine
(defvar cpp-compiler "g++-4.0" "Store the name of the compiler to use with .cpp files")

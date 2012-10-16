
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(if (functionp 'set-message-beep) (set-message-beep 'silent))
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(line-number-mode 1)
(which-func-mode 1)
(ido-mode 1)

(let (
      (conf-path (file-name-directory
			(if load-file-name
			    load-file-name
			  "~/.emacs")))
      )
  (let ((my-path (concat conf-path "emacs")))
    ;; enable session saving
    (add-to-list 'load-path my-path)
    (load "codepad")
    (load "c++-settings")
    (load "my-git")
    (load "misc")
    (defun load-sytem-misc-path (system-file)
      (let (
      (my-system-path
       (concat my-path (cond
       ((string= system-type "gnu/linux") "/linux/")
       ((string= system-type "windows-nt") "/win32/")
       ((string= system-type "darwin") "/macosx/")
       (t "none")) )))
	(let ((my-system-file (concat my-system-path system-file ".el")))
	  (if (file-exists-p my-system-file)
	      (progn
		(add-to-list 'load-path my-system-path)
		(load system-file))))))
    (load-sytem-misc-path "my-system")
  )
 )

;; Trailing whitespace is unnecessary
;(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Don't spawn backup files everywhere
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; enable session saving
(desktop-save-mode 1)
(setq desktop-dirname "~/")

(put 'narrow-to-region 'disabled nil)

(put 'narrow-to-page 'disabled nil)

;; make ediff use vertical split by default
(setq ediff-split-window-function (quote split-window-horizontally))

;; regex example
;; (while (re-search-forward "get\\(H\\|A\\|V\\|S\\)\\([^m][a-z]\\{1,5\\}\\)()" nil t)
;;  (replace-match (concat "Get(rendu3DProperty::"
;;		  (upcase (match-string 2)) ")." (downcase (match-string 1))) nil nil))

;; shell
(global-set-key [(C-f10)]   'shell)
(global-set-key [(C-f11)]   '(lambda () (interactive)(switch-to-buffer "*Messages*")))
(global-set-key [(C-f4)]   '(lambda () (interactive)(kill-buffer nil)))
(global-set-key [(C-z)]   'execute-extended-command)
(global-set-key [(C-f12)]   'gdb-display-gdb-buffer)
(put 'upcase-region 'disabled nil)

(global-set-key "\C-c\C-r"
		'(lambda () (interactive) (revert-buffer t t)))
(global-set-key "\C-c\C-k" 'uncomment-region)

(if (not (daemonp)) (progn
		      (split-window-horizontally)
		      (server-start)
		      ))
;; set header format
(setq default-header-line-format '("--"
 (which-func-mode
  ("" which-func-format
   #("--" 0 2
     (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display")))) "%f"))

;; move out
(defun py-make-console-scetch ()
  (interactive)
  (let ((file-name (make-temp-file "emacs_test" nil ".py")))
    (find-file file-name)))

(put 'dired-find-alternate-file 'disabled nil)

(let ((w3m-path "/usr/share/emacs/site-lisp/w3m"))
  (if (file-exists-p w3m-path)
      (progn
	(add-to-list 'load-path w3m-path)
	(if window-system
	    (require 'w3m-load)))))

(defun grep-current-word ()
  (interactive)
  (grep (concat "find . -type f -exec grep -nHi "(current-word)" {} \\;")))

(defun wx-include ()
  (interactive)
  (let
      ((temp-buf-name "*incl-output*"))
    (shell-command (concat "wx.sh " (current-word)) temp-buf-name)
    (switch-to-buffer temp-buf-name)
    (kill-ring-save (point-min) (point-max))
    (kill-buffer temp-buf-name)
    ))

(defun wx-help ()
  (interactive)
  (browse-url (format "http://docs.wxwidgets.org/stable/wx_%s.html" (downcase (current-word)))))

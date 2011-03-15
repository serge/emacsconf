
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
  (desktop-save-mode 1)
  (setq desktop-dirname conf-path)
  (let ((my-path (concat conf-path "emacs")))
    ;; enable session saving
    (add-to-list 'load-path my-path)
    (load "codepad")
    (load "c++-settings")
    (load "my-git")
    (load "misc")
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
(split-window-horizontally)

(put 'narrow-to-region 'disabled nil)

(put 'narrow-to-page 'disabled nil)

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

;; move into misc
(if (string= system-type "windows-nt")
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
      ;(global-set-key [capslock] 'event-apply-control-modifier)
      (w32-send-sys-command WM_MAXIMIZE)))

(defun current-buffer-other-window ()
  (interactive)
  (switch-to-buffer-other-window (buffer-name)))
(global-set-key "\C-c\C-r"
		'(lambda () (interactive) (revert-buffer t t)))
(global-set-key "\C-c\C-k" 'uncomment-region)

;; move into misc
(if (string= system-type "gnu/linux")
    (progn
      (setq x-select-enable-clipboard t)
      (set-face-attribute 'default nil :family "Anonymous Pro" :height 110)
      ))

 ;Consolas
(server-start)

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

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(exec-path (quote ("~/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/git/bin" "/Applications/Emacs.app/Contents/MacOS/bin" "/opt/local/bin"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
(if window-system
   (require 'w3m-load))

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

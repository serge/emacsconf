;; *****************************************************************************
;; ******************************* GLOBAL C++ MODE CHANGES *********************
;; *****************************************************************************

;; make c++-mode the default mode for h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; *****************************************************************************
;; ******************************* VARIABLES ***********************************
;; *****************************************************************************

(defvar cpp-compiler "g++" "Store the name of the compiler to use with .cpp files")

;; *****************************************************************************
;; ******************************* FUNCTIONS ***********************************
;; *****************************************************************************

;; check is current file has one of the given extensions
(defun _private-c++-is-has-extension (ext-list file-ext)
  (if (not ext-list) nil
    (if (string= (car ext-list) file-ext) t
      (_private-c++-is-has-extension (cdr ext-list) file-ext))))

(defun cpp-make-temp-file-and-compile ()
  "Save current buffer in a file in a temp directory and compile"
  (interactive)
  (defun make-and-compile (buffer)
  (if (bufferp buffer) 
      (let ((filename (format "%s%s" temporary-file-directory buffer)))
        (find-file filename)
        (insert-buffer buffer)
        (basic-save-buffer)
        (cpp-compile-current-file))
    (message "Buffer %s isn't really a buffer")))
  (make-and-compile (get-buffer (buffer-name))))

;; compile single file
(defun cpp-compile-current-file ()
  "Compile and link a file visited in the current buffer"
  (interactive)
  (defun get-full-exename (exename ext)
    (cond
       ((string= system-type "gnu/linux") exename)
       ((string= system-type "windows-nt") exename)
       ((string= system-type "darwin") (concat exename "." ext))
       (t "none")))
  (let ((filename (buffer-file-name)))
    (if filename
        (let (
              (file-ext (file-name-extension filename))
              (exename (file-name-sans-extension filename))
              (compiler cpp-compiler)
              (options "-Wall")
              (exe-ext "exe")
              (accept-ext '("cpp" "c" "cxx" "cc"))
              )
          (if (_private-c++-is-has-extension accept-ext file-ext)
	      (let ((full-exename (get-full-exename exename exe-ext)))
		(compile (format "%s %s \"%s\" -o \"%s\"" compiler options filename full-exename))
		full-exename
		)
            (message "%s isn't a valid C/C++ file" filename)))
      (message "Buffer %s isn't visiting any file" (buffer-name)))))

;; compile and run current file
(defun cpp-compile-and-run-current-file ()
  "Compile link and run a file"
  (interactive)
  (shell-command (concat "\"" (cpp-compile-current-file) "\"&")))
;; add include guards
(defun cpp-add-include-guard ()
  "Add an include guard to the current header file"
  (interactive)
  (defun list-to-string (list node)
    (defun list-to-string-hp (list str node)
      (if list
          (list-to-string-hp (cdr list) (concat str node (car list)) node)
    str))
    (list-to-string-hp (cdr list) (car list) node))
  (let ((
   filename
   (list-to-string
    (split-string (upcase (buffer-name)) "[.]")
    "_")
   ))
  (push-mark)
  (goto-char (point-min))
  (let ((frm "%s __%s__\n"))
    (insert (format frm "#ifndef" filename))
    (insert (format frm "#define" filename))
    )
  (goto-char (point-max))
  (insert "\n#endif\n")
  ))
 
(defun cpp-swap-header-source ()
  "Switch to corresponding C/C++ header/source"
  (interactive)
  (defun get-conj-extension (extension)
    (defun is-in-list (list str)
      (if (not list) nil
        (if (equal (downcase (car list)) (downcase str)) t
          (is-in-list (cdr list) str))))
      (let ((src-ext '("cpp" "c" "cxx"))
            (hdr-ext '("h" "hpp")))
        (cond ((is-in-list src-ext extension) hdr-ext)
              ((is-in-list hdr-ext extension) src-ext)
              (t nil))))
    (defun switch-to-existing-buffer (bare-name ext-list pred)
      (if (not ext-list) nil
        (let ((next-name (concat bare-name "." (car ext-list))))
          (if (funcall pred next-name) next-name 
            (switch-to-existing-buffer bare-name (cdr ext-list) pred)))))
    (defun switch-to-conj-buffer-h (init-buffer-name pred)
      (let ((file-name (file-name-sans-extension init-buffer-name))
            (extensions (get-conj-extension (file-name-extension init-buffer-name))))
        (switch-to-existing-buffer file-name extensions pred)))
    (defun switch-conj-buffer-in-vcproj (file-name)
      (defun l-switch-to-1 (arg)
        (if (get-buffer arg) (switch-to-buffer-other-window arg) nil)) 
      (defun l-switch-to-2 (arg)
        (let ((solution (_private-find-file-by-extension (buffer-list) arg "vcproj")))
          (if solution
              (progn
                (switch-to-buffer solution)
                (if (_private-is-found (concat "RelativePath=\"\\([^\"]*[\\/]" arg "\\)"))
                    (let ((sub-path-string (replace-regexp-in-string "\\\\" "/" (match-string 1)))
                          (abs-path default-directory))
                      (bury-buffer solution)
                      (find-file (concat abs-path sub-path-string)))
                  (progn (bury-buffer solution) nil))
                )
            nil)))
      (let ((res (switch-to-conj-buffer-h file-name (quote l-switch-to-1))))
        (if res res (switch-to-conj-buffer-h file-name (quote l-switch-to-2)))))
    (let ((res (switch-conj-buffer-in-vcproj (buffer-name))))
      (if res (message "%s" res) (message "Couldn't find buffer"))))


(defun simplify-path (path)
  (defun simplify-path-once (path)
    (defun concat-path (path-node-list sep)
      (let
          ((path-node (car path-node-list))
           (rest-path-list (cdr path-node-list))
           (next-path-node (nth 1 path-node-list)))
        (if rest-path-list
            (cond
             ((string= path-node ".") (concat "" (concat-path rest-path-list sep)))
             ((and
                 (string= next-path-node "..")
                 (not (string= path-node "..")))
                (concat "" (concat-path (cdr rest-path-list) sep)))
             (t (concat path-node sep (concat-path rest-path-list sep))))
          path-node))
      )
    (concat-path (split-string path "\\(/\\|\\\\\\)") "/"))
  (let
      ((s-path (simplify-path-once path)))
    (if (= (length path) (length s-path))
        (if (string= "" s-path) "./" s-path)
      (simplify-path s-path))))

(defun _private-is-found (file-name)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward file-name (point-max) t)))

(defun _private-find-file-by-extension (file-list file ext)
  (if file-list
      (if (string= (file-name-extension (buffer-name (car file-list))) ext)
          (let ((solution-file (car file-list))
                (rest-of-the-list (cdr file-list)))
            (set-buffer solution-file)
            (if (_private-is-found file) solution-file
              (_private-find-file-by-extension rest-of-the-list file ext)))
        (_private-find-file-by-extension (cdr file-list) file ext))
    nil))

(defun msvc-get-add-include-path ()
  (let ((include-name "AdditionalIncludeDirectories=\"\\([^\"]+\\)\""))
    (beginning-of-buffer)
    (if (re-search-forward include-name (point-max) t)
          (split-string (replace-regexp-in-string "\&quot" "" (match-string 1)) "\\(,\\|;\\)")
      nil)))


(defun _private-make-include-compiler-specific (base include-list compiler)
  (defun make-gcc (include-list output)
    (if include-list
        (make-gcc (cdr include-list)
                               (format "%s\n-I\"%s\"" output
                                       (simplify-path (concat base (car include-list)))))
      output))
  (cond ((string= compiler "gcc") (make-gcc include-list ""))
        (t nil)))

(defun _private-get-compiler-settings (compiler)
  (cond ((string= compiler "gcc") "-c -Wall -Wno-deprecated")
        (t "")))

(defun _private-get-compiler-objects-rules (cpp-files base-dir compiler V_CC V_INC V_CFLAGS)
  (defun get-file (filename)
    (let ((bare-file-name (file-name-sans-extension (file-name-nondirectory filename))))
      (let ((out-file (format "%s%s.o" base-dir bare-file-name))
            (target-name (format "%s.o" bare-file-name)))
        (cons target-name (format "%s: %s\n\t$(%s) $(%s) \"%s\" -o\"%s\" $(%s)\n" target-name filename
                                     V_CC V_CFLAGS filename out-file V_INC)))))
  (defun make-gcc (cpp-files str all-rule)
    (if cpp-files
        (let ((res (get-file (car cpp-files))))
          (make-gcc (cdr cpp-files) (format "%s\n%s" str (cdr res)) (format "%s %s" all-rule (car res))))
      (cons all-rule str)))
  (cond ((string= compiler "gcc") (make-gcc cpp-files "" ""))
        (t (cons "" ""))))

(defun msvc-compile (file)
  (defun normalize-path (path bad-separator good-separator)
    (let ((path-list (split-string path bad-separator))
          (norm-path ""))
      (while path-list
        (let ((path-node (car path-list))
              (next-path-node (car (cdr path-list))))
          (if (not (and (not (string= path-node "..")) (string= next-path-node "..")))
              (setq norm-path (format "%s%s%s" norm-path good-separator (car path-list))))
          (setq path-list (cdr path-list))))
      norm-path))

  (defun get-path-difference (old new)
    (let (
          (path-separator "\\(/\\|\\\\\\)")
          (file-separator "[a-zA-Z0-9_]+.vcproj")
          )
      (car (split-string old file-separator))))

  (let ((sol-file (_private-find-file-by-extension (buffer-list) file "vcproj")))
    (if (bufferp sol-file)
        (progn
          (set-buffer sol-file)
          (save-excursion
            (let (
                  (includes (msvc-get-add-include-path))
                  (incl_params " ")
                  (out-file (format "\"%s%s.o\"" temporary-file-directory (file-name-sans-extension file)))
                  (comp-settings (_private-get-compiler-settings "gcc"))
                  (path-prefix (get-path-difference
                                (buffer-file-name sol-file)
                                (buffer-file-name (get-buffer file))))
                  )
              (if includes
                  (let ((incl_params (_private-make-include-compiler-specific path-prefix includes "gcc")))
                    (format "%s %s \n%s -o%s %s" cpp-compiler comp-settings file out-file incl_params))
                nil))))
      nil)))

(defun _private-create-makefile (cpp-files include-path compiler)
  (defun make-gcc (cpp-files include-path)
    (let ((V_CC "CC")
          (V_INC "INCLUDES")
          (V_OBJS "OBJS")
          (V_CFLAGS "CFLAGS"))
      (let ((rules (_private-get-compiler-objects-rules cpp-files temporary-file-directory "gcc"
                                                    V_CC V_INC V_CFLAGS))
            (include-dirs (_private-make-include-compiler-specific "" include-path "gcc")))
       (format "%s\n%s\n%s\n%s\n"
               (format "%s = %s" V_CC cpp-compiler)
               (format "%s = %s" V_INC (replace-regexp-in-string "
" " " include-dirs))
               (format "%s = %s" V_CFLAGS (_private-get-compiler-settings "gcc"))
               (format "all: %s\n\t@echo \"Done\"\n\n%s" (car rules) (cdr rules))))))
  (make-gcc cpp-files include-path))

(defun msvc-compile-all-in-vcproj (buffer)
  "Compile all source files which can be found in vcproj file"
  (interactive)
  (defun compile-file (filename)
    (let ((compile-line (msvc-compile filename)))
      (if compile-line (compile compile-line)
        nil)))
  (defun compile-all-in-vcproj (all-cpp-files)
    (let ((file-regexp "RelativePath=\"\\([^\"]+.cpp\\)\""))
      (if (re-search-forward file-regexp (point-max) t)
          (let ((filename (match-string 1)))
            (compile-all-in-vcproj (cons filename all-cpp-files)))
        all-cpp-files)))
    (set-buffer buffer)
    (save-excursion
      (if (string= (file-name-extension buffer) "vcproj")
          (progn
            (beginning-of-buffer)
            (let ((all-cpp-files (compile-all-in-vcproj '())))
              (set-buffer buffer)
	      (save-excursion
		(let ((includes-line (msvc-get-add-include-path))
		      (all-files-string "")
		      (make-file-name (make-temp-file "makefile"))
		      (base-dir (file-name-directory (buffer-file-name))))
		  (if includes-line
		      (progn
			(find-file make-file-name)
			(insert (_private-create-makefile all-cpp-files includes-line "gcc"))
			(save-buffer)
			(kill-buffer (current-buffer))
			(compile (format "make -k -f\"%s\"" make-file-name))
			)
		    (message "compile-line isn't good")))))
	    (message "%s isn't a Visual Studio project file" buffer)))))

(defun msvc-compile-all-in-vcproj-current ()
  (interactive)
  (msvc-compile-all-in-vcproj (buffer-name)))

(defun msvc-compile-current-file ()
  "Compile current file with gcc compiler using settings from a corresponding MS Visual C++ project file"
  (interactive)
  (let ((file (buffer-name))
        (cmdline (msvc-compile (buffer-name))))
    (if cmdline
        (progn
          (set-buffer file)
          (compile cmdline))
      (message "Solution file not found, make sure it's open"))))

(defun cpp-do-static-check ()
  "Check current file with cppcheck static analyzer"
  (interactive)
  (compile (format "cppcheck -a -s -v %s" (buffer-name))))

(defun _private-c++-get-comp-name ()
  (if compilation-buffer-name-function (funcall compilation-buffer-name-function)
    "*compilation*"))

(defun cpp-make-console-scetch ()
  (interactive)
  (let ((file-name (make-temp-file "emacs_test" nil ".cpp")))
    (let ((buf (find-file-literally file-name)))
      (c++-mode)
      (insert (concat "#include <string>\n"
		      "#include <iostream>\n"
		      "#include <fstream>\n"
		      "#include <algorithm>\n"
		      "#include <vector>\n"
		      "\n"
		      "int main(int argc, char** argv)\n"
		      "{\n\n"
		      "   return 0;\n"
		      "}\n"))
      (basic-save-buffer))))
;; *****************************************************************************
;; ************************* Editing tricks ************************************
;; *****************************************************************************

(defun cpp-center-current-function ()
  (interactive)
  (beginning-of-defun)
  (recenter-top-bottom 0))
(defun cpp-find-grep-current-word ()
  (interactive)
  (grep-find (format "find . -name \"*.%s\" -exec grep -nH -e %s {} \\;" (file-name-extension (buffer-name)) (current-word))))
(defun cpp-occur-current-word ()
  (interactive)
  (occur (current-word)))
(defun cpp-edit-add-include (filename)
  (interactive "s")
  (save-excursion
    (end-of-buffer)
    (if (search-backward-regexp "^#include" (point-min) t)
	(end-of-line)
      (progn (goto-char (point-min)) (message "asdfasdfasd"))
    (newline-and-indent)
    (insert (format "#include %s" filename)))))

;; *****************************************************************************
;; ************************* VTK UTILS *****************************************
;; *****************************************************************************

(defun cpp-vtk-make-include ()
 (interactive)
 (defun helper-cpp-make-include (word)
   (let (
	 (std_inc '("vector" "list" "map" "deque")))
     (defun strip-std (item)
       (if (string= (substring item 0 5) "std::") (substring item 5) item))
     (defun find-in-list (l item)
       (message (car l))
       (if l (if (string= (car l) item) t (find-in-list (cdr l) item))
	 nil))
     (cond
      ( (find-in-list std_inc (strip-std word)) (format "#include <%s>" (strip-std word)))
      ( (string= (substring word 0 3) "vtk") (format "#include <vtk/%s.h>" word))
      ( t (format "#include \"%s.h\"" word)))))
 (save-excursion
   (let ((inc (helper-cpp-make-include (current-word))))
     (search-backward-regexp "^#include")
     (end-of-line)
     (newline-and-indent)
     (insert inc))))
;; *****************************************************************************
;; ************************* KEY BINDING ***************************************
;; *****************************************************************************

(global-set-key [(f8)] 'cpp-swap-header-source)
(global-set-key [(C-f7)] 'msvc-compile-current-file)
(global-set-key [(f9)]   'compile)
(global-set-key [(C-f9)]   'recompile)
(global-set-key [(C-f3)] 'cpp-do-static-check)
(global-set-key [(f7)] 'cpp-compile-current-file)
(global-set-key [(C-f5)] 'cpp-compile-and-run-current-file)
(global-set-key [(M-f7)] '(lambda ()
                            "Switch to a compilation buffer"
                   (interactive)
                   (let ((buff (_private-c++-get-comp-name)))
                   (if (get-buffer buff) (switch-to-buffer buff)
                     (message "Buffer %s doesn't exist" buff)))))
(global-set-key [(C-M-f7)] '(lambda ()
                              "Kill buffer a compilation buffer"
                              (interactive)(kill-buffer (_private-c++-get-comp-name))))
;; *****************************************************************************
;; *********************************** HOOKS ***********************************
;; *****************************************************************************

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "stroustrup")
            (defun my-build-tab-stop-list (width)
              (let ((num-tab-stops (/ 80 width))
                    (counter 1)
                    (ls nil))
                (while (<= counter num-tab-stops)
                  (setq ls (cons (* width counter) ls))
                  (setq counter (1+ counter)))
                (set (make-local-variable 'tab-stop-list) (nreverse ls))))
            (setq tab-width 3) ;; change this to taste, this is what K&R uses :)
            (my-build-tab-stop-list tab-width)
            (setq c-basic-offset tab-width)
            (setq indent-tabs-mode nil))) ;; force only spaces for indentation

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>")  'hs-hide-block)
            (local-set-key (kbd "C-c <up>")    'hs-hide-all)
            (local-set-key (kbd "C-c <down>")  'hs-show-all)
            (hs-minor-mode t)))
    
(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\)[ ]*:" 1 font-lock-warning-face t)))))

(add-hook 'c-mode-common-hook
               (lambda ()
                (setq font-lock-string-face font-lock-warning-face)))


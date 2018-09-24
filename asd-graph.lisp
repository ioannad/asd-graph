#|

* ASD-GRAPH

This utility uses Graphviz Draw to visualise the dependencies
declared in a Common Lisp `<system-name>.asd` file.

Its designed to be run as a file with the location of the asd file to be drawn,
as argument:

Usage:
(load "asd-graph.lisp")
(asd-graph "~/path/to/asd-filename.asd")

Note: Unlike ASDF, asd-graph does not requirs that your .asd file has the same
name as the system you are defining. But only the first defined system will be 
used!

See REAMDE.md file for more details.


* TODO: Address safety concerns about using the READer
* TODO: Reconsider: 
        In the case of an .asd file with multiple 'DEFSYSTEM definitions,
        would ASDF choose the definition which matches the file name?
        If so, asd-graph should, too. 

|#

;; * Code

(ql:quickload :external-program)

;; ** Extracting system-definition and components, counting modules
;;
;; References to "packages" or "package" are removed.

(defun packages-string-p (string)
  (member string
	  (list "packages"
		"package"
		"\"packages\""
		"\"package\"")
	  :test 'equal))

(defun get-name (definition)
  (format nil "~(~a~)" (second definition)))

(defun get-components (system-definition)
  (loop for component in (getf system-definition :components)
     unless (packages-string-p (get-name component))
     collect component))

(defun module-p (component)
  (equal :module (first component)))

(defun module-with-count (module number)
  "Returns a different module, with a key :module-number."
  (append module (list :module-number number)))

(defun components-with-count (components &optional (count 1))
  "Recursively counts all the modules in =components=, and returns the updated
components."
  (let ((current-count count))
    (loop for component in components
       for new-component = component
       if (module-p component)
       do (setf new-component (module-with-count component current-count))
       and
       do (setf current-count (1+ current-count))
       and
       do (setf (getf new-component :components)
		(components-with-count (get-components component) current-count))
       end
       collect new-component)))

(defun get-flat-modules (system-definition)
  "Returns a new system-definition, with two keys :files and :modules, each
containing a list of names of the files and modules in the system."
  (loop for component in (get-components system-definition)
     when (module-p component)
     collect (get-name component)
     append (get-flat-modules component)))

(defun add-flat-modules (system-definition)
  (append system-definition
	  (list :modules (get-flat-modules system-definition))))

(defun add-module-count (system-definition)
  "Returns a different system-definition, with a key :amount-of-modules, and 
with each module with a key :module-number. Run after =add-flat-modules=."
  (let ((new-definition system-definition)
	(components (get-components system-definition))
	(count (length (getf system-definition :modules))))
    (setf (getf new-definition :components)
	  (components-with-count components))
    (append new-definition
	    (list :amount-of-modules count))))

(defun read-system-definition (asd-pathname)
  (with-open-file (in asd-pathname
		      :direction :input
		      :if-does-not-exist :error)
    (loop for system-definition = (read in)
       until (or (not system-definition)
		 (equal 'DEFSYSTEM (first system-definition)))
       finally
	 (return system-definition))))

(defun get-system-definition (asd-pathname)
  (add-module-count
   (add-flat-modules
    (read-system-definition asd-pathname))))

;; ** Manipulating file and path names

(defun resolve-tilde (path)
  "Returns a pathname from PATH, with the ~ referring to the user's home 
directory."
  (if (equal #\~ (first (coerce path 'list)))
      (merge-pathnames (subseq path 2)
		       (user-homedir-pathname))
      (pathname path)))

(defun get-path (asd-filename)
  (resolve-tilde asd-filename))

(defun get-directory (asd-pathname)
  (make-pathname :directory
		 (pathname-directory asd-pathname)))
  
(defun make-file-pathname (output-directory-path name format)
  (let ((filename (format nil "~a.~a" name format)))
    (merge-pathnames filename output-directory-path)))
  
;; ** Outputting dot syntax

(defun format-dot-beginning (stream system-name)
  (format stream "digraph \"~a\" {~%" system-name))

(defun format-dot-settings (stream)
  (format stream "overlap=false; ~%splines=ortho; ~%rankdir = LR; ~%")
  (format stream "node [shape=box]; ~%ranksep=1.5; ~%compound=true; ~%"))

(defun format-dot-ending (stream)
  (format stream "~%}~%"))

;; * TODO Colourful modules.
;; According to the dot manual:
;; #+BEGIN_QUOTE
;; A color value can be a hue-saturation-brightness triple (three floating
;; point numbers between 0 and 1, separated by commas); one of the colors names
;; listed in Appendix J (borrowed from some version of the X window system);
;; or a red-green-blue (RGB) triple (three hexadecimal number between 00 and FF,
;; preceded by the character ’#’).
;;#+END_QUOTE

(defun format-dot-arrow (stream origin target options)
  (format stream "\"~a\" -> \"~a\" " origin target)
  (when options
    (format stream "[~a]" options))
  (format stream "; ~%"))
  
(defun format-dot-dependency (stream component dependency system-definition
			      &aux options)
  (when (module-p component)
    (setf options (format nil "ltail=\"cluster~a\";" (get-name component))))
  (when (member dependency (getf system-definition :modules))
    (concatenate 'string options (format nil "lhead=\"cluster~a\"" dependency)))
  (format-dot-arrow stream (get-name component) dependency options))

(defun format-dot-dependencies (stream component system-definition)
  (loop for dependency in (getf component :depends-on)
     do (format-dot-dependency stream component dependency system-definition)))

;; color="0.2, 0.3, 1.0"; style=filled;

(defun format-dot-module (stream module system-definition)
  (let ((components (get-components module))
	(name (get-name module)))
    (format stream "subgraph \"cluster~a\" \{~%" name)
    (format stream "\"~a\" [shape=point style=invis];~%label=\"~a\";~%"
	    name name)
    (format stream "style=filled;~%color=\"~a, 0.4, 1.0\";~%"
	    (/ (getf module :module-number)
	       (+ 1.0 (getf system-definition :amount-of-modules))))
    (format-dot-components stream components system-definition)
    (format stream "                        \}~%")))

(defun format-dot-component (stream component system-definition)
  (if (module-p component)
      (format-dot-module stream component system-definition)
      (format stream "\"~a\";~%" (get-name component)))
  (format-dot-dependencies stream component system-definition))
  
(defun format-dot-components (stream components system-definition)
  (loop for component in components
     do (format-dot-component stream component system-definition)))

;; * Rendering with shiny R () in the branch =shiny-R=
;;
;; The following should get exported too, when this becomes a system.
;; It is used by the shiny R app, which can be found in the shiny-R
;; branch. With R and the library shiny installed on a server, it creates
;; a website on which you can upload .asd files and graph them.
;;

(defun asd->dot (asd-filename stream)
  "Prins a string with the contents of the dot file to stream."
  (let* ((asd-pathname (get-path asd-filename))
	 (system-definition (get-system-definition asd-pathname))
	 (system-name (get-name system-definition)))
    (format-dot-beginning stream system-name)
    (format-dot-settings stream)
    (format-dot-components stream
			   (getf system-definition :components)
			   system-definition)
    (format-dot-ending stream)))

(defun external-dot-arguments (format output-pathname system-name)
  (list	(format nil "-T~a" format)
	(make-file-pathname output-pathname
			    system-name
			    "dot")
	"-o"
	(make-file-pathname output-pathname
			    system-name
			    format)))

;; ## The main function

(defun asd-graph (asd-filename &key (output-dir nil) (format "svg") (debug nil))
  "Creates a SYSTEM-NAME.dot and a SYSTEM-NAME.FORMAT file."
  (let* ((asd-pathname (get-path asd-filename))
	 (system-definition (get-system-definition asd-pathname))
	 (system-name (get-name system-definition))
	 (directory-pathname (get-directory asd-pathname))
	 (output-pathname (if output-dir
			      (get-path output-dir)
			      directory-pathname)))
    (when debug
      (format t "System ~a contains ~a modules: ~{~a~^, ~} ~%"
	      system-name (getf system-definition :amount-of-modules)
	      (getf system-definition :modules)))
    (with-open-file (out (make-file-pathname output-pathname
					     system-name
					     "dot")
			 :direction :output
			 :if-exists :supersede)
      (asd->dot asd-filename out))
    (external-program:run  "/usr/bin/dot"
			   (external-dot-arguments format
						   output-pathname
						   system-name))))

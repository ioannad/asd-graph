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
		"package")
	  :test 'equal))

(defun get-components (system-definition)
  (loop for component in (getf system-definition :components)
     unless (packages-string-p (second component))
     collect component))

(defun module-p (component)
  (equal :module (first component)))

(defun count-modules (system-definition)
  (loop for component in (get-components system-definition)
     counting (module-p component)))

(defun add-module-count (system-definition)
  (append system-definition
	  (list :amount-of-modules
		(count-modules system-definition))))

(defun get-system-definition (asd-pathname)
  (with-open-file (in asd-pathname
		      :direction :input
		      :if-does-not-exist :error)
    (loop for system-definition = (read in)
       until (or (not system-definition)
		 (equal 'DEFSYSTEM (first system-definition)))
       finally
	 (return (add-module-count system-definition)))))


;; ## Manipulating file and path names

(defun resolve-tilde (path)
  "Returns a pathname from PATH, with the ~ referring to the user's home directory."
  (if (equal #\~ (first (coerce path 'list)))
      (merge-pathnames (subseq path 2)
		       (user-homedir-pathname))
      (pathname path)))

(defun get-path (asd-filename)
  (resolve-tilde asd-filename))

(defun get-directory (asd-pathname)
  (make-pathname :directory
		 (pathname-directory asd-pathname)))

(defun get-system-name (system-definition)
  (format nil "~(~a~)" (second system-definition)))
  
(defun make-file-pathname (output-directory-path name format)
  (let ((filename (format nil "~a.~a" name format)))
    (merge-pathnames filename output-directory-path)))
  
;; ## Outputting dot syntax

(defun format-dot-beginning (stream system-name)
  (format stream "digraph \"~a\" {~%" system-name))

(defun format-dot-settings (stream)
  (format stream "~%splines=ortho; ~%rankdir = LR; ~%node [shape=box]; ~%"))

(defun format-dot-ending (stream)
  (format stream "~%}~%"))

(defun format-dot-node (stream file)
  (format stream "~a;~%" file))

(defun format-dot-component (stream component)
  (let ((name (second component))
	(dependency-list (getf component :depends-on))
	);;add here nested component support
    (when dependency-list
      (format stream "~a -> {~{~a ~}}~%" name dependency-list))))

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
	 (system-name (get-system-name system-definition)))
    (format-dot-beginning stream system-name)
    (format-dot-settings stream)
    (loop for component in (get-components system-definition)
       for name = (second component)
       do (format-dot-node stream name)
       do (format-dot-component stream component))
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

(defun asd-graph (asd-filename &key (output-dir nil) (format "svg"))
  "Creates a SYSTEM-NAME.dot and a SYSTEM-NAME.FORMAT file."
  (let* ((asd-pathname (get-path asd-filename))
	 (system-definition (get-system-definition asd-pathname))
	 (system-name (get-system-name system-definition))
	 (directory-pathname (get-directory asd-pathname))
	 (output-pathname (if output-dir
			      (get-path output-dir)
			      directory-pathname)))
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

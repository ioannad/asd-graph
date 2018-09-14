#|

# ASD-GRAPH


This utility uses Graphviz Draw to visualise the dependencies
declared in a Common Lisp `<system-name>.asd` file.

See REAMDE.md file for more details.

|#

;; # Code

(ql:quickload :external-program)

;; ## Extracting system-definition
;;
;; References to \"packages\" or "\"package\"" are removed.

(defun get-system-definition (asd-pathname)
  (with-open-file (in asd-pathname
		      :direction :input
		      :if-does-not-exist :error)
    (loop for system-definition = (read in)
       until (or (not system-definition)
		 (equal 'DEFSYSTEM (first system-definition)))
       finally (return system-definition))))

(defun packages-string-p (string)
  (member string
	  (list "packages"
		"package")
	  :test 'equal))

(defun components (asd-pathname)
  (let ((system-definition (get-system-definition pathname)))
    (loop for component in (getf system-definition :components)
     unless (packages-string-p (second component))
     collect component)))
  
;; ## Manipulating file and path names

(defun resolve-tilde (path)
  "Returns a pathname from PATH, with the ~ referring to the user's home directory."
  (if (equal #\~ (first (coerce path 'list)))
      (merge-pathnames (subseq path 2)
		       (user-homedir-pathname))
      (pathname path)))

(defun split-path (asd-filename)
  "Returns three values: the resolved asd-pathname, the system-name, and the
 directory pathname to the file-name."
  (let* ((asd-pathname (resolve-tilde asd-filename))
	 (system-name (pathname-name asd-pathname))
	 (directory-pathname (make-pathname
			      :directory
			      (pathname-directory asd-pathname))))
    (values asd-pathname system-name directory-pathname)))

(defun make-file-pathname (output-pathname name format)
  (let ((filename (format nil "~a.~a" name format)))
    (merge-pathnames filename output-pathname)))
  
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

;; The following should get exported too, when this becomes a system.

(defun asd->dot (asd-filename stream)
  "Prins a string with the contents of the dot file to stream."
  (multiple-value-bind (asd-pathname system-name)
      (split-path asd-filename)
    (format-dot-beginning stream system-name)
    (format-dot-settings stream)
    (loop for component in (components asd-pathname)
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
  (multiple-value-bind (asd-pathname system-name directory-pathname)
      (split-path asd-filename)
    (declare (ignorable asd-pathname))
    (let ((output-pathname (if output-dir
			       (resolve-tilde output-dir)
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
						     system-name)))))

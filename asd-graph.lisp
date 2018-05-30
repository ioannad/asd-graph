#|

# ASD-GRAPH


This utility uses Graphviz Draw to visualise the dependencies
declared in a Common Lisp `<system-name>.asd` file.

See REAMDE.md file for more details.

|#

;; # Code

(ql:quickload :maxpc :silent t)
(ql:quickload :external-program :silent t)

;; ## Parsing

(defun =text-until (parser)
  "The input, which `parser` parses is not consumed."
  (maxpc:=subseq (maxpc:%some (maxpc:?not parser))))

(defun %skip-up-to-string (string)
  (maxpc:=destructure (_ _)
      (maxpc:=list (=text-until #1=(maxpc.char:?string string))
		   #1#)))

(defun %quote ()
  (maxpc.char:?string "\""))

(defun %skip-whitespace ()
  (maxpc:%any
    (maxpc.char:?whitespace)))

(defun =quoted-word ()
  (maxpc:=destructure (_ list _)
      (maxpc:=list (%skip-whitespace)
		   (maxpc:=subseq
		    (maxpc:?seq (%quote)
				(=text-until (%quote))
				(%quote)))
		   (%skip-whitespace))))

(defun =in-parens (parser)
  (maxpc:=destructure (_ content _)
      (maxpc:=list (maxpc.char:?string "(")
		   parser
		   (maxpc.char:?string ")"))))

(defun =dependency ()
  (maxpc:=destructure (_ file dependencies _)
      (maxpc:=list (%skip-up-to-string ":file")
		   (=quoted-word)
		   (maxpc:%maybe
		    (maxpc:=list
		     (maxpc.char:?string ":depends-on ")
		     (%skip-whitespace)
		     (=in-parens (maxpc:%some (=quoted-word)))))
		   (maxpc.char:?string ")"))
    (list file (third dependencies))))
  
(defun =dependencies ()
  (maxpc:%some (=dependency)))

;; ## Extracting dependencies
;;
;; References to \"packages\" or "\"package\"" are removed.

(defun get-dependencies (asd-pathname)
  (with-open-file (in asd-pathname
		      :direction :input
		      :if-does-not-exist :error)
    (maxpc:parse in (=dependencies))))

(defun packages-string-p (string)
  (member string
	  (list "\"packages\""
		"\"package\"")
	  :test 'equal))

(defun dependencies (asd-pathname)
  (loop for (file dependencies) in (get-dependencies asd-pathname)
     for clean-dependencies = (loop for dependency in dependencies
				 unless (packages-string-p dependency)
				 collect dependency)
     unless (packages-string-p file)
     collect (list file clean-dependencies)))
  
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

(defun format-dot-dependency (stream dependency)
  (destructuring-bind (file dependency-list) dependency
    (when dependency-list
      (format stream "~a -> {~{~a ~}}~%" file dependency-list))))
	      
;; The following should get exported too, when this becomes a system.

(defun asd->dot (asd-filename stream)
  "Prins a string with the contents of the dot file to stream."
  (multiple-value-bind (asd-pathname system-name)
      (split-path asd-filename)
    (format-dot-beginning stream system-name)
    (format-dot-settings stream)
    (loop for dependency in (dependencies asd-pathname)
       for file = (first dependency)
       do (format-dot-node stream file)
       do (format-dot-dependency stream dependency))
    (format-dot-ending stream)))

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
      (external-program:run
       "/usr/bin/dot"
       (list
	(format nil "-T~a" format)
	(make-file-pathname output-pathname
			    system-name
			    "dot")
	"-o"
	(make-file-pathname output-pathname
			    system-name
			    format))))))

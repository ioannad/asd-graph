#|

# ASD-GRAPH


This utility uses Graphviz Draw to visualise the dependencies
declared in a Common Lisp `<system-name>.asd` file.

See REAMDE.md file for more details.

|#

;; # Code

(ql:quickload :maxpc)
(ql:quickload :external-program)

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

(defun =dep ()
  (maxpc:=destructure (_ file deps _)
      (maxpc:=list (%skip-up-to-string ":file")
		   (=quoted-word)
		   (maxpc:%maybe
		    (maxpc:=list
		     (maxpc.char:?string ":depends-on ")
		     (%skip-whitespace)
		     (=in-parens (maxpc:%some (=quoted-word)))))
		   (maxpc.char:?string ")"))
    (list file (third deps))))
  
(defun =deps ()
  (maxpc:%some (=dep)))

;; ## Extracting dependencies
;;
;; References to \"packages\" or "\"package\"" are removed.

(defun get-deps (file-name)
  (with-open-file (in file-name
		      :direction :input
		      :if-does-not-exist :error)
    (maxpc:parse in (=deps))))

(defun packages-string-p (string)
  (member string
	  (list "\"packages\""
		"\"package\"")
	  :test 'equal))

(defun deps (file-name)
  (loop for (file deps) in (get-deps file-name)
     for clean-deps = (loop for d in deps
			 unless (packages-string-p d)
			 collect d)
     unless (packages-string-p file)
     collect (list file clean-deps)))
  
;; ## Manipulating file and path names

(defun split-path (file-name)
  (let ((name-start (+ 1 (search "/" file-name :from-end T))))
    (list
     (subseq file-name 0 name-start)
     (subseq file-name name-start (- (length file-name) 4)))))

(defun filename (path name ending)
  (concatenate 'string
	       path
	       name
	       "."
	       ending))

;; ## Outputting dot syntax

(defvar *dot-settings*
  "
splines=ortho;
rankdir = LR;
node [shape=box];")

(defvar *dot-ending*
  "}")

(defun dot-beginning (system-name)
  (format nil "digraph ~a {" system-name))

(defun dep->dot (dep)
  (destructuring-bind (file dep-list) dep
    (labels ((format% (d)
	     (format nil "~a -> ~a;" file d)))
      (format nil "~{~a~%~}" (mapcar #'format% dep-list)))))

(defun asd->dot (path file-name system-name)
    (with-open-file (out (filename path system-name "dot")
			 :direction :output
			 :if-exists :supersede)
      (format out (dot-beginning system-name))
      (format out *dot-settings*)
      (loop for dep in (deps file-name)
	 do (format out (dep->dot dep)))
      (format out *dot-ending*)))

;; ## The main function

(defun asd-graph (file-name &key (output-dir nil) (format "svg"))
  (destructuring-bind
	(path system-name) (split-path file-name)
    (unless output-dir
      (setf output-dir path))
    (asd->dot output-dir file-name system-name)
    (external-program:run
     "/usr/bin/dot"
     (list
      (format nil "-T~a" format)
      (filename output-dir system-name "dot")
      "-o"
      (filename output-dir system-name format)))))

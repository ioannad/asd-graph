(in-package :foo)

;; some comment

(defsystem test
  :components ((:file "packages")
	       (:file "package")
	       (:file "baz"
		      :depends-on ("x" z))
	       (:file "bla")
	       (:module "blah"
			:pathname "src"
			:depends-on ("x" "bla" z)
			:components ((:file "a")
				     (:file "b"))))
  :depends-on ("g" h))

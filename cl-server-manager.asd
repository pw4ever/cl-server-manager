;;;; cl-server-manager.asd

(asdf:defsystem #:cl-server-manager
  :serial t
  :description "Manage port-based servers (e.g., Swank and Hunchentoot) through a unified interface."
  :version "0.1"
  :license "MIT"
  :author "Wei Peng <write.to.peng.wei@gmail.com>"
  :depends-on (
	       #:alexandria
	       #:swank
	       #:hunchentoot
	       #:prepl
	       )
  :components (
	       (:file "package")
	       (:file "manager")
               (:file "util")

	       (:module types
			:components (
				     (:file "console")
				     (:file "http")
				     (:file "https")
				     ))

	       (:module vendors
			:components (
				     (:file "swank")
				     (:file "hunchentoot")
				     ))
	       ))


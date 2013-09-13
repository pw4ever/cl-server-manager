(in-package #:cl-server-manager)

;;; class and defaults
(defclass swank (server-console) 
  ((port-pathname :accessor port-pathname :initarg :port-pathname :initform nil
		  :documentation "pathname to save port")))
(register-server-vendor-by-type :console :swank "http://common-lisp.net/project/slime/")
(set-server-type-defaults :console
			  :class 'swank
			  :name :console
			  :make-server-initargs (list :port-pathname nil)
			  :port 0
			  :start-initargs nil)


;;; server-port operations

(defmethod start ((server swank) (port integer) &rest args &key &allow-other-keys)
  (let ((actual-port nil)
	(port-pathname (port-pathname server)))
    (let ((server-object 
	   (swank::setup-server port
				(lambda (port) (setf actual-port port))
				swank:*communication-style* ;:style
				t			       ; :dont-close
				nil		; :backlog ; deprecated
				)))
      (setf (gethash actual-port (server-ports server)) server-object)
      (when port-pathname
	(with-open-file (port-file port-pathname :direction :output :if-exists :overwrite :if-does-not-exist :create)
	  (format port-file "~A" actual-port)))
      server-object)))

(defmethod query ((server swank) (port integer) &rest args &key &allow-other-keys))

(defmethod stop ((server swank) (port integer) &rest args &key &allow-other-keys)
  (swank:stop-server port)
  (let ((port-pathname (port-pathname server)))
    (when (and port-pathname (probe-file port-pathname))
      (delete-file port-pathname))))



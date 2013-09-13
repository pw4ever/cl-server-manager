(in-package #:cl-server-manager)


;;; system parameters
(defparameter *servers* (make-hash-table :test 'equal) "Indexed by server NAME.")
(defparameter *server-types* (make-hash-table :test 'equal) "Indexed by server TYPE.")


;;; server classes
(defclass server ()
  ((name :accessor server-name :initarg :name
	 :documentation "Server instance name.")
   (ports :accessor server-ports :initform (make-hash-table :test 'equal)
	  :documentation "Client interface.")))


;;; server management

;;
(defgeneric make-server (server-class &rest args &key name &allow-other-keys)
  (:documentation "Make a server of SERVER-CLASS with a given NAME."))
(defmethod make-server (server-class &rest args &key name &allow-other-keys)
  (setf (gethash name *servers*) (apply #'make-instance server-class args)))

;;
(defun list-servers ()
  "List all server names."
  (loop for server being each hash-key of *servers* collect server))

;;
(defun find-server (&key name &allow-other-keys)
  "Find a server with NAME."
  (gethash name *servers*))

;;
(defun remove-server (&key name &allow-other-keys)
  "Remove a server with NAME."
  (remhash name *servers*))

;;
(defun shutdown-server (&key name &allow-other-keys)
  "Stop all PORTS of the server with NAME and remove it; return the STOP result for the PORTS."
  (multiple-value-bind (server presentp) (gethash name *servers*)
    (when presentp
      (let ((replies (loop for port being each hash-key of (server-ports server) 
			collect (stop server port))))
	(remove-server :name name)
	replies))))


;;; port management

;;
(defun list-ports (&key name &allow-other-keys)
  "List ports of the server with NAME."
  (let ((server (find-server :name name)))
    (when server
      (loop for port being each hash-key of (server-ports server) collect port))))

;;
(defun find-port (&key name port &allow-other-keys)
  "Find PORT of server with NAME."
  (let ((server (find-server :name name)))
    (when server
      (gethash port (server-ports server)))))

;;
(defun remove-port (&key name port &allow-other-keys)
  "Remove PORT of server with NAME."
  (let ((server (find-server :name name)))
    (when server
      (remhash port (server-ports server)))))


;;; server-port operations

;;
(defgeneric start (server port &rest args &key &allow-other-keys)
  (:documentation "Start PORT of SERVER."))
(defmethod start :around ((server (eql nil)) port &rest args &key &allow-other-keys)
  "Prevent nil start."
  (declare (ignore args))
  t					; operation trivally succeed
)
(defmethod start :around ((server server) port &rest args &key &allow-other-keys)
  "Prevent double start."
  (multiple-value-bind (port presentp) (find-port :name (server-name server) :port port)
    (if presentp
	port
	(when (next-method-p) (call-next-method)))))

;;
(defgeneric query (server port &rest args &key &allow-other-keys)
  (:documentation "Query PORT of SERVER."))

;;
(defgeneric stop (server port &rest args &key &allow-other-keys)
  (:documentation "Stop PORT of SERVER."))
(defmethod stop :around ((server (eql nil)) port &rest args &key &allow-other-keys)
  "Prevent nil stop."
  (declare (ignore args))
  t					; operation trivally succeed
)
(defmethod stop :around ((server server) port &rest args &key &allow-other-keys)
  "Prevent double stop."
  (multiple-value-bind (port presentp) (find-port :name (server-name server) :port port)
    (when presentp
      (when (next-method-p) (call-next-method))
      (remove-port :name (server-name server) :port port))))


;;; defaults

(defmacro define-default (name)
  "Define a default system parameter with accessor."
  (declare (type keyword name))
  (let* ((name (intern (concatenate 'string (symbol-name :default-server-) (symbol-name name))))
	 (var-name (intern (concatenate 'string (symbol-name :*) (symbol-name name) (symbol-name :*)))))
    `(progn
       (defparameter ,var-name (make-hash-table :test 'equal) "Indexed by SERVER-TYPE.")
       (defun ,name (server-type) (gethash server-type ,var-name))
       (defun (setf ,name) (value server-type) (setf (gethash server-type ,var-name) value)))))

(defmacro define-defaults (&rest names)
  "Define all default system parameters."
  `(progn ,@(loop for name in names collect `(define-default ,name))

	  (defun set-server-type-defaults (server-type &key ,@(loop for name in names collect 
								   (let ((keyword-var-name (intern (symbol-name name))))
								     keyword-var-name)) &allow-other-keys)
	    ,@(loop for name in names collect 
		   `(setf (,(let ((accessor-name (intern (concatenate 'string (symbol-name :default-server-) (symbol-name name)))))
				 accessor-name) 
			    server-type) 
			  ,(let ((keyword-var-name (intern (symbol-name name))))
				keyword-var-name))))

	  (defun show-server-type-defaults (server-type)
	    (list ,@(loop for name in names collect
			 `(cons ,name
				(,(let ((accessor-name (intern (concatenate 'string (symbol-name :default-server-) (symbol-name name)))))
				       accessor-name) 
				  server-type)))))))

(define-defaults :class :name :make-server-initargs :port :start-initargs)

(defmacro define-server-type-with-defaults (server-type &rest args)
  "Define SERVER-TYPE with defaults."
  `(progn (defclass ,(intern (concatenate 'string (symbol-name :server-) (symbol-name server-type))) (server) ())
	  (set-server-type-defaults ,server-type ,@args)
	  (multiple-value-bind (vendors presentp) (gethash ,server-type *server-types*)
	    (unless presentp (setf (gethash ,server-type *server-types*) (make-hash-table :test 'equal))))))


;;; server types and vendors

(defun list-server-types ()
  "List all server types."
  (hash-table-keys *server-types*))

(defun register-server-vendor-by-type (server-type vendor vendor-info)
  "Register server vendor by type."
  (setf (gethash vendor (gethash server-type *server-types*)) 
	vendor-info))

(defun list-server-vendors ()
  "List registered server vendors."
  (loop for server-type being each hash-key of *server-types* 
     collect (list server-type 
		   (hash-table-keys (gethash server-type *server-types*)))))

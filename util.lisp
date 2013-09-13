;;;; main event loop
(in-package #:cl-server-manager)

(defun launch-system-with-defaults (start-repl-p &rest server-types)
  "Load the SERVER-TYPES with defaults; launch REPL if START-REPL-P."
  (loop for server-type in server-types do
       (when (multiple-value-bind (class presentp) (gethash server-type *default-server-class*) presentp) 
	 (apply #'make-server (gethash server-type *default-server-class*)
		:name (gethash server-type *default-server-name*) 
		(gethash server-type *default-server-make-server-initargs*)) 
	 (apply #'start (find-server :name (gethash server-type *default-server-name*))
		(gethash server-type *default-server-port*)
		(gethash server-type *default-server-start-initargs*))))
  (when start-repl-p (prepl:repl)))

(defun shutdown-all-servers ()
  (loop for server in (list-servers) do (shutdown-server :name server)))

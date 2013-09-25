(in-package #:cl-server-manager)

;;; class and defaults


(defclass hunchentoot (server-http server-https) ())
(export 'hunchentoot)

;;; http
(register-server-vendor-by-type :http :hunchentoot "http://weitz.de/hunchentoot/")
(set-server-type-defaults :http
			  :class 'hunchentoot
			  :name :http
			  :make-server-initargs nil
			  :port 8085
			  :start-initargs (list :acceptor-class 'hunchentoot:easy-acceptor))

;;; https
(register-server-vendor-by-type :https :hunchentoot "http://weitz.de/hunchentoot/")
(setf *default-https-server-privatekey-pathname* (merge-pathnames "ssl-materials/server.key" (user-homedir-pathname)))
(setf *default-https-server-certificate-pathname* (make-pathname :type "crt" :defaults *default-https-server-privatekey-pathname*))
(set-server-type-defaults :https
			  :class 'hunchentoot
			  :name :https
			  :make-server-initargs nil
			  :port 8086
			  :start-initargs (list :acceptor-class 'hunchentoot:easy-ssl-acceptor :ssl-certificate-file *default-https-server-certificate-pathname* :ssl-privatekey-file *default-https-server-privatekey-pathname*))


;;; server-port operations

(defmethod start ((server hunchentoot) (port integer) &rest args &key (acceptor-class 'hunchentoot:acceptor) &allow-other-keys)
  (when acceptor-class
    (let ((server-object (hunchentoot:start (apply #'make-instance acceptor-class :port port (remove-from-plist args :acceptor-class)))))
      (setf (gethash port (server-ports server)) server-object))))

(defmethod query ((server hunchentoot) (port integer) &rest args &key &allow-other-keys))

(defmethod stop ((server hunchentoot) (port integer) &rest args &key &allow-other-keys)
  (hunchentoot:stop (find-port :name (server-name server) :port port)))



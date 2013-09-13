(defpackage #:cl-server-manager
  (:use 
   #:cl 
   #:alexandria
   )
  (:export
   #:make-server
   #:list-servers
   #:find-server
   #:remove-server
   #:shutdown

   #:start
   #:status
   #:stop
   #:list-ports
   #:find-port
   #:remove-port

   #:list-server-types
   #:register-server-vendor-by-type
   #:list-server-vendors

   #:launch-system-with-defaults
   #:shutdown-all-servers
   )
  )


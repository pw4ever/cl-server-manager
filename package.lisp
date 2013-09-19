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
   #:shutdown-server

   #:list-ports
   #:find-port
   #:remove-port

   #:start
   #:query
   #:stop

   #:list-server-types
   #:register-server-vendor-by-type
   #:list-server-vendors

   #:launch-system-with-defaults
   #:shutdown-all-servers
   )
  )


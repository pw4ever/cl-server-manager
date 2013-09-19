(in-package #:cl-server-manager)

(define-server-type-with-defaults :https 
    :class nil
    :name :https
    :make-server-initargs nil
    :port nil
    :start-initargs nil)

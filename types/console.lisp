(in-package #:cl-server-manager)

(define-server-type-with-defaults :console 
    :class nil
    :name :console
    :make-server-initargs nil
    :port nil
    :start-initargs nil)

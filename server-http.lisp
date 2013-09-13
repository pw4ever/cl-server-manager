(in-package #:cl-server-manager)

(define-server-type-with-defaults :http
    :class nil
    :name :http
    :make-server-initargs nil
    :port nil
    :start-initargs nil)

(in-package #:cl-server-manager)

;;; comment out the line to disable loading a server vendor
(setf *features* (adjoin :cl-server-manager-swank *features*))
(setf *features* (adjoin :cl-server-manager-hunchentoot *features*))

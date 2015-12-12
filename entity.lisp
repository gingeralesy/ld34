(in-package #:ld34)

(defclass entity ()
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location (vec 0 0 0)))

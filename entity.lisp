(in-package #:ld34)

(defgeneric updatable (updatable))
(defclass updatable () ())

(defclass entity (updatable)
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location (vec 0 0 0)))

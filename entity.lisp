(in-package #:ld34)

(defgeneric updatable (updatable))
(defclass updatable () ())

(defclass entity (updatable)
  ((location :initarg :location :accessor location)
   (name :initarg :name :accessor name))
  (:default-initargs
   :location (vec 0 0 0)
   :name "generic-entity"))

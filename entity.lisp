(in-package #:ld34)
(in-readtable :qtools)

(defgeneric update (updatable))
(defclass updatable () ())

(defclass entity (updatable)
  ((location :initarg :location :accessor location)
   (name :initarg :name :accessor name)
   (level :initarg :level :accessor level))
  (:default-initargs
   :location (vec 0 0 0)
   :name :generic
   :level (error "Please define level.")))

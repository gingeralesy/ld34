(in-package #:ld34)
(in-readtable :qtools)

;; Level

(defclass level (paintable clock)
  ((extend :initform '(-4096 +4096 -4096 +4096) :accessor extent)
   (timers :initform (make-hash-table :test 'eql) :accessor timers)
   (objects :initform (make-array 8
                                  :fill-pointer 0
                                  :adjustable T
                                  :element-type 'updatable)
            :accessor objects)))

(defmethod initialize-instance :after ((level level) &key)
  (enter (make-instance 'origin) level))

(defmethod update ((level level))
  (call-next-method))

(defmethod cap ((level level) vec)
  (destructuring-bind (left right bottom top) (extent level)
    (setf (x vec) (max left (min right (x vec)))
          (y vec) (max bottom (min top (y vec)))))
  vec)

;; Clock

(defclass clock (updatable)
  ((previous-time :initform (get-internal-real-time) :accessor previous-time)
   (clock :initarg :clock :accessor clock)
   (running :initarg :running :accessor running))
  (:default-initargs
   :clock 0.0s0
   :running NIL))

(defmethod reset ((clock clock))
  (setf (clock clock) 0.0s0)
  (setf (previous-time clock) (get-internal-real-time))
  clock)

(defmethod stop ((clock clock))
  (setf (running clock) NIL)
  clock)

(defmethod start ((clock clock))
  (setf (previous-time clock) (get-internal-real-time))
  (setf (running clock) T)
  clock)

(defmethod sync ((clock clock) (with clock))
  (setf (clock clock) (clock with)
        (previous-time clock) (get-internal-real-time))
  clock)

(defmethod update :before ((clock clock))
  (let ((new-time (get-internal-real-time)))
    (incf (clock clock)
        (float (/ (- new-time (previous-time clock))
                  internal-time-units-per-second)
               1.0s0))
    (setf (previous-time clock) new-time)))

(defmethod update :around ((clock clock))
  (when (running clock)
    (call-next-method)))

;; Origin
(defclass origin (entity paintable) ())

(defmethod name ((origin origin))
  :origin)

(defmethod paint ((origin origin) target)
  (call-next-method)
  (setf (q+:color (q+:brush target)) (q+:qt.white))
  (q+:draw-ellipse target
                   (round (- (x (location origin)) 10))
                   (round (- (y (location origin)) 10))
                   20 20))

;; Sprite entity
(defclass sprite-entity (entity animatable) ())

;; Entity with Hitbox
(defclass hitbox-entity () ())

;; Entity with HP
(defclass damageable-entity (entity)
  ((health :initargs :health :accessor health))
  (:default-initargs :health 100))

(defmethod damage ((entity damageable-entity) amount)
  (decf (health entity) amount))

(defmethod paint :after ((entity damageable-entity) target)
  (when (< 0 (health entity))
    (q+:fill-rect target
                  (round (- (x (location entity)) (health entity)))
                  (round (- (y (location entity)) 2))
                  (round (* (health entity) 2))
                  2
                  (q+:qt.red))))

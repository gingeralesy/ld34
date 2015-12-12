(in-package #:ld34)
(in-readtable :qtools)

(defclass object-container () ())
(defclass level (paintable clock object-container) ())
(defclass clock (updatable) ())
(defclass origin (entity paintable) ())
(defclass sprite-entity (entity animatable) ())
(defclass hitbox (entity) ())
(defclass hitbox-entity (entity object-container) ())
(defclass damageable-entity (entity) ())

;; Container

(defclass object-container ()
  ((objects :initform (make-array 8
                                  :fill-pointer 0
                                  :adjustable T
                                  :element-type 'updatable)
            :accessor objects)))

(defmethod enter ((object updatable) (container object-container))
  (vector-push-extend object (objects container)))

;; Level

(defclass level (paintable clock object-container)
  ((extend :initform '(-4096 +4096 -4096 +4096) :accessor extent)
   (timers :initform (make-hash-table :test 'eql) :accessor timers)))

(defmethod initialize-instance :after ((level level) &key)
  (enter (make-instance 'origin) level))

(defmethod enter ((object updatable) (level level))
  (unless (typep object 'paintable)
    (error (format NIL "Invalid entity type: ~a" object)))
  (vector-push-extend object (objects level)))

(defmethod update ((level level))
  (call-next-method))

(defmethod paint ((level level) target)
  (let ((entities (objects level)))
    (dotimes (i (length entities))
      (let ((entity (elt entities i)))
        (paint entity target)))))

(defmethod cap ((level level) vec)
  (destructuring-bind (left right bottom top) (extent level)
    (setf (x vec) (max left (min right (x vec)))
          (y vec) (max bottom (min top (y vec)))))
  vec)

(defmethod timer-ready-p (name timeout (level level))
  (let ((last (gethash name (timers level))))
    (cond ((not last)
           (setf (gethash name (timers level)) (clock level))
           NIL)
          ((< (+ last timeout) (clock level))
           (setf (gethash name (timers level)) (clock level))))))

(defmacro with-timer-ready ((name timeout &optional (level '(level))) &body body)
  `(when (timer-ready-p ,name ,timeout ,level)
     ,@body))

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
(defclass origin (entity paintable) ()
  (:default-initargs
   :name :origin))

(defmethod paint ((origin origin) target)
  (setf (q+:color (q+:brush target)) (q+:qt.white))
  (q+:draw-ellipse target
                   (round (- (vx (location origin)) 10))
                   (round (- (vy (location origin)) 10))
                   20 20))

;; Sprite entity
(defclass sprite-entity (entity animatable) ())

;; Hitbox class
(defclass hitbox (entity)
  ((w :initarg :w :accessor w)
   (h :initarg :h :accessor h))
  (:default-initargs
   :w 32 :h 32))

;; Entity with Hitbox
(defclass hitbox-entity (entity object-container) ())

(defmethod enter ((hitbox updatable) (entity hitbox-entity))
  (unless (typep hitbox 'hitbox)
    (error "May only store hitboxes into this collection."))
  (vector-push-extend hitbox (objects entity)))

;; Entity with HP
(defclass damageable-entity (entity)
  ((health :initarg :health :accessor health))
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

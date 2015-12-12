(in-package #:ld34)
#|
| Classes defined here:
|  - paintable
|  - animatable
|  - animation
|  - frame
|#

;; Generics
(defgeneric call-with-translation (func target vec))

(defmethod call-with-translation (func (target qobject) vec)
  (q+:save target)
  (unwind-protect
       (progn (q+:translate target (round (vx vec)) (round (vy vec)))
              (funcall func)))
  (q+:restore target))

;; Macros
(defmacro with-translation ((vec target) &body body)
  `(call-with-translation (lambda () ,@body) ,target ,vec))

;; Paintable class
(defgeneric paint (paintable target))

(defclass paintable ()
  ((visibility :initarg :visibility :accessor visibility))
  (:default-initargs :visibility 1.0))

;; Animatable class
(defclass animatable (paintable)
  ((animations :initform (make-hash-table :test 'eql)
               :accessor animations)
   (animation :initarg :default-animation :accessor animation)
   (frame :initform 0 :accessor frame)
   (spritesheet :initarg :spritesheet :accessor spritesheet))
  (:default-initargs :default-animation :idle))

(defmethod paint ((paintable animatable) target)
  (let ((image (sprite (elt (frames (gethash (animation paintable)
                                             (animations paintable)))
                            (frame paintable)))))
    (q+:draw-image target
                   (round (/ (q+:width image) -2))
                   (q+:height image)
                   image)))

;; Animation class
(defclass animation ()
  ((frames :initform (make-array 8
                                 :fill-pointer 0
                                 :adjustable T
                                 :element-type 'frame)
           :accessor frames)))

;; Frame class
(defclass frame ()
  ((duration :initarg :duration :accessor duration)
   (sprite :initarg :sprite :accessor sprite)
   (area :initarg :area :accessor area))
  (:default-initargs
   :duration 1.0
   :sprite (error "Sprite needed!")))

(defmethod initialize-instance :after ((frame frame) &key)
  (setf (sprite frame) (sprite frame)))

(defmethod (setf sprite) (sprite (frame frame))
  (etypecase sprite
    (string (setf (sprite frame) (asset 'image sprite)))
    (pathname (setf (sprite frame)
                    (q+:make-qimage (uiop:native-namestring sprite))))
    (qobject
     (unless (qtypep sprite "QImage")
       (error "~s is not of class QImage." sprite))
     (setf (slot-value frame 'sprite) sprite))))

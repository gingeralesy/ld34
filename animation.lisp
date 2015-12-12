(in-package #:ld34)
(in-readtable :qtools)

;; Classes of this file
(defclass paintable () ())
(defclass animatable () ())
(defclass animation () ())
(defclass frame () ())

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

(defmacro define-animation (name options &body sequences)
  (format T "Name: ~a~%Options:~a" name options)
  `(progn ,@sequences))

;; Paintable class
(defgeneric paint (paintable target))
(defgeneric sprite (paintable))

(defclass paintable ()
  ((visibility :initarg :visibility :accessor visibility))
  (:default-initargs :visibility 1.0))

;; Animatable class
(defclass animatable (paintable)
  ((animations :initform NIL :accessor animations)
   (default-animation :initarg :default-animation :accessor default-animation)
   (frame :initform 0 :accessor frame)
   (spritesheet :initarg :spritesheet :accessor spritesheet))
  (:default-initargs :default-animation :idle))

(defmethod (setf animation) ((entity animatable) (animation animation))
  (setf (getf (animations entity) (name animation)) animation))

(defmethod animation ((entity animatable) name)
  (getf (animations entity) name))

(defmethod sprite ((obj animatable))
  (let* ((animation (or (gethash (animation obj) (animations obj))
                        (error (format NIL "Invalid animation requested: ~a"
                                       (animation obj)))))
         (frame (or (elt (frames animation) (frame obj))
                    (error (format NIL "Invalid frame '~a' for animation '~a'"
                                   (frame obj) animation)))))
    (sprite frame)))

(defmethod paint ((obj animatable) target)
  (let ((image (sprite obj)))
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
           :accessor frames)
   (name :initarg :name :accessor name))
  (:default-initargs
   :name (error "Must define a name.")))

;; Frame class
(defclass frame ()
  ((duration :initarg :duration :accessor duration)
   (sprite :initarg :sprite :accessor sprite)
   (area :initarg :area :accessor area))
  (:default-initargs
   :duration 1.0
   :sprite (error "Sprite needed!")
   :area (vec 32 32 0)))

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

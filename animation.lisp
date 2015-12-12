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

;; Generic macros
(defmacro with-translation ((vec target) &body body)
  `(call-with-translation (lambda () ,@body) ,target ,vec))

;; Paintable class
(defgeneric paint (paintable target))
(defgeneric sprite (paintable))

(defclass paintable ()
  ((visibility :initarg :visibility :accessor visibility))
  (:default-initargs :visibility 1.0))

;; Animatable class
(defclass animatable (paintable)
  ((animations :initform NIL :accessor animations)
   (current-animation :initarg :default-animation :accessor current-animation)
   (current-frame :initform 0 :accessor current-frame)
   (spritesheet :initarg :spritesheet :accessor spritesheet))
  (:default-initargs :default-animation (error "Please define the default animation.")))

(defmethod (setf animation) ((entity animatable) (animation animation))
  (setf (getf (animations entity) (name animation)) animation))

(defmethod animation ((entity animatable) &optional name)
  (getf (animations entity) (or name (current-animation animation))))

(defmethod sprite ((obj animatable))
  (let* ((animation (or (animation obj)
                        (error (format NIL "Invalid animation requested: ~a"
                                       (current-animation obj)))))
         (frame (or (frame animation (current-frame obj))
                    (error (format NIL "Invalid frame '~a' for animation '~a'"
                                   (current-frame obj) (name animation))))))
    (sprite frame)))

(defmethod paint ((obj animatable) target)
  (let ((image (sprite obj)))
    (q+:draw-image target
                   (round (/ (q+:width image) -2))
                   (q+:height image)
                   image)))

;; Animation class
(defclass animation ()
  ((frames :initform NIL :accessor frames)
   (name :initarg :name :accessor name))
  (:default-initargs
   :name (error "Must define a name.")))

(defmethod (setf frame) ((animation animation) (frame frame))
  (push (frames animation) frame))

(defmethod frame ((animation animation) index)
  (nth index (frames animation)))

(defmacro define-animation (name options &body sequences)
  (make-instance 'animation :name name)
  `(progn ,@sequences))

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

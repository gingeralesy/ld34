(in-package #:ld34)
(in-readtable :qtools)

;; Classes of this file
(defclass paintable () ())
(defclass animatable () ())
(defclass animation () ())
(defclass frame () ())

;; Generics
(defgeneric (setf animation) (list animatable))
(defgeneric (setf animation) (animation animatable))
(defgeneric (setf frame) (frame animation))
(defgeneric (setf sprite) (sprite frame))

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

(defmethod (setf animation) ((animations list) (entity animatable))
  "Adds a list of animations to the entity."
  (loop for animation in animations
        do (setf (animation entity) animation)))

(defmethod (setf animation) ((animation animation) (entity animatable))
  "Adds a single animation to the entity."
  (setf (getf (animations entity) (name animation)) animation))

(defmethod animation ((entity animatable) &optional name)
  "Gets the animation by the given name from the entity."
  (getf (animations entity) (or name (current-animation entity))))

(defmethod sprite ((animatable animatable))
  "Gets the current sprite for the entity."
  (let* ((animation (or (animation animatable NIL)
                        (error (format NIL "Invalid animation requested: ~a"
                                       (current-animation animatable)))))
         (frame (or (frame animation (current-frame animatable))
                    (error (format NIL "Invalid frame '~a' for animation '~a'"
                                   (current-frame animatable) (name animation))))))
    (or (sprite frame)
        (error (format NIL "Missing sprite '~a' for animation '~a'"
                       (index frame) (current-animation animatable))))))

(defmethod paint ((animatable animatable) target)
  (let ((image (sprite animatable)))
    (q+:draw-image target
                   (round (/ (q+:width image) -2))
                   (q+:height image)
                   image)))

;; Animation class
(defclass animation ()
  ((frames :initform (make-queue) :accessor frames)
   (name :initarg :name :accessor name))
  (:default-initargs
   :name (error "Must define a name.")))

(defmethod push-frame ((frame frame) (animation animation))
  "Adds a frame to the frame queue."
  (queue-push frame (frames animation)))

(defmethod frame ((animation animation) index)
  "Gets the nth frame in the frame queue."
  (when (queue-empty-p (frames animation))
    (error "The frame list for animation ~a is empty." (name animation)))
  (queue-nth index (frames animation)))

(defmacro define-animation (name options &body sequences)
  "Constructs an animation with the frames for it."
  `(let (animations (file (getf ',options :file)))
     ,(if sequences
          `(loop for sequence in ',sequences
                 do (let ((animation (make-instance 'animation :name (getf sequence :sequence ',name))))
                      (loop for frame-info in (getf sequence :frames)
                            do (push-frame
                                (etypecase frame-info
                                  (list
                                   (let ((index (pop frame-info)))
                                     (make-frame file index ',options
                                                 :offset (or (getf frame-info :offset)
                                                             (getf sequence :offset))
                                                 :duration (or (getf frame-info :duration)
                                                               (getf sequence :duration)))))
                                  (number (make-frame file frame-info ',options
                                                      :offset (getf sequence :offset)
                                                      :duration (getf sequence :duration))))
                                animation))
                      (push animation animations)))
          `(let ((animation (make-instance 'animation :name ',name)))
             (loop for frame-info in (getf ',options :frames)
                   do (push-frame
                       (etypecase frame-info
                         (list
                          (let ((index (pop frame-info))) ;; remove index
                            (make-frame file index ',options
                                        :offset (getf frame-info :offset)
                                        :duration (getf frame-info :duration))))
                         (number (make-frame file frame-info ',options)))
                       animation))
             (push animation animations)))
     (unless (< 0 (length animations))
       (error "No animations specified."))
     animations))

;; Frame class
(defclass frame ()
  ((index :initarg :index :accessor index)
   (duration :initarg :duration :accessor duration)
   (sprite :initarg :sprite :accessor sprite)
   (offset :initarg :offset :accessor offset))
  (:default-initargs
   :index (error "Define index in animation.")
   :duration (error "Must define duration.")
   :sprite (error "Must define sprite-sheet.")
   :offset (error "Must define an offset.")))

(defmethod initialize-instance :after ((frame frame) &key)
  (setf (sprite frame) (sprite frame)))

(defmethod (setf sprite) (sprite (frame frame))
  (etypecase sprite
    (string
     (let ((image (asset 'image sprite)))
       (when (q+:is-null image)
         (error "File does not exist: ~a" sprite))
       (setf (sprite frame) (asset 'image sprite))))
    (pathname
     (let ((filepath (uiop:native-namestring sprite)))
       (unless (probe-file filepath)
         (error "File does not exist: ~a" filepath))
       (let ((image (q+:make-qimage filepath)))
         (when (q+:is-null image)
           (error "File does not exist: ~a" sprite))
         (setf (sprite frame) image))))
    (qobject
     (unless (and (qtypep sprite "QImage") (not (q+:is-null sprite)))
       (error "~s is not of class QImage." sprite))
     (setf (slot-value frame 'sprite) sprite))))

(defun make-frame (file index options &key offset duration)
  (make-instance 'frame
                 :index index
                 :sprite file
                 :offset (or offset (getf options :offset))
                 :duration (or duration (getf options :duration))))

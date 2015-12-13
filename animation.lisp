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
                        (error "Invalid animation requested: ~a"
                               (current-animation animatable))))
         (frame (or (frame animation (current-frame animatable))
                    (error "Invalid frame '~a' for animation '~a'"
                           (current-frame animatable) (name animation)))))
    (or (sprite frame)
        (error "Missing sprite '~a' for animation '~a'"
               (index frame) (current-animation animatable)))))

(defmethod paint ((animatable animatable) target)
  (let ((image (sprite animatable)))
    (q+:draw-image target
                   (round (/ (q+:width image) -2))
                   (q+:height image)
                   image)))

;; Animation class
(defvar *animations* (make-hash-table :test 'eql))

(defun animation (name &optional error-p)
  (or (gethash name *animations*)
      (and error-p (error "No such animation ~s." name))))

(defun (setf animation) (animation name)
  (setf (gethash name *animations*) animation))

(defun remove-animation (name)
  (remhash name *animations*))

(defclass animation ()
  ((name :initarg :name :accessor name)
   (sprite :initarg :sprite :accessor sprite)
   (sequences :initarg :sequences :accessor sequences))
  (:default-initargs
   :name (error "Must define a name.")
   :sprite (error "Must define a sprite.")))

(defmethod initialize-instance :after ((animation animation) &key)
  (setf (sprite animation) (sprite animation)))

;; This is a memory leak!!
(defmethod (setf sprite) (sprite (animation animation))
  (etypecase sprite
    (string
     (let ((image (asset 'image sprite)))
       (when (q+:is-null image)
         (error "File does not exist: ~a" sprite))
       (setf (sprite animation) (asset 'image sprite))))
    (pathname
     (let ((filepath (uiop:native-namestring sprite)))
       (unless (probe-file filepath)
         (error "File does not exist: ~a" filepath))
       (let ((image (q+:make-qimage filepath)))
         (when (q+:is-null image)
           (error "File does not exist: ~a" sprite))
         (setf (sprite animation) image))))
    (qobject
     (unless (and (qtypep sprite "QImage") (not (q+:is-null sprite)))
       (error "~s is not of class QImage." sprite))
     (setf (slot-value animation 'sprite) sprite))))

(defmacro define-animation (name options &body sequences)
  "Constructs an animation with the frames for it."
  (let ((defaults (copy-list options))
        (file (or (getf options :file)
                  (string name))))
    (remf defaults :file)
    `(progn
       (setf (animation ',name)
             (make-instance
              'animation
              :name ',name
              :sprite ,file))
       ,@(loop for sequence in sequences
               collect `(define-frame-sequence ,@(rest sequence))))))

(trivial-indent:define-indentation define-animation (2 4 &rest (&whole 2 0 4 &body)))

;;; Sample:


;; Sequence class
(defclass frame-sequence ()
  ((frames :initform (make-queue) :accessor frames)
   (name :initarg :name :accessor name))
  (:default-initargs
   :name (error "Must define a name.")))

(defmethod push-frame ((frame frame) (frame-sequence frame-sequence))
  "Adds a frame to the frame queue."
  (queue-push frame (frames frame-sequence)))

(defmethod frame ((frame-sequence frame-sequence) index)
  "Gets the nth frame in the frame queue."
  (when (queue-empty-p (frames frame-sequence))
    (error "The frame list for frame-sequence ~a is empty." (name frame-sequence)))
  (queue-nth index (frames frame-sequence)))

(defun frame-sequence (animation name)
  (find name (sequences (animation animation T)) :key #'name))

(defun (setf frame-sequence) (frame-sequence animation name)
  (let* ((animation (animation animation T))
         (pos (position name (sequences animation) :key #'name)))
    (if pos
        (setf (nth pos (sequences animation)) frame-sequence)
        (push frame-sequence (sequences animation))))
  frame-sequence)

(defun remove-frame-sequence (animation name)
  (let ((animation (animation animation T)))
    (setf (sequences animation)
          (remove name (sequences animation) :key #'name))))

(defmacro define-frame-sequence ((animation name) defaults &body frames)
  `(setf (frame-sequence ',animation ',name)
         (make-instance
          'frame-sequence
          :name ',name
          :frames (list ,@(loop for (index . args) in frames
                                collect `(make-frame ,index ,@args ,@defaults))))))

;; Frame class
(defclass frame ()
  ((index :initarg :index :accessor index)
   (duration :initarg :duration :accessor duration)
   (offset :initarg :offset :accessor offset))
  (:default-initargs
   :index (error "Define index in animation.")
   :duration (error "Must define duration.")
   :offset (error "Must define an offset.")))

(defun make-frame (index &key offset duration)
  (make-instance 'frame
                 :index index
                 :offset offset
                 :duration duration))

(defun offset (offset step index)
  (list (+ (car offset) (* index (car step)))
        (+ (cdr offset) (* index (cdr step)))))

(in-package #:ld34)
(in-readtable :qtools)

(defvar *standalone* NIL
  "Whether this build is stand-alone or ASDF dependant.")
(defvar *asset-cache* (make-hash-table :test 'equalp)
  "Cache assets here after load.")

;; Assets
(defun asset-filetype (type)
  "Gets the file extension for a type of an asset."
  (ecase type (image "png")))

(defun asset-storage (type)
  "Gets the storage for a type of an asset."
  (ecase type (image "graphics")))

(defun asset (type file)
  "Gets the wanted asset."
  (let* ((filetype (asset-filetype type))
         (path (format NIL "~(~a/~a.~a~)"
                       (asset-storage type) file filetype)))
    (or (gethash path *asset-cache*)
        (setf (gethash path *asset-cache*)
              (let ((path (uiop:native-namestring
                           (if *standalone*
                               (merge-pathnames path (uiop:argv0))
                               (asdf:system-relative-pathname :ld34 path)))))
                (ecase type (image (q+:make-qimage path filetype))))))))

;; Vectors
(defun translate-vec (vec with)
  "Translates a vector with another."
  (vec (+ (vx vec) (vx with))
       (+ (vy vec) (vy with))
       (+ (vz vec) (vz with))))

(defun scale-vec (vec scale)
  "Scales a vector."
  (vec (* (vx vec) scale)
       (* (vy vec) scale)
       (* (vz vec) scale)))

(defun size-vec (vec)
  "Size of a vector."
  (+ (vx vec) (vy vec) (vz vec)))

;; Queues
(defun make-queue ()
  "Creates a queue."
  (cons nil nil))

(defun queue-push (value queue)
  "Pushes a value into a queue."
  (let ((new-item (cons value nil)))
    (setf (cdr queue)
          (if (car queue)
              (setf (cddr queue) new-item)
              (setf (car queue) new-item))))
  queue)

(defun queue-pop (queue)
  "Pops a value from a queue."
  (pop (car queue)))

(defun queue-front (queue)
  "Returns the value at the head of the queue without popping it."
  (caar queue))

(defun (setf queue-front) (queue value)
  "Replaces the value in the front of the queue with a new one."
  (setf (caar queue) value))

(defun queue-back (queue)
  "Returns the value at the tail of the queue."
  (cadr queue))

(defun (setf queue-back) (queue value)
  "Replaces the value in the back of the queue with a new one."
  (setf (cadr queue) value))

(defun queue-nth (index queue)
  "Gets the nth item in the queue."
  (nth index (car queue)))

(defun (setf queue-nth) (index queue value)
  "Sets the nth item in the queue with value."
  (setf (nth index (car queue)) value))

(defun queue-length (queue)
  "Returns the size of the queue."
  (length (car queue)))

(defun queue-empty-p (queue)
  "Checks if the queue is empty of content."
  (null (car queue)))

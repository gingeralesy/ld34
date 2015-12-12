(in-package #:ld34)
(in-readtable :qtools)

(defvar *main-window* NIL)
(defparameter *fps* 1000/30)
(defparameter *title* "LD34 - Eggcellent")

;; Widgets

(define-widget main (QWidget)
  ((paused :initform NIL :accessor paused-p)
   (victory :initform NIL :accessor victory-p)
   (level :initform (make-instance 'level) :finalized T)
   (player :initform (make-instance 'player))
   (keys :initform (make-hash-table :test 'eql) :accessor keys)))

(defmethod running ((main main))
  (running (slot-value main 'level)))

;; Sub-widgets, additional slots, initializer, and finalizer

(define-subwidget (main timer) (q+:make-qtimer main)
  (setf (q+:single-shot timer) T)
  (q+:start timer (round *fps*)))

(define-subwidget (main background) (asset 'image 'bg))

(define-initializer (main setup)
  (setf *main-window* main)
  (q+:resize main 1024 768)
  (setf (q+:window-title main) *title*)
  (enter player level))

(define-finalizer (main teardown)
  (loop for v being the hash-values of *asset-cache* do (finalize v))
  (setf *asset-cache* (make-hash-table :test 'equalp))
  (setf *main-window* NIL))

;; Slot getters
;; TODO: Support for multiple levels?

(defun level (&optional (win *main-window*))
  "Gets the level object in game."
  (slot-value win 'level))

(defun player (&optional (win *main-window*))
  "Gets the player object in game."
  (slot-value win 'player))

;; Key press events

(defun press-key (key &optional (win *main-window*))
  "Sets key as being pressed."
  (setf (gethash key (keys win)) key))

(defun release-key (key &optional (win *main-window*))
  "Unsets key from being pressed."
  (remhash key (keys win)))

(defun key-pressed-p (key &optional (win *main-window*))
  "Checks if a key is pressed or not."
  (gethash key (keys main)))

(define-override (main key-release-event) (ev)
  (release-key (q+:key ev) main)
  (stop-overriding))

(define-override (main key-press-event) (ev)
  (let (key (q+:key ev))
    (press-key key main)
    (case key
      (#.(q+:qt.key_escape)
       (if (running main)
           (stop main)
           (start main)))))
  (stop-overriding))

;; Paint event

(define-override (main paint-event) (ev)
  (with-simple-restart (abort "Abort the drawing and continue.")
    (with-finalizing ((painter (q+:make-qpainter main))
                      (bgbrush (q+:make-qbrush background)))
      (setf (q+:render-hint painter) (q+:qpainter.antialiasing)
            (q+.render-hint painter) (q+:qpainter.text-antialiasing)
            (q+.render-hint painter) (q+:qpainter.smooth-pixmap-transform)
            (q+.render-hint painter) (q+:qpainter.high-quality-antialiasing)
            (q+:style (q+:background painter)) (q+:qt.solid-pattern)
            (q+:color (q+:background painter)) (q+:qt.black)
            (q+:style (q+:brush painter)) (q+:qt.solid-pattern)
            ;; Background
            (q+:transform bgbrush) (q+:translate (q+:transform bgbrush)
                                                 (* (x (location player)) -2)
                                                 (* (y (location player)) -2)))
      (q+:fill-rect painter (q+:rect main) bgbrush)
      ;; Translate view
      (let ((view (translate (vec (/ (q+:width main) 2)
                                  (/ (q+:height main) 2)
                                  0)
                             (scaled (location player) -1))))
        (with-translation (view painter)
          (paint level painter)))
      ;; Overlay
      (unless (running main)
        (with-finalizing ((overlay (q+:make-qcolor 0 0 0 180))
                          (white (q+:make-qcolor 255 255 255)))
          (q+:fill-rect painter (q+:rect main) overlay)
          (let ((font (q+:font painter))
                (player (unit :player level)))
            (setf (q+:color (q+:pen painter)) white
                  (q+:font painter) font)
            (q+:draw-text painter
                          (q+:rect main)
                          (q+:qt.align-center)
                          (cond ((not player)
                                 (setf (q+:point-size font) 72)
                                 "Game Over")
                                ((victory-p main)
                                 (setf (q+:point-size font) 72)
                                 "Victory!")
                                ((paused-p main)
                                 (setf (q+:point-size font) 32)
                                 (format NIL "Paused.~%Press ESC to resume."))
                                (T ;; return T for paint even success
                                 T)))))))))

;; Methods
(defmethod call-with-translation (func target vec)
  (q+:save target)
  (q+:translate target (x vec) (y vec))
  (unwind-protect
   (funcall func)
   (q+:restore target)))

(defmethod paint :around ((paintable paintable) target)
  (let ((opacity (q+:opacity target)))
    (setf (q+:opacity target) (* opacity (visibility paintable)))
    (unwind-protect
         (call-next-method)
      (setf (q+:opacity target) opacity))))

;; Exported functions

(defun main (&key (blocking NIL))
  "Launches the game in debug mode."
  (unless *main-window*
    (setf v:*global-controller* (v:make-standard-global-controller))
    (with-main-window (window 'main :blocking blocking :name *title*))))

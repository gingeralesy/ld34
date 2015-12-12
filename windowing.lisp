(in-package #:ld34)
(in-readtable :qtools)

(defvar *main-window* NIL)
(defparameter *fps* 1000/30)
(defparameter *title* "Eggcellent")

(defun main (&key (blocking NIL))
  (unless *main-window*
    (setf v:*global-controller* (v:make-standard-global-controller))
    (with-main-window (window 'main :blocking blocking :name *title*))))

(in-package #:ld34)
(in-readtable :qtools)

(defvar *standalone* NIL
  "Whether this build is stand-alone or ASDF dependant.")
(defvar *asset-cache* (make-hash-table :test 'equalp)
  "Cache assets here after load.")

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


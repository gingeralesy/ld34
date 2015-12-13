(in-package #:cl-user)
#| 
 | Because Qtools has its own debugging system that can use Verbose if it has
 | been loaded, we make sure here that Verbose is loaded before Qtools.
 |#
(eval-when (:load-toplevel :compile-toplevel :execute)
  (push :verbose-no-init *features*)
  #+quicklisp (ql:quickload :verbose)
  #-quicklisp (asdf:load-system :verbose))

(asdf:defsystem ld34
  :author "Janne Pakarinen <gingeralesy@gmail.com>"
  :homepage "https://github.com/gingeralesy/ld34"
  :version "0.0.0"
  :components ((:file "package")
               (:file "utils")
               (:file "entity")
               (:file "animation")
               (:file "level")
               (:file "player")
               (:file "windowing"))
  :defsystem-depends-on (:qtools)
  :depends-on (:qtools
               :qtcore
               :qtgui
               :3d-vectors
               :trivial-indent))

;;-*- Mode: Lisp -*- 
;;; Author Benjamin E. Lambert
;;; ben@benjaminlambert.com

(asdf:defsystem "gnuplot"
  :description "Plotting gnuplot graphs from Lisp"
  :version "0.1"
  :author "Benjamin E. Lambert"
  :licence "All rights reserved"
  :serial t
  :components
  ((:module 
    "package-init"
    :pathname #P "src/"
    :components
    ((:file "package")))
   (:module
    "main"
    :pathname #P "src/"
    :serial t
    :components
    ((:file "gnuplot")
     )))
  ;;:depends-on (:cl-ppcre :lispdoc :gzip-stream :parse-number :scone :alexandria :cl-fad :split-sequence :cl-utilities :array-operations))
  :depends-on (:blambert-util))


;;-*- Mode: Lisp -*- 

(asdf:defsystem "gnuplot"
  :description "Plotting gnuplot graphs from Lisp"
  :version "0.1"
  :author "Ben Lambert"
  :licence "Apache-2.0"
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
    ((:file "util")
     (:file "gnuplot"))))
  :depends-on ())

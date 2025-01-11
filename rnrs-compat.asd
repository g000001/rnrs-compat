;;;; rnrs-compat.asd

(cl:in-package :asdf)

(defsystem :rnrs-compat
  :serial t
  :depends-on (:mbe :quasiquote1 :cl-unicode)
  :components ((:file "package")
               ;;(:file "package-after")
               (:file "ext")
               (:file "base")
               (:file "rnrs-compat")
               (:file "readtable")))

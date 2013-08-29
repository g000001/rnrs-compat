;;;; rnrs-compat.asd

(cl:in-package :asdf)

(defsystem :rnrs-compat
  :serial t
  :depends-on (:mbe :quasiquote1)
  :components ((:file "package")
               (:file "package-after")
               (:file "base")
               (:file "ext")
               (:file "rnrs-compat")
               (:file "readtable")))

(defmethod perform ((o test-op) (c (eql (find-system :rnrs-compat))))
  (load-system :rnrs-compat)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :rnrs-compat-internal :rnrs-compat))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

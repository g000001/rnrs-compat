(cl:in-package :rnrs-compat-internal)

(defmacro define-function (name-args &body body)
  (if (cl:consp name-args)
      (cl:destructuring-bind (name . args)
                             name-args
        `(defun ,name ,(to-proper-lambda-list args)
           ,@body))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (fdefinition ',name-args)
                 (function cl:values) ))
         (setf (fdefinition ',name-args)
               ,(car body) ))))


(defmacro with-local-define-function (&body defines-body)
  (or (cl:member :in defines-body) (error "no body"))
  (let* ((body-pos (cl:position :in defines-body))
         (defines  (cl:subseq defines-body 0 body-pos))
         (body     (cl:subseq defines-body (cl:1+ body-pos))) )
    (cl:loop
       :for (nil name-arg . bo) :in defines
       :collect (cl:let ((name-arg (to-proper-lambda-list name-arg)))
                  `(,(car name-arg) ,(cdr name-arg) ,@bo) )
       :into defs
       :finally (cl:return
                  `(labels (,@defs)
                     ,@body )))))


(defmacro with-local-define-variable (&body defines-body)
  (or (cl:member :in defines-body) (error "no body"))
  (let* ((body-pos (cl:position :in defines-body))
         (defines  (cl:subseq defines-body 0 body-pos))
         (body     (cl:subseq defines-body (cl:1+ body-pos))) )
    (cl:loop
       :for (nil v bo) :in defines
       :collect v :into vars
       :collect v :into setqs
       :collect bo :into setqs
       :finally (cl:return
                  `(cl:let (,@vars)
                     (cl:psetq ,@setqs)
                     ,@body )))))

;;; eof

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
  (cl:let* ((body-pos (cl:position :in defines-body))
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


(deftype definition-operator ()
  `(cl:member define internal-define))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun process-scheme-body* (exps &optional name)
    (labels ((defs+exps (body exps defs decls)
                 (if (endp body)
                     (values (reverse exps) (reverse defs) (reverse decls))
                     (typecase (car body)
                       ((cl:cons definition-operator cl:*)
                        (defs+exps (cdr body) exps (cons (car body) defs) decls))
                       ((cl:cons (eql cl:declare) cl:*)
                        (defs+exps (cdr body) exps defs (cons (car body) decls)))
                       (otherwise 
                        (exps (cdr body) (cons (car body) exps) defs decls)))))
             (exps (body exps defs decls)
               (if (endp body)
                   (values (reverse exps) (reverse defs) (reverse decls))
                   (typecase (car body)
                     ((cl:cons definition-operator cl:*)
                      (cl:error "~:[~;~:*~A: ~]no expression after a sequence of internal definitions" name))
                     (otherwise 
                      (exps (cdr body) (cons (car body) exps) defs decls))))))
      (defs+exps exps '() '() '() )))

  
  (defun expand-define (expr)
    (etypecase expr
      ((cl:cons definition-operator (cl:cons cl:cons))
       (destructuring-bind ((name &rest args) &rest body)
                           (cdr expr)
         `(,name (lambda ,args ,@body))))
      ((cl:cons definition-operator (cl:cons cl:symbol cl:*))
       (cdr expr))))

  
  (defun @expand-internal-define (body)
    (multiple-value-bind (body defs decls)
                         (process-scheme-body* body)
      (typecase defs
        (null `(,@decls 
                (begin ,@body)))
        (otherwise `(,@decls
                     (letrec ,(mapcar #'expand-define defs)
                       ,@body))))))
  
  
  (defun null-lexenv-p (env)
    #+sbcl (sb-c::null-lexenv-p env)))


;;; eof

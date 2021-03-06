;;;; readtable.lisp

(cl:in-package :rnrs-compat-internal)
(named-readtables:in-readtable :common-lisp)

(defun read-list (stream ignore)
  #+sbcl (sb-impl::read-list stream ignore)
  #+lispworks (system::read-list stream ignore)
  #+allegro (excl::read-list stream ignore )
  #+ccl (ccl::read-list stream)
  #+abcl (sys::read-list stream ignore))

(cl:defun read-list* (stream ignore)
  (cl:case (cl:peek-char t stream)
    ((#\_) 
     (cl:read-char stream)
     (cl:case (cl:peek-char nil stream)
       ((#\Space #\Tab #\Newline #\Return)
        (cl:read-char stream)
        (cons 'rnrs::_ (read-list stream ignore)))
       (cl:otherwise `(cl:funcall ,@(read-list stream ignore)))))
    (otherwise
     (read-list stream ignore) )))


(named-readtables:defreadtable :rnrs
  (:merge :quasiquote)
  (:macro-char #\( #'read-list*)
  (:dispatch-macro-char #\# #\t (constantly cl:t))
  (:dispatch-macro-char #\# #\f (constantly cl:nil))
  (:case :upcase))

;;; eof

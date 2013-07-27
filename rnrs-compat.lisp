;;;; rnrs-compat.lisp

(cl:in-package :rnrs-compat-internal)

(def-suite rnrs-compat)

(in-suite rnrs-compat)

;;; define
(defun CHECK-ARG-to-DECLARE (expr)
  (destructuring-bind (ignore pred var name) expr
    (declare (ignore ignore name))
    `(declare ((satisfies ,pred) ,var))))

(defun restify (expr)
  (etypecase expr
    (cl:symbol (list 'cl:&rest expr))
    (cl:list (if (tailp () expr)
              expr
              (let ((last (last expr)))
                (append (butlast expr)
                        (list (car last)
                              'cl:&rest
                              (cdr last))))))))


;; `(eval-when (:compile-toplevel :load-toplevel :execute)
;;                 (defun ,name (,@(restify args))
;;                   ,(CHECK-ARG-to-DECLARE decl)
;;                   ,@(@expand-internal-define body)))

(defmacro global-define (name&args &body body)
  (etypecase name&args
    ((cl:cons cl:t cl:null)
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (defun ,(car name&args) ()
          ,@(@expand-internal-define body))))
    (cl:cons
     (destructuring-bind (name &rest args)
                         name&args
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          (defun ,name (,@(restify args))
            ,@(@expand-internal-define body)))))
    (cl:symbol 
     `(progn
        (setf (symbol-function ',name&args) 
              ,@body)))))


#|(defmacro define (name&args &body body &environment env)
  (if (null-lexenv-p env)
      (etypecase name&args
        ((cl:cons cl:t cl:null)
         `(eval-when (:compile-toplevel :load-toplevel :execute)
            (defun ,(car name&args) ()
              (@with-internal-define
                ,@body))))
        (cl:cons
         (destructuring-bind (name &rest args)
                             name&args
           (destructuring-bind (decl &rest body) body
             (if (cl:string= 'check-arg (car decl))
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                    (defun ,name (,@(restify args))
                      ,(CHECK-ARG-to-DECLARE decl)
                      (@with-internal-define 
                        ,@body))
                    )
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                    (defun ,name (,@(restify args))
                      ,decl
                      (@with-internal-define
                        ,@body))
                    )))))
        (cl:symbol 
         `(@with-internal-define
            (setf (symbol-function ',name&args) (progn ,@body))
            )))
      `(internal-define ,name&args ,@body)))|#


(defmacro define (name&args &body body &environment env)
  (if (null-lexenv-p env)
      `(global-define ,name&args ,@body)
      `(internal-define ,name&args ,@body)))


#|(format t "~{;; ~A~%(defun ~:*~A ()~% )~3%~}"
        (sort (copy-list  (set-difference
                            *r5rs-symbols*
                            *intersection-of-cl-r5rs-symbols*)) #'string<)
        #|(subseq
         0 10)|#)|#
;; ...

;; ANGLE
(defsynonymfun ANGLE cl:phase)


;; ASSQ
(defun ASSQ (item alist)
  (cl:loop
     :for e :on alist
     :when (and (cl:consp (car e))
                (eq? item (caar e)))
       :return (car e)))

;; ASSV
(defun ASSV (item alist)
  (cl:loop
     :for e :on alist
     :when (and (cl:consp (car e))
                (eqv? item (caar e)))
       :return (car e)))

;; BEGIN
(defmacro BEGIN (&body body)
  `(progn ,@body))


;; BOOLEAN?
(defun BOOLEAN? (obj)
  (cl:typep obj '(cl:member cl:t cl:nil)))


;; CALL-WITH-CURRENT-CONTINUATION
(defun CALL-WITH-CURRENT-CONTINUATION ()
 )


;; CALL-WITH-INPUT-FILE
(defun CALL-WITH-INPUT-FILE (filename proc
                                      &key
                                      if-does-not-exist
                                      (element-type 'base-char)
                                      (encoding :default))
  (with-open-stream (in (cl:open filename
                                 :direction :input
                                 :if-does-not-exist if-does-not-exist
                                 :element-type element-type
                                 :external-format encoding))
    (funcall proc in)))


;; CALL-WITH-OUTPUT-FILE
(defun CALL-WITH-OUTPUT-FILE (filename thunk
                                       &key
                                       if-does-not-exist
                                       if-exists
                                       (element-type 'base-char)
                                       (encoding :default))
  (with-open-stream (out (cl:open filename
                                  :direction :input
                                  :if-exists if-exists
                                  :if-does-not-exist if-does-not-exist
                                  :element-type element-type
                                  :external-format encoding))
    (funcall thunk out)))


;; CALL-WITH-VALUES
(defun CALL-WITH-VALUES (producer consumer)
  (multiple-value-call consumer (funcall producer)))

;; CHAR->INTEGER
(defsynonymfun CHAR->INTEGER cl:char-code)


;; CHAR-ALPHABETIC?
(defun CHAR-ALPHABETIC? (char)
  (and (not (digit-char-p char))
       (alphanumericp char)))


;; CHAR-CI<=?
(defsynonymfun CHAR-CI<=? cl:char-not-greaterp)


;; CHAR-CI<?
(defsynonymfun CHAR-CI<? cl:char-lessp)


;; CHAR-CI=?
(defsynonymfun CHAR-CI=? cl:char-equal)


;; CHAR-CI>=?
(defsynonymfun CHAR-CI>=? cl:char-not-lessp)


;; CHAR-CI>?
(defsynonymfun CHAR-CI>? cl:char-greaterp)


;; CHAR-LOWER-CASE?
(defsynonymfun CHAR-LOWER-CASE? cl:lower-case-p)


;; CHAR-NUMERIC?
(defun CHAR-NUMERIC? (char)
  (and (digit-char-p char) t))


;; CHAR-READY?
(defun CHAR-READY? ()
 )


;; CHAR-UPPER-CASE?
(defsynonymfun CHAR-UPPER-CASE? cl:upper-case-p)


;; CHAR-WHITESPACE?
(defun CHAR-WHITESPACE? (char)
  (declare (cl:character char))
  (typep char '(cl:member #\Space #\Newline #\Tab #\Return #\Page)))


;; CHAR<=?
(defsynonymfun CHAR<=? cl:char<=)


;; CHAR<?
(defsynonymfun CHAR<? cl:char<)


;; CHAR=?
(defsynonymfun CHAR=? cl:char=)


;; CHAR>=?
(defsynonymfun CHAR>=? cl:char>=)


;; CHAR>?
(defsynonymfun CHAR? cl:characterp)


;; CHAR?
(defsynonymfun CHAR? cl:characterp)


;; CLOSE-INPUT-PORT
(defsynonymfun CLOSE-INPUT-PORT cl:close)


;; CLOSE-OUTPUT-PORT
(defsynonymfun CLOSE-OUTPUT-PORT cl:close)


;; COMPLEX?
(defsynonymfun COMPLEX? cl:numberp)


;; CURRENT-INPUT-PORT FIXME
(defun CURRENT-INPUT-PORT ()
  *standard-input*)


;; CURRENT-OUTPUT-PORT FIXME
(defun CURRENT-OUTPUT-PORT ()
  *standard-output*)


;; DEFINE


;; DEFINE-SYNTAX


;; DELAY
(defun DELAY ()
 )


;; DISPLAY
(defun-inline display (obj &optional (port (current-output-port)))
  (princ obj port))


;; DYNAMIC-WIND
(defun DYNAMIC-WIND (in body out)
  (declare (function in body out))
  (funcall in)
  (unwind-protect (funcall body)
    (funcall out) ))


;; ELSE


;; EOF-OBJECT?
(defun EOF-OBJECT? (obj)
  (eq obj (eof-object)) )


;; EVEN?
(defsynonymfun EVEN? cl:evenp)


;; EXACT->INEXACT
(defun EXACT->INEXACT (n)
  (float n 0d0) )


;; EXACT?
(defsynonymfun EXACT? cl:rationalp)


;; FOR-EACH
(defun FOR-EACH (fn cl:&rest lists)
  (cl:apply #'cl:mapc fn lists)
  nil )


;; FORCE
(defun FORCE ()
 )


;; IMAG-PART
(defsynonymfun IMAG-PART cl:imagpart)


;; INEXACT->EXACT
(defun INEXACT->EXACT (n)
  (rationalize n))


;; INEXACT?
(defun INEXACT? (n)
  (floatp n))


;; INPUT-PORT?
(defun INPUT-PORT? (port)
  (and (streamp port)
       (input-stream-p port)))


;; INTEGER->CHAR
(defsynonymfun INTEGER->CHAR cl:code-char)


;; INTEGER?
(defun integer? (n)
  (and (numberp n)
       (or (zerop n)
           (integerp n)
           (and (not (= (* 2 n) n))
                (= (truncate n) n)) )))


;; INTERACTION-ENVIRONMENT
(defun INTERACTION-ENVIRONMENT ()
  nil)


;; LET-SYNTAX


;; LETREC
(defmacro letrec ((&rest binds) &body body)
  `(cl:let (,@(mapcar (cl:lambda (x)
                     `(,(car x) #'cl:values) )
             binds ))
     (cl:declare (cl:optimize (cl:debug 1) (cl:space 3)))
     (labels (,@(remove nil
                  (mapcar (cl:lambda (x &aux (name (car x)))
                            `(,name
                              (&rest args)
                              (declare (dynamic-extent args))
                              (the (cl:values &rest t) (cl:apply ,name args)) ))
                          binds )))
       (psetq ,@(cl:apply #'append binds))
       ,@body )))


;; LETREC-SYNTAX


;; LIST->STRING
(defun LIST->STRING (list)
  (coerce list 'cl:string))


;; LIST->VECTOR
(defun-inline list->vector (obj)
  (declare (cl:list obj))
  (coerce obj 'cl:vector))


;; LIST-REF
(defun LIST-REF (list k)
  (cl:nth k list))


;; LIST-TAIL
(defun LIST-TAIL (list k)
  (cl:nthcdr k list))


;; LIST?
(defun LIST? (obj)
  (and (cl:listp obj)
       (tailp () obj)))


;; MAGNITUDE
(defsynonymfun magnitude cl:abs)


;; MAKE-POLAR
(defun MAKE-POLAR (r th)
  (* r (cis th)))


;; MAKE-RECTANGULAR
(defsynonymfun MAKE-RECTANGULAR cl:complex)


;; MAKE-VECTOR
(defun MAKE-VECTOR (size &optional (init 0))
  (cl:make-array size                   ;***
                 :initial-element init
                 :adjustable nil
                 :fill-pointer nil))


;; MEMQ
(defun-inline MEMQ (x list)
  (cl:member x list :test #'eq?))


;; MEMV
(defun-inline MEMV (x list)
  (cl:member x list :test #'eqv?))


;; MODULO
(defsynonymfun MODULO cl:mod)

;;; NEGATIVE?
(defsynonymfun NEGATIVE? cl:minusp)

;; NEWLINE
(defun-inline NEWLINE (&optional port)
  (terpri port))


;; NULL-ENVIRONMENT
(defun NULL-ENVIRONMENT ()
  nil)


;; NULL?
(defun-inline NULL? (obj) (null obj))


;; NUMBER->STRING
(defun NUMBER->STRING (number &optional (radix 10) use-upper?)
  (funcall (if use-upper? #'cl:string-upcase #'string-downcase)
           (write-to-string number :base radix)))


;; NUMBER?
(defsynonymfun NUMBER? cl:numberp)


;; ODD?
(defsynonymfun odd? cl:oddp)


;; OPEN-INPUT-FILE
;;  filename :key if-does-not-exist element-type encoding
(defun OPEN-INPUT-FILE (filename &key
                                 if-does-not-exist
                                 (element-type 'character)
                                 (encoding :default))
  (cl:open filename
           :direction :input
           :if-does-not-exist if-does-not-exist
           :element-type element-type
           :external-format encoding))


;; OPEN-OUTPUT-FILE
;; filename :key if-does-not-exist if-exists element-type encoding
(defun OPEN-OUTPUT-FILE (filename &key
                                  (if-does-not-exist :create)
                                  (if-exists :error)
                                  (element-type 'base-char)
                                  (encoding :default))
  (cl:open filename
           :direction :output
           :if-exists if-exists
           :if-does-not-exist if-does-not-exist
           :element-type element-type
           :external-format encoding))


;; OUTPUT-PORT?
(defun OUTPUT-PORT? (port)
  (and (streamp port)
       (output-stream-p port)))


;; PAIR?
(defun-inline PAIR? (obj) (consp obj))


;; PORT?
(defsynonymfun PORT? cl:streamp)


;; POSITIVE?
(defsynonymfun POSITIVE? cl:plusp)


;; PROCEDURE?
(defsynonymfun PROCEDURE? cl:functionp)


;; QUASIQUOTE
(defun QUASIQUOTE ()
  )


;; QUOTIENT
(defun QUOTIENT (x y)
  (cl:values (cl:truncate x y)))


;; RATIONAL?
(defsynonymfun RATIONAL? cl:realp)


;; REAL-PART
(defsynonymfun REAL-PART cl:realpart)


;; REAL?
(defun REAL? (n)
  (and (numberp n)
       (or (realp n)
           (zerop (cl:imagpart n)))))


;; REMAINDER
(defsynonymfun REMAINDER cl:rem)


;; SCHEME-REPORT-ENVIRONMENT
(defun SCHEME-REPORT-ENVIRONMENT ()
  nil)


;; SET!
(defmacro set! (var val)
  `(setq ,var ,val))


;; SET-CAR!
(defun SET-CAR! (list obj)
  (cl:rplaca list obj))


;; SET-CDR!
(defun SET-CDR! (cons x)
  (cl:rplacd cons x))


;; STRING->LIST
(defun STRING->LIST (string)
  (coerce string 'cl:list))


;; STRING->NUMBER FIXME
(defun STRING->NUMBER (string &optional (radix 10))
  (let ((*read-base* radix)
        (*read-eval* nil)
        (*read-default-float-format* 'single-float))
    (let ((number? (with-input-from-string (in string)
                     (cl:read in nil nil))))
      (and (numberp number?)
           number?))))


;; STRING->SYMBOL FIXME
(defsynonymfun STRING->SYMBOL cl:intern)


;; STRING-APPEND
(defun STRING-APPEND (&rest strings)
  (cl:format nil "~{~A~}" strings))


;; STRING-CI<=?
(defsynonymfun STRING-CI<=? cl:string-not-greaterp)


;; STRING-CI<?
(defsynonymfun STRING-CI<? cl:string-lessp)


;; STRING-CI=?
(defsynonymfun STRING-CI=? cl:string-equal)


;; STRING-CI>=?
(defsynonymfun STRING-CI>=? cl:string-not-lessp)


;; STRING-CI>?
(defsynonymfun STRING-CI>? cl:string-greaterp)


;; STRING-COPY
(defsynonymfun STRING-COPY cl:copy-seq)


;; STRING-FILL!
(defsynonymfun STRING-FILL! cl:fill)


;; STRING-LENGTH
(defun-inline STRING-LENGTH (str)
  (declare (simple-string  str))
  (length str))


;; STRING-REF
(defun-inline STRING-REF (string k)
  (declare (simple-string string))
  (char string k))


;; STRING-SET!
(defun-inline string-set! (string k char)
  (declare (simple-string  string))
  (setf (char string k) char))


;; STRING<=?
(defsynonymfun STRING<=? cl:string<=)


;; STRING<?
(defsynonymfun STRING<? cl:string<)


;; STRING=?
(defsynonymfun STRING=? cl:string=)


;; STRING>=?
(defsynonymfun STRING>=? cl:string>=)


;; STRING>?
(defsynonymfun STRING>? cl:string>)


;; STRING?
(defsynonymfun STRING? cl:stringp)


;; SUBSTRING
(defun-inline SUBSTRING (str start end)
  (declare (simple-string  str))
  (subseq str start end))


;; SYMBOL->STRING
(defsynonymfun SYMBOL->STRING cl:string)


;; SYMBOL?
(defsynonymfun SYMBOL? cl:symbolp)


;; SYNTAX-RULES


;; VECTOR->LIST
(defun-inline vector->list (obj)
  (declare (cl:vector obj))
  (cl:coerce obj 'cl:list))


;; VECTOR-FILL!
(defsynonymfun VECTOR-FILL! cl:fill)


;; VECTOR-LENGTH
(defun-inline VECTOR-LENGTH (vec)
  (declare (cl:vector vec))
  (length vec))


;; VECTOR-REF
(defun-inline vector-ref (vec index)
  (declare (cl:vector vec)
           (fixnum index))
  (aref vec index))


;; VECTOR-SET!
(defun-inline VECTOR-SET! (vec index value)
  (declare (cl:vector vec) (fixnum index))
  (setf (aref vec index) value))


;; VECTOR?
(defun-inline VECTOR? (obj)
  (typep obj '(cl:and cl:vector (cl:not cl:string))))

;; WITH-INPUT-FROM-FILE
;; filename thunk :key if-does-not-exist element-type encoding
(defun WITH-INPUT-FROM-FILE (filename thunk
                                      &key
                                      if-does-not-exist
                                      (element-type 'base-char)
                                      (encoding :default))
  (with-open-stream (*standard-input* (cl:open filename
                                               :direction :input
                                               :if-does-not-exist if-does-not-exist
                                               :element-type element-type
                                               :external-format encoding))
    (funcall thunk)))


;; WITH-OUTPUT-TO-FILE
(defun WITH-OUTPUT-TO-FILE (filename thunk
                                     &key
                                     if-does-not-exist
                                     if-exists
                                     (element-type 'base-char)
                                     (encoding :default))
  (with-open-stream (*standard-output* (cl:open filename
                                                :direction :input
                                                :if-exists if-exists
                                                :if-does-not-exist if-does-not-exist
                                                :element-type element-type
                                                :external-format encoding))
    (funcall thunk)))



;; ZERO?
(defun-inline ZERO? (x) (zerop x))

;; (defsynonymfun any cl:some)
;; (defsynonymfun keyword? #'keywordp)

;;; eof


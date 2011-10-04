;;;; rnrs-compat.lisp

(cl:in-package :rnrs-compat-internal)

(def-suite rnrs-compat)

(in-suite rnrs-compat)

;;; "rnrs-compat" goes here. Hacks and glory await!

(defmacro defun-inline (name (&rest args) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,@args)
       ,@body)))


;;; define
(defun CHECK-ARG-to-DECLARE (expr)
  (destructuring-bind (ignore pred var name) expr
    (declare (ignore ignore name))
    `(declare ((satisfies ,pred) ,var))))

(defun restify (expr)
  (etypecase expr
    (symbol (list 'cl:&rest expr))
    (list (if (tailp () expr)
              expr
              (let ((last (last expr)))
                (append (butlast expr)
                        (list (car last)
                              'cl:&rest
                              (cdr last))))))))

(defmacro define (name&args &body body)
  (etypecase name&args
    (list (destructuring-bind (name &rest args)
                              name&args
            (destructuring-bind (decl &rest body) body
              (if (cl:string= 'check-arg (car decl))
                  `(eval-when (:compile-toplevel :load-toplevel :execute)
                     (defun ,name (,@(restify args))
                       ,(CHECK-ARG-to-DECLARE decl)
                       ,@body)
                     )
                  `(eval-when (:compile-toplevel :load-toplevel :execute)
                     (defun ,name (,@(restify args))
                       ,decl
                       ,@body)
                     )))))
    (symbol `(progn
               (setf (symbol-function ',name&args) (progn ,@body))
               ))))

(defun to-proper-lambda-list (list)
  (typecase list
    (list (if (tailp () list)
              list
              (cl:let ((last (last list)))
                `(,@(butlast list)
                      ,(car last)
                    cl:&rest
                    ,(cdr last)))))
    (symbol `(cl:&rest ,list))))

(defmacro lambda (args &rest body)
  `(cl:lambda ,(to-proper-lambda-list args)
     ,@body))

;;; B
(defmacro begin (&body body)
  `(progn ,@body))

;;; C
(define char=? #'cl:char=)
(define char<? #'cl:char<)
(define char-ci<? #'cl:char-lessp)
(define char-ci=? #'cl:char-equal)
(define char? #'cl:characterp)

;;; D
(defun-inline display (obj)
  (princ obj))

;;; E
(defun-inline eq? (x y) (eq x y))

(defun-inline equal? (x y)
  (equal x y))

(define exact? #'cl:rationalp)

;;; I
(define integer? #'cl:integerp)

;;; M
(defun-inline map (function list &rest more-list)
  (apply #'mapcar function list more-list))

(defun-inline memq (x list)
  (cl:member x list :test #'eq))

(defun make-string (len)
  (cl:make-string len))

;;; N
(defun-inline null? (obj) (null obj))

(defun-inline newline ()
  (terpri))

;;; P
(defun-inline pair? (obj) (consp obj))

(define procedure? #'cl:functionp)

;;; S
(defmacro set! (var val)
  `(setq ,var ,val))

(defun-inline set-car! (list obj)
  (rplaca list obj))

(defun-inline set-cdr! (cons x)
  (rplacd cons x))

(define any #'cl:some)

(define string? #'cl:stringp)

(defun string-length (str)
  (declare (optimize (safety 3) (speed 3)))
  (declare (simple-string  str))
  (length str))

(defun string-set! (string k char)
  (declare (optimize (safety 3) (speed 3)))
  (declare (simple-string  string))
  (setf (char string k) char))

(defun string-ref (string k)
  (declare (optimize (safety 3) (speed 3)))
  (declare (simple-string string))
  (char string k))

(defun substring (str start end)
  (declare (optimize (safety 3) (speed 3)))
  (declare (simple-string  str))
  (subseq str start end))

;;; V
(defun vector-length (vec)
  (declare (optimize (safety 3) (speed 3)))
  (declare (vector vec))
  (length vec))

(defun vector-ref (vec index)
  (declare (optimize (safety 3) (speed 3)))
  (declare (vector vec)
           (fixnum index))
  (aref vec index))

(defun vector-set! (vec index value)
  (declare (optimize (safety 3) (speed 3)))
  (declare (vector vec)
           (fixnum index))
  (setf (aref vec index) value))

;;; Z
(defun-inline zero? (x) (zerop x))

;;; eof
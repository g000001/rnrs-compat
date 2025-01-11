(cl:in-package :rnrs-compat-internal)

(defvar *r5rs-symbols*
  '(:* :+ :- :|...| :/  :< :<= := :=> :> :>= :abs :acos
    :and :angle :append :apply :asin :assoc :assq :assv :atan :begin
    :boolean? :caar :cadr :call-with-current-continuation :call-with-input-file
    :call-with-output-file :call-with-values :car :case :cdddar :cddddr :cdr
    :ceiling :char->integer :char-alphabetic? :char-ci<=? :char-ci<? :char-ci=?
    :char-ci>=? :char-ci>? :char-downcase :char-lower-case? :char-numeric?
    :char-ready? :char-upcase :char-upper-case? :char-whitespace? :char<=? :char<?
    :char=? :char>=? :char>? :char? :close-input-port :close-output-port :complex?
    :cond :cons :cos :current-input-port :current-output-port :define
    :define-syntax :delay :denominator :display :do :dynamic-wind :else
    :eof-object? :eq? :equal? :eqv? :eval :even? :exact->inexact :exact? :exp
    :expt :floor :for-each :force :gcd :if :imag-part :inexact->exact
    :inexact? :input-port? :integer->char :integer? :interaction-environment
    :lambda :lcm :length :let :let* :let-syntax :letrec :letrec-syntax :list
    :list->string :list->vector :list-ref :list-tail :list? :load :log :magnitude
    :make-polar :make-rectangular :make-string :make-vector :map :max :member
    :memq :memv :min :modulo :negative? :newline :not :null-environment :null?
    :number->string :number? :numerator :odd? :open-input-file
    :open-output-file :or :output-port? :pair? :peek-char :port? :positive?
    :procedure? :quasiquote :quote :quotient :rational? :rationalize :read
    :read-char :real-part :real? :remainder :reverse :round
    :scheme-report-environment :set! :set-car! :set-cdr! :setcar :sin :sqrt
    :string :string->list :string->number :string->symbol :string-append
    :string-ci<=? :string-ci<? :string-ci=? :string-ci>=? :string-ci>?
    :string-copy :string-fill! :string-length :string-ref :string-set! :string<=?
    :string<? :string=? :string>=? :string>? :string? :substring :symbol->string
    :symbol? :syntax-rules :tan :transcript-off :transcript-on :truncate
    :values :vector :vector->list :vector-fill! :vector-length :vector-ref
    :vector-set! :vector? :with-input-from-file :with-output-to-file :write
    :write-char :zero?))

(defvar *intersection-of-cl-r5rs-symbols*
  '(:quote :map :atan :abs :eval :<= :gcd :max :floor :do :min :write-char :/
    :numerator :asin :lambda :list :denominator :if :values :assoc :- :sin
    :char-downcase :round :or :cdr :exp :ceiling :peek-char :cos :read-char :cons
    :log :cadr :string :> :expt :char-upcase :cdddar :>= :tan :write :vector :let
    :caar :let* := :apply :rationalize :cddddr :member :make-string :length :acos
    :* :< :not :car :+ :load :cond :append :sqrt :lcm :truncate :read :case :and
    :reverse))

(defmacro defsynonymfun (name fcn)
  `(progn
     #-lispworks (declaim (inline ,name))
     (cl:setf (cl:fdefinition ',name) (function ,fcn))))

(defmacro defsynonymclfun (name)
  `(progn
     #-lispworks (declaim (inline ,name))
     (cl:setf (cl:fdefinition ',name)
              (function ,(intern (cl:string name) :cl)))))

(defmacro defun-inline (name (&rest args) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,@args)
       ,@body)))

(cl:let ((eof (cl:list :eof)))
  (defun eof-object ()
    eof))


;; IF
(defmacro IF (pred then &optional *else)
  `(cl:if ,pred ,then ,*else))


;; EQ?
(defun-inline eq? (x y) (eq x y))


;; EQV?
(defun-inline eqv? (x y)
  (typecase x
    (cl:number (cl:and (numberp y) (cl:= x y)))
    (cl:character (cl:and (characterp y) (char= x y)))
    (t (eq? x y))))


;; EQUAL?
(defun-inline equal? (x y)
  (typecase x
    ((cl:and cl:vector (cl:not cl:string))
     (equalp x y))
    (T (equal x y))))

#|(format t "~{;; ~A~%(defsynonymclfun ~:*~A)~2%~}"
        (sort (copy-list  *intersection-of-cl-r5rs-symbols*) #'string<))|#

;; *
(defsynonymclfun *)

;; +
(defsynonymclfun +)

;; -
(defsynonymclfun -)

;; /
(defsynonymclfun /)

;; <
(defsynonymclfun <)

;; <=
(defsynonymclfun <=)

;; =
(defsynonymclfun =)

;; >
(defsynonymclfun >)

;; >=
(defsynonymclfun >=)

;; ABS
(defsynonymclfun ABS)

;; ACOS
(defsynonymclfun ACOS)

;; AND
(defmacro AND (&rest args)
  `(cl:and ,@args))

;; APPEND
(defsynonymclfun APPEND)

;; APPLY
(defsynonymclfun APPLY)

;; ASIN
(defsynonymclfun ASIN)

;; ASSOC
(defun ASSOC (item alist &optional (compare #'equal?))
  (cl:loop
     :for e :on alist
     :when (and (cl:consp (car e))
                (funcall compare item (caar e)))
       :return (car e)))

;; ATAN
(defsynonymclfun ATAN)


;; BEGIN
(defmacro begin (&body body)
  `(progn ,@body))


;; CAR
(defun-inline CAR (list)
  (cl:car (the cl:cons list)))


(PROGN
  ;; (DEFSYNONYMCLFUN CAR)
  ;; (DEFSYNONYMCLFUN CDR)
  (DEFSYNONYMCLFUN CAAR)
  (DEFSYNONYMCLFUN CADR)
  (DEFSYNONYMCLFUN CDAR)
  (DEFSYNONYMCLFUN CDDR)
  (DEFSYNONYMCLFUN CAAAR)
  (DEFSYNONYMCLFUN CAADR)
  (DEFSYNONYMCLFUN CADAR)
  (DEFSYNONYMCLFUN CADDR)
  (DEFSYNONYMCLFUN CDAAR)
  (DEFSYNONYMCLFUN CDADR)
  (DEFSYNONYMCLFUN CDDAR)
  (DEFSYNONYMCLFUN CDDDR)
  (DEFSYNONYMCLFUN CAAAAR)
  (DEFSYNONYMCLFUN CAAADR)
  (DEFSYNONYMCLFUN CAADAR)
  (DEFSYNONYMCLFUN CAADDR)
  (DEFSYNONYMCLFUN CADAAR)
  (DEFSYNONYMCLFUN CADADR)
  (DEFSYNONYMCLFUN CADDAR)
  (DEFSYNONYMCLFUN CADDDR)
  (DEFSYNONYMCLFUN CDAAAR)
  (DEFSYNONYMCLFUN CDAADR)
  (DEFSYNONYMCLFUN CDADAR)
  (DEFSYNONYMCLFUN CDADDR)
  (DEFSYNONYMCLFUN CDDAAR)
  (DEFSYNONYMCLFUN CDDADR)
  (DEFSYNONYMCLFUN CDDDAR)
  (DEFSYNONYMCLFUN CDDDDR))


;; CASE
(defmacro CASE (item &body clauses)
  `(cl:case ,item
     ,@clauses))

#|(define-syntax case
  (syntax-rules (else)
    ((case expr0
       ((key ***) res1 res2 ***)
       ***
       (else else-res1 else-res2 ***) )
     (mbe:with ((tmp (gensym)))
       (let ((tmp expr0))
         (cond
           ((memv tmp '(key ***)) res1 res2 ***)
           ***
           (else else-res1 else-res2 ***) ))))
    ((case expr0
       ((keya ***) res1a res2a ***)
       ((keyb ***) res1b res2b ***)
       *** )
     (mbe:with ((tmp (gensym)))
       (let ((tmp expr0))
         (cond
           ((memv tmp '(keya ***)) res1a res2a ***)
           ((memv tmp '(keyb ***)) res1b res2b ***)
           *** ))))))|#


;; CDDDAR
(defsynonymclfun CDDDAR)

;; CDDDDR
(defsynonymclfun CDDDDR)

;; CDR
(defun-inline CDR (list)
  (cl:cdr (the cl:cons list)))

;; CEILING
(defsynonymclfun CEILING)

;; CHAR-DOWNCASE
(defun-inline CHAR-DOWNCASE (ch)
  (cl-unicode:lowercase-mapping ch))

;; CHAR-UPCASE
(defun-inline CHAR-UPCASE (ch)
  (cl-unicode:uppercase-mapping ch))


;; COND
;; (defsynonymclfun COND)

(define-syntax cond
  (syntax-rules (:else =>)
    ((cond (:else result1 result2 ***))
     (progn result1 result2 ***))
    ((cond (test => result))
     (mbe:with ((temp (gensym)))
       (let ((temp test))
         (if temp (cl:funcall result temp)))))
    ((cond (test => result) clause1 clause2 ***)
     (mbe:with ((temp (gensym)))
       (let ((temp test))
         (if temp
             (funcall result temp)
             (cond clause1 clause2 ***)))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ***)
     (mbe:with ((temp (gensym)))
       (let ((temp test))
         (if temp
             temp
             (cond clause1 clause2 ***)))))
    ((cond (test result1 result2 ***))
     (if test (progn result1 result2 ***)))
    ((cond (test result1 result2 ***)
           clause1 clause2 ***)
     (if test
         (progn result1 result2 ***)
         (cond clause1 clause2 ***)))))

#|(defmacro cond (r c &rest clauses)
  (labels ((recur (clauses)
             (if (null? clauses)
                 (funcall r 'unspecific)
                 (process-cond-clause r c
                                      (car clauses)
                                      (recur (cdr clauses)) ))))
    (recur clauses)))|#


; Auxiliary also used by DO

#|(defun process-cond-clause (r c clause rest)
  (cl:cond ((null? (cdr clause))
            `(,(funcall r 'or-aux) ,(car clause)
               (,(funcall r 'lambda) () ,rest) ))
           ((funcall c (car clause) (funcall r 'else))
            `(,(funcall r 'begin) ,@(cdr clause)) )
           ((funcall c (cadr clause) (funcall r '=>))
            `(,(funcall r '=>-aux) ,(car clause)
               (,(funcall r 'lambda) () ,(caddr clause))
               (,(funcall r 'lambda) () ,rest) ))
           (:else
            `(,(funcall r 'if) ,(car clause)
               (,(funcall r 'begin) ,@(cdr clause))
               ,rest ))))|#

;; CONS
(defsynonymclfun CONS)

;; COS
(defsynonymclfun COS)

;; DENOMINATOR
(defsynonymclfun DENOMINATOR)

;; DO
(defmacro DO ((&rest varlist) endlist &body body)
  (cl:let* ((vars (cl:mapcar (cl:lambda (v)
                               (cl:if (cl:consp v) (car v) v) )
                             varlist ))
            (binds (cl:mapcar
                    (cl:lambda (b)
                      (cl:if (cl:consp b)
                             (cl:destructuring-bind (var &optional init next)
                                  b
                               (cl:if next
                                   `(,var ,init
                                          (cl:let (,@(cl:mapcar
                                                      (cl:lambda (x)
                                                        (cl:list x x) )
                                                       vars ))
                                            (declare (ignorable ,@vars))
                                         ,next ))
                                `(,var ,init) ))
                          (cl:list b nil) ))
                    varlist )))
    `(cl:do ,binds ,endlist ,@body) ))

;; EVAL FIXME
(defsynonymclfun EVAL)

;; EXP
(defsynonymclfun EXP)

;; EXPT
(defsynonymclfun EXPT)

;; FLOOR
(defsynonymclfun FLOOR)

;; GCD
(defsynonymclfun GCD)


;; LAMBDA
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-proper-lambda-list (list)
    (cl:typecase list
      (cl:list (if (cl:tailp () list)
                   list
                   (cl:let ((last (cl:last list)))
                     `(,@(cl:butlast list)
                         ,(car last)
                         cl:&rest
                         ,(cdr last)))))
      (cl:symbol `(cl:&rest ,list)))))


(defmacro lambda (args &rest body)
  `(cl:lambda ,(to-proper-lambda-list args)
     ,@(@expand-internal-define body)))


;; LCM
(defsynonymclfun LCM)

;; LENGTH
(defsynonymclfun LENGTH)

;; LET
;;; from sbcl
(defmacro named-let (name binds &body body)
  (dolist (x binds)
    (unless (= 2(length x))
      (error "malformed NAMED-LET variable spec: ~S" x)))
  `(labels ((,name ,(mapcar #'cl:first binds) 
              (rnrs:let () ,@body)))
     (cl:declare (cl:optimize (cl:debug 1) (cl:space 3)))
     (,name ,@(mapcar #'cl:second binds))))

(defmacro let (&rest args)
  (etypecase (car args)
    (cl:list `(cl:let ,(car args)
                ,@(@expand-internal-define (cdr args))))
    (cl:symbol `(named-let ,@args))))

;; LET*
(defmacro LET* ((&rest binds) &body body)
  `(cl:let* (,@binds) ,@(@expand-internal-define body)))

;; LIST
(defsynonymclfun LIST)

;; LOAD
(defsynonymclfun LOAD)

;; LOG
(defsynonymclfun LOG)

;; MAKE-STRING
(defun MAKE-STRING (length &optional (init #\Nul))
  (cl:make-string length :initial-element init))

;; MAP
(defsynonymfun MAP cl:mapcar)

;; MAX
(defsynonymclfun MAX)

;; MEMBER
(defun member (item list)
  (cl:do ((e list (cdr e)))
       ((cl:atom e))
    (cl:when (equal? item (car e))
      (cl:return e))))

;; MIN
(defsynonymclfun MIN)

;; NOT
(defsynonymclfun NOT)

;; NUMERATOR
(defsynonymclfun NUMERATOR)

;; OR
(defmacro OR (&rest args)
  `(cl:or ,@args))

;; PEEK-CHAR
;; Function: peek-char :optional iport
(defun PEEK-CHAR (&optional (iport *standard-input*))
  (cl:peek-char nil iport nil (eof-object) nil))

;; QUOTE
(defmacro QUOTE (expr)
  `(cl:quote ,expr))

;; RATIONALIZE
(defsynonymclfun RATIONALIZE)

;; READ
(defun READ (&optional (iport *standard-input*))
  (cl:read-preserving-whitespace iport nil (eof-object) nil))

;; READ-CHAR
;; Function: read-char :optional iport
(defun READ-CHAR (&optional (iport *standard-input*))
  (cl:read-char iport nil (eof-object) nil))

;; REVERSE
(defsynonymclfun REVERSE)

;; ROUND
(defsynonymclfun ROUND)

;; SIN
(defsynonymclfun SIN)

;; SQRT
(defsynonymclfun SQRT)

;; STRING
(defun STRING (&rest characters)
  (coerce characters 'cl:string))

;; TAN
(defsynonymclfun TAN)

;; TRUNCATE
(defsynonymclfun TRUNCATE)

;; VALUES
(defsynonymfun VALUES cl:values)

;; VECTOR
(defsynonymclfun VECTOR)

;; WRITE
;; obj :optional port
(defun write (obj &optional (port *standard-output*))
  (cl:write obj :stream port :readably T))

;; WRITE-CHAR
(defsynonymclfun WRITE-CHAR)

;;; eof

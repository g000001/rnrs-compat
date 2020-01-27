;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :rnrs-compat
  (:use :mbe)
  (:nicknames :rnrs)
  (:intern :loop)
  (:import-from :keyword :else)
  (:export
   :_ :* :+ :- :|...| :/  :< :<= := :=> :> :>= :abs :acos
   :and :angle :append :apply :asin :assoc :assq :assv :atan :begin
   :boolean? :call-with-current-continuation :call-with-input-file
   :call-with-output-file :call-with-values :case 
   :ceiling :char->integer :char-alphabetic? :char-ci<=? :char-ci<? :char-ci=?
   :char-ci>=? :char-ci>? :char-downcase :char-lower-case? :char-numeric?
   :char-ready? :char-upcase :char-upper-case? :char-whitespace? :char<=? :char<?
   :char=? :char>=? :char>? :char? :close-input-port :close-output-port :complex?
   :cond :cons :cos :current-input-port :current-output-port :define
   :define-syntax :delay :denominator :display :do :dynamic-wind :else
   :eof-object
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
   :scheme-report-environment :set! :set-car! :set-cdr! :sin :sqrt
   :string :string->list :string->number :string->symbol :string-append
   :string-ci<=? :string-ci<? :string-ci=? :string-ci>=? :string-ci>?
   :string-copy :string-fill! :string-length :string-ref :string-set! :string<=?
   :string<? :string=? :string>=? :string>? :string? :substring :symbol->string
   :symbol? :syntax-rules :tan :truncate
   :values :vector :vector->list :vector-fill! :vector-length :vector-ref
   :vector-set! :vector? :with-input-from-file :with-output-to-file :write
   :write-char :zero?
   . (:CAR :CDR :CAAR :CADR :CDAR :CDDR :CAAAR :CAADR :CADAR :CADDR :CDAAR :CDADR
    :CDDAR :CDDDR :CAAAAR :CAAADR :CAADAR :CAADDR :CADAAR :CADADR :CADDAR
    :CADDDR :CDAAAR :CDAADR :CDADAR :CDADDR :CDDAAR :CDDADR :CDDDAR :CDDDDR)))


(defpackage :rnrs-user
  (:use :rnrs-compat :cl)
  (:shadowing-import-from
   :rnrs-compat
   :quote :map :atan :abs :eval :<= :gcd :max :floor :do :min :write-char :/
   :numerator :asin :lambda :list :denominator :if :values :assoc :- :sin
   :char-downcase :round :or :cdr :exp :ceiling :peek-char :cos :read-char :cons
   :log :cadr :string :> :expt :char-upcase :cdddar :>= :tan :write :vector :let
   :caar :let* := :apply :rationalize :cddddr :member :make-string :length :acos
   :* :< :not :car :+ :load :cond :append :sqrt :lcm :truncate :read :case :and
   :reverse 
   . (:car :cdr :caar :cadr :cdar :cddr :caaar :caadr :cadar :caddr :cdaar :cdadr
           :cddar :cdddr :caaaar :caaadr :caadar :caaddr :cadaar :cadadr :caddar
           :cadddr :cdaaar :cdaadr :cdadar :cdaddr :cddaar :cddadr :cdddar :cddddr))
  (:shadowing-import-from :rnrs-compat :loop)
  (:export :define-function
           :with-local-define-function
           :with-local-define-variable)
  (:export :string-fill!  :delay :char-ci>? :denominator :caaddr
   :char-ci>=? :memq :cdr :sin :char-downcase :current-output-port
   :cons :cdaar :do :caaadr :char-ready?  :equal? :string-ci>? :cddr
   :open-output-file :assq :input-port? :force :cddddr :list-ref
   :for-each :char-upper-case? :not :quote :pair? :begin :caadar
   :cddaar :close-input-port :append :real? :integer->char :read
   :cdadar :let* :imag-part :length :cdddr :number? :char-upcase
   :caddar :string>=? :procedure?  :integer? :char-ci=? :exp :angle
   :max :with-input-from-file :string-ci=?  :truncate :min
   :letrec-syntax :null? :load :cadadr :vector-set! :string<?
   :char>=? :list->vector :list->string :<= :case :+ :and :odd? :cadr
   :read-char :cdaadr :/ :vector-ref :char-whitespace? :< :>=
   :string-ref :|...| :cdaaar :string-ci<? :remainder :- :output-port?
   :char-numeric? :floor :list-tail :quotient :define :even?
   :quasiquote :eqv? :eof-object? :make-polar := :make-string
   :negative? :inexact? :set-cdr! :peek-char :* :assv :cdaddr
   :vector->list :string-set! :string->number :cos :string<=? :char=?
   :char<=?  :else :member :close-output-port :exact? :inexact->exact
   :cddadr :lambda :tan :log :positive? :char<? :let :char-alphabetic?
   :complex? :symbol->string :rational? :string-copy :vector-fill!
   :caddr :char-lower-case? :string-append :caaaar :char-ci<=? :atan
   :letrec :port? :zero? :set! :magnitude :boolean? :_ :display
   :interaction-environment :cdadr :if :cadar :make-vector
   :open-input-file :scheme-report-environment :write :ceiling :abs
   :write-char :symbol? :dynamic-wind :call-with-current-continuation
   :values :asin :string?  :number->string :string->symbol
   :define-syntax :memv :list :reverse :map :caaar :=> :lcm
   :string-ci<=? :vector? :call-with-values :current-input-port
   :null-environment :cadddr :list? :expt :char->integer :sqrt :cond
   :with-output-to-file :set-car! :char>? :cadaar :vector-length
   :real-part :call-with-input-file :eof-object :vector :cdddar
   :substring :string-length :gcd :eq? :exact->inexact :caadr
   :call-with-output-file :char? :round :string->list :modulo :cdar
   :numerator :eval :syntax-rules :char-ci<? :or :cddar :let-syntax
   :string=? :assoc :apply :> :acos :rationalize :caar :string>?
   :string-ci>=? :car :make-rectangular :newline :string :nsubst
   :copy-pprint-dispatch :gentemp :continue :*package*
   :symbol-function :bit-and :listp :char-greaterp
   :make-dispatch-macro-character :last :software-type :dotimes
   :invoke-debugger :least-positive-single-float :simple-type-error
   :pprint-fill :destructuring-bind :make-broadcast-stream :peek-char
   :macro-function :macroexpand-1 :slot-unbound :*print-gensym*
   :package-error :method :not :*print-base* :fround :pprint-dispatch
   :third :1- :string<= :caadar :caddr :psetf :pathname-name :aref
   :shiftf :caaadr :cdaddr :most-negative-long-float :princ-to-string
   :*print-right-margin* :clrhash :funcall :bit-andc1 :nbutlast
   :map-into :*features* :get-macro-character :+ :defmethod
   :most-negative-double-float :internal-time-units-per-second :read
   :mapcon :unsigned-byte :+++ :sqrt :adjoin :alpha-char-p :return
   :simple-bit-vector :truncate :array :make-hash-table
   :ensure-directories-exist :function :parse-integer :and
   :substitute-if-not :set-difference :&allow-other-keys :macroexpand
   :get-universal-time :slot-value :prin1-to-string :double-float :eq
   :float :make-string :*print-pretty* :least-negative-short-float
   :simple-bit-vector-p :multiple-value-prog1 :define-condition
   :file-error :position-if :subseq :keywordp :short-site-name :cdar
   :signed-byte :upgraded-array-element-type :declare :labels
   :mismatch :array-has-fill-pointer-p :storage-condition :copy-list
   :two-way-stream :hash-table-test :gensym :unexport :change-class
   :gethash :conjugate :consp :string-right-trim :with-simple-restart
   :rassoc :asinh :dynamic-extent :find-class :remove-if-not :ash
   :char>= :tan :cdddar :*compile-file-pathname* :room :second
   :standard-generic-function :pprint-indent :style-warning :variable
   :nset-exclusive-or :make-two-way-stream :asin :let*
   :nstring-capitalize :with-output-to-string :standard :symbol-name
   :make-sequence :nconc :boole-c2 :string-not-equal
   :package-shadowing-symbols :unread-char :machine-version
   :array-dimension :describe-object :vectorp :vector :sin
   :least-positive-long-float :structure :sbit :boole-and :setq
   :string< :print-not-readable-object :space :list* :*gensym-counter*
   :nset-difference :pprint-exit-if-list-exhausted :exp :tagbody
   :ratio :copy-symbol :char-upcase :function-lambda-expression
   :invoke-restart-interactively :boole-xor :nthcdr :store-value
   :fill-pointer :logcount :makunbound :&environment :get :notinline
   :error :directory-namestring :schar :simple-warning :condition
   :simple-string :unbound-slot :*read-eval* :synonym-stream-symbol
   :list := :least-positive-normalized-single-float :the :*query-io*
   :readtable :generic-function :most-negative-single-float
   :remove-method :string-upcase :*standard-input* :fboundp
   :wild-pathname-p :delete-file :ctypecase :rem :logeqv :shadow
   :&body :caaddr :restart :ffloor :readtable-case :null :expt
   :values-list :use-value :fifth :clear-input :compile :*print-level*
   :ignore-errors :nunion :caddar :print-object :* :in-package :&rest
   :division-by-zero :defconstant :apropos-list :optimize
   :pathname-device :char-int :write-string :print-not-readable
   :package :double-float-epsilon :file-author :mapl :float-precision
   :string-downcase :intersection :ftype :boole-eqv :let
   :byte-position :*readtable* :do :nth :cddr :delete-duplicates :cadr
   :cdaar :log :assoc-if-not :bit-andc2 :with-standard-io-syntax
   :set-pprint-dispatch :otherwise :standard-object :*print-circle*
   :base-char :nsubst-if :synonym-stream :print :denominator
   :digit-char-p :when :dpb :nsubstitute :bit-eqv :boole-andc1
   :with-open-file :call-next-method :warning :most-positive-fixnum
   :stream-external-format :make-package :reverse
   :floating-point-underflow :parse-error :prin1 :get-setf-expansion
   :package-use-list :pprint-newline :string :mapcar :caaar
   :*print-radix* :*error-output* :floatp :multiple-value-setq
   :bit-orc1 :package-used-by-list :string-stream :prog1 :clear-output
   :most-negative-short-float :cons :subst-if :standard-char
   :update-instance-for-different-class :>= :return-from :string>=
   :*** :&aux :char< :nsublis :t :machine-type :realpart
   :set-exclusive-or :least-negative-double-float
   :lisp-implementation-type :most-positive-long-float :digit-char
   :case :input-stream-p :with-condition-restarts :type-of :svref
   :string/= :simple-condition :rename-file :stream-error :write-line
   :file-write-date :cddadr :*print-lines* :dribble :cadar :reduce
   :type :*default-pathname-defaults* :cddaar :logorc1 :base-string
   :first :*trace-output* :least-positive-normalized-long-float
   :most-negative-fixnum :hash-table-p :abort :pprint-pop :cadaar
   :lower-case-p :eval :numerator :rational :butlast :simple-array
   :*load-truename* :declaration :least-negative-single-float
   :compiler-macro :probe-file :symbol-package :atom
   :print-unreadable-object :compile-file-pathname :restart-bind
   :ftruncate :echo-stream-input-stream :get-decoded-time
   :*load-pathname* :*print-case* :make-pathname :char> :file-stream
   :graphic-char-p :unintern :hash-table-rehash-threshold :or
   :upper-case-p :handler-bind :rationalize :speed :with-accessors
   :file-length :function-keywords :interactive-stream-p :find-method
   :file-position :remove-if :array-rank-limit :char-not-greaterp
   :shadowing-import :class-name :cell-error-name :delete :realp
   :least-negative-normalized-double-float :rotatef
   :no-applicable-method :read-delimited-list :pathname-match-p :pop
   :ldb-test :array-element-type :require :equal :*print-escape*
   :string-lessp :elt :decode-float :/= :boole-clr :muffle-warning
   :make-symbol :two-way-stream-output-stream :tanh :handler-case
   :notany :open :floating-point-overflow :copy-alist
   :string-capitalize :*print-length* :remove :close
   :multiple-value-list :char-code-limit :built-in-class
   :copy-structure :// :*read-default-float-format* :pprint-tab
   :readtablep :invalid-method-error :code-char :compiled-function-p
   :oddp :make-concatenated-stream :defpackage :array-in-bounds-p
   :string= :logandc2 :inline :single-float-negative-epsilon
   :vector-push :cosh :pathname-directory :disassemble :find-package
   :*print-pprint-dispatch* :file-namestring :fresh-line
   :ensure-generic-function :reader-error :char-lessp :char-equal
   :double-float-negative-epsilon :loop-finish :logxor :replace
   :initialize-instance :remove-duplicates :string-left-trim :subsetp
   :*print-miser-width* :position :lambda-list-keywords :maplist
   :array-rank :float-sign :directory :logical-pathname-translations
   :hash-table :char-name :bit-nand :sublis :ecase :assoc :zerop
   :add-method :row-major-aref :subtypep :compute-restarts :atan
   :single-float-epsilon :symbol-plist :complement :symbolp :apropos
   :stable-sort :class-of :call-method :&optional :stream-error-stream
   :/ :update-instance-for-redefined-class :imagpart :bit-vector-p
   :copy-seq :getf :boole-andc2 :simple-error :lognot :namestring
   :make-method :logbitp :symbol-value :type-error-datum :rationalp
   :defmacro :boole-set :/// :deftype :declaim :char-not-equal
   :control-error :*terminal-io* :shared-initialize :array-total-size
   :cdadr :pathname :< :type-error :search :type-error-expected-type
   :find-restart :broadcast-stream-streams :read-preserving-whitespace
   :force-output :with-input-from-string :compilation-speed :name-char
   :array-row-major-index :check-type :logorc2 :rplaca :if :logandc1
   :standard-class :least-positive-normalized-short-float :tailp
   :do-external-symbols :fmakunbound :extended-char
   :array-dimension-limit :defvar :cond :nreconc
   :arithmetic-error-operands :bit-vector :plusp :list-all-packages
   :signal :map :assoc-if :multiple-value-call :package-error-package
   :*load-print* :caar :max :copy-tree :eql :make-synonym-stream
   :rename-package :hash-table-size :do* :prog2 :open-stream-p
   :make-string-output-stream :pprint-tabular :reinitialize-instance
   :translate-pathname :boolean :*debug-io* :string-greaterp :logand
   :princ :random :cddddr :export :pprint-linear :floor :boole-nand
   :call-arguments-limit :cadadr :fdefinition :truename :prog*
   :least-positive-normalized-double-float :cdddr :string-not-greaterp
   :pathname-type :count-if-not :method-combination-error
   :*compile-print* :caaaar :mapcan :&whole :cos
   :unbound-slot-instance :simple-vector-p :stream :safety
   :standard-method :do-symbols :adjust-array :software-version
   :defstruct :fourth :eval-when :least-negative-normalized-long-float
   :acosh :concatenated-stream :string-equal :program-error
   :read-from-string :throw :define-modify-macro :sleep
   :least-negative-normalized-single-float :maphash :coerce
   :least-positive-double-float :get-dispatch-macro-character
   :concatenate :ignore :least-positive-short-float :long-float :cis
   :++ :array-dimensions :read-sequence :host-namestring
   :*standard-output* :set-macro-character :bit-ior :float-digits
   :package-nicknames :array-displacement :car :ldiff :cerror :cdaaar
   :make-echo-stream :ldb :multiple-values-limit :slot-makunbound
   :isqrt :caadr :functionp :with-package-iterator :pathname-host
   :ignorable :boole-orc1 :string> :push :find :typep :restart-case
   :single-float :define-compiler-macro :constantly :serious-condition
   :flet :pairlis :psetq :char-not-lessp :boole-nor
   :nsubstitute-if-not :char= :boole-1 :some :count-if :rassoc-if
   :standard-char-p :untrace :abs :format :integer-decode-float :warn
   :unwind-protect :make-string-input-stream :scale-float :read-line
   :simple-string-p :*modules* :sxhash :with-slots :formatter
   :copy-readtable :packagep :find-symbol :read-byte :subst-if-not
   :lambda-parameters-limit :tree-equal :fixnum :intern :macrolet
   :load-time-value :rassoc-if-not :*load-verbose* :constantp
   :symbol-macrolet :logtest :mapc :write-char :equalp
   :pprint-logical-block :floating-point-invalid-operation
   :nstring-downcase :progv :endp :string-not-lessp :*read-suppress*
   :char-code :nintersection :cdr :slot-boundp :invoke-restart
   :method-qualifiers :sixth :*print-array* :go :make-condition
   :long-float-epsilon :vector-pop :vector-push-extend :import
   :*compile-file-truename* :*debugger-hook* :acos
   :short-float-negative-epsilon :enough-namestring
   :simple-condition-format-arguments :with-open-stream :locally
   :two-way-stream-input-stream :encode-universal-time :substitute-if
   :values :lcm :write-to-string :make-load-form-saving-slots :trace
   :remhash :concatenated-stream-streams :step
   :least-negative-normalized-short-float :length
   :lisp-implementation-version :find-all-symbols :mod :real :set
   :every :stream-element-type :string-trim :identity :documentation
   :random-state-p :rplacd :revappend :symbol :simple-vector
   :characterp :min :find-if :eighth :*random-state* :time :subst
   :compiler-macro-function :set-dispatch-macro-character
   :package-name :boundp :acons :file-error-pathname :get-properties
   :simple-condition-format-control :make-load-form :loop
   :*print-readably* :*compile-verbose* :defgeneric :y-or-n-p
   :satisfies :with-compilation-unit :long-site-name :delete-if
   :compute-applicable-methods :get-output-stream-string :1+
   :allocate-instance :prog :integer :numberp :finish-output :lognor
   :typecase :machine-instance :compile-file :substitute :random-state
   :most-positive-single-float :yes-or-no-p :decode-universal-time
   :get-internal-run-time :hash-table-count :byte-size :load :count
   :delete-package :undefined-function :gcd :ninth :&key
   :pathname-version :cddar :keyword :signum :member :use-package
   :write-byte :bignum :catch :user-homedir-pathname :bit
   :make-instances-obsolete :get-internal-real-time :cadddr :arrayp
   :boole :decf :evenp :merge-pathnames :listen :boole-orc2
   :array-total-size-limit :simple-base-string :structure-object
   :*read-base* :position-if-not :define-method-combination :union
   :minusp :setf :write-sequence :boole-ior :end-of-file :pprint
   :remprop :progn :boole-c1 :integer-length :unuse-package :terpri
   :delete-if-not :make-instance :nreverse :slot-exists-p :describe
   :provide :bit-not :complex :ed :special-operator-p :complexp
   :assert :do-all-symbols :inspect :bit-xor
   :echo-stream-output-stream :sinh :bit-nor :unless :phase
   :logical-pathname :load-logical-pathname-translations
   :nstring-upcase :echo-stream :restart-name :set-syntax-from-char
   :nil :arithmetic-error-operation :number :*macroexpand-hook*
   :member-if :notevery :arithmetic-error :nsubst-if-not :find-if-not
   :pathnamep :long-float-negative-epsilon :parse-namestring :bit-orc2
   :member-if-not :make-array :with-hash-table-iterator
   :translate-logical-pathname :read-char-no-hang :sort :defsetf
   :*break-on-signals* :apply :least-negative-long-float :list-length
   :special :class :make-list :append :char-downcase :cdaadr :<=
   :hash-table-rehash-size :nth-value :next-method-p :char<=
   :deposit-field :output-stream-p :multiple-value-bind :fill
   :etypecase :block :write :lognand :char/= :floating-point-inexact
   :method-combination :float-radix :quote :cell-error :alphanumericp
   :file-string-length :stringp :** :tenth :defun :integerp :read-char
   :defclass :define-symbol-macro :structure-class :char
   :make-random-state :nsubstitute-if :pushnew :atanh :- :debug
   :seventh :byte :slot-missing :logior :define-setf-expander
   :both-case-p :character :short-float-epsilon :mask-field :streamp
   :remf :adjustable-array-p :ceiling :cdadar :sequence
   :no-next-method :boole-2 :lambda :short-float :pi :proclaim
   :broadcast-stream :round :ccase :upgraded-complex-part-type :incf
   :most-positive-double-float :fceiling :unbound-variable
   :defparameter :rest :compiled-function :break :> :dolist
   :most-positive-short-float :merge))

(defpackage :rnrs-compat-internal
  (:use :rnrs-user))

;;; eof


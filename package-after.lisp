(in-package :cl-user)

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
   :reverse )
  (:export . #.(let ((ans '()))
                 (do-external-symbols (s :cl)
                   (push s ans))
                 (do-external-symbols (s :rnrs-compat)
                   (push s ans))
                 (mapcar (lambda (s) (intern (string s) :keyword)) ans))))

(defpackage :rnrs-compat-internal
  (:use :rnrs-user :fiveam))

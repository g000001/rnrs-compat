;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :rnrs-compat
  (:use)
  (:nicknames :rnrs)
  (:export :lambda
           :map
           :make-string
           :define
           :begin
           :char=?
           :char<?
           :char-ci<?
           :char-ci=?
           :char?
           :display
           :eq?
           :equal?
           :exact?
           :integer?
           :map
           :memq
           :null?
           :newline
           :pair?
           :procedure?
           :set!
           :set-car!
           :set-cdr!
           :any
           :string?
           :string-length
           :string-set!
           :string-ref
           :substring
           :vector-length
           :vector-ref
           :vector-set!
           :zero?
           ))

(defpackage :rnrs-compat-internal
  (:use :rnrs-compat :cl :fiveam)
  (:shadowing-import-from :rnrs-compat
                          :map
                          :make-string
                          :lambda
                          ))


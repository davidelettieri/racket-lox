#lang racket/base

(require (for-syntax racket/base racket/syntax))

(define-syntax (lox-define-var stx)
  (syntax-case stx ()
    [(_ name val)
     (with-syntax ([name_ (format-id #'name "~a" #'name)])
       #'(define name_ val))]))

(define-syntax lox-program
  (syntax-rules ()
    [(lox-program a) a]
    [(lox-program a ...) (begin a ...)]))

(define-syntax (lox-assignment stx)
  (syntax-case stx ()
    [(_ name val)
     (with-syntax ([name_ (format-id #'name "~a" #'name)])
       #'(begin
           (set! name_ val)
           val))]))

(define-syntax (lox-var-value stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([name_ (format-id #'name "~a" #'name)])
       #'name_)]))

(define-syntax-rule (lox-print value)
  (displayln value))

(define-syntax-rule (lox-invalid-assignment-target)
  (displayln "Error at '=': Invalid assignment target."))

(provide lox-define-var lox-program lox-assignment lox-var-value lox-print lox-invalid-assignment-target)


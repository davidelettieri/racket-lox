#lang racket

(require (for-syntax racket/base racket/syntax))

(define-syntax (lox-define-var stx)
  (syntax-case stx ()
    [(_ name val)
     (with-syntax ([name_ (format-id #'name "~a" (syntax-e #'name))])
       #'(define name_ val))]))

(define-syntax lox-program
  (syntax-rules ()
    [(lox-program a) a]
    [(lox-program a ...) (begin a ...)]))

(define-syntax lox-declarations
  (syntax-rules ()
    [(lox-declarations a) a]
    [(lox-declarations a ...) (begin a ...)]))

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

(provide lox-define-var 
         lox-program 
         lox-assignment 
         lox-var-value 
         lox-print
         lox-declarations)


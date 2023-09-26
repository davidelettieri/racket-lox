#lang racket/base

(require (for-syntax racket/base racket/syntax))

(define-syntax (lox-var stx)
  (syntax-case stx ()
    [(lox-var name val)
     (with-syntax ([name_ (format-id #'name "~a" #'name)])
       #'(define name_ val))]))

(define-syntax lox-program
  (syntax-rules ()
    [(lox-program a) a]
    [(lox-program a ...) (begin a ...)]))

(provide lox-var lox-program)


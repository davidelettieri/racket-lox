#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "lang/resolver.rkt")
         "lox.rkt")

(begin-for-syntax
  (define (unwrap-forms forms-stx)
    (define unwrapped
      (syntax-parse forms-stx
        [(((~datum lox-module-wrapper) form ...)) #'(form ...)]
        [_ forms-stx]))
    (syntax-parse unwrapped
      [(((~datum begin) form ...)) (syntax->list #'(form ...))]
      [_ (syntax->list unwrapped)])))

(define-syntax lox-module-wrapper
  (syntax-parser
    [(_ form ...)
     #'(begin
         form ...)]))

(define-syntax custom-module-begin
  (syntax-parser
    [(_ form ...)
     (define raw-forms (unwrap-forms #'(form ...)))
     (resolve-statements raw-forms)
     (with-syntax ([(fixed-forms ...) (resolve-redefinitions raw-forms)])
       #'(#%plain-module-begin ;; use module-begin to have expressions printed out
          fixed-forms ...))]))

(define (clock)
  (current-inexact-milliseconds))

(provide [rename-out (custom-module-begin #%module-begin) (lox-top #%top)]
         lox-module-wrapper
         #%datum
         #%app
         #%top-interaction
         clock
         (except-out (all-from-out "lox.rkt") lox-top))

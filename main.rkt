#lang racket

(require "lox.rkt"
         (for-syntax racket/base
                     syntax/parse))

(define-syntax custom-module-begin
  (syntax-parser
    [(_ forms:expr ...)
     (with-syntax ([(fixed-forms ...) (resolve-redefinitions (syntax->list #'(forms ...)))])
       #'(#%plain-module-begin ;; use module-begin to have expressions printed out
          fixed-forms ...))]))

(define (clock)
  (current-inexact-milliseconds))

(provide [rename-out (custom-module-begin #%module-begin) (lox-top #%top)]
         #%datum
         #%app
         #%top-interaction
         clock
         (except-out (all-from-out "lox.rkt") lox-top))

#lang racket

(require 
  "lox.rkt"
  (for-syntax racket/base syntax/parse))

(define-syntax #%module-begin
  (syntax-parser
   [(_ forms:expr ...)
    #'(#%plain-module-begin
       forms ...)]))

(provide #%module-begin #%app #%datum #%top-interaction (all-from-out racket) (all-from-out "lox.rkt"))
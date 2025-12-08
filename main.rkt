#lang racket

(require 
  "lox.rkt"
  (for-syntax racket/base syntax/parse))

(define-syntax custom-module-begin
  (syntax-parser
   [(_ forms:expr ...)
    #'(#%plain-module-begin ;; use module-begin to have expressions printed out
       forms ...)]))

(provide [rename-out (custom-module-begin #%module-begin)] #%datum #%top #%top-interaction (all-from-out "lox.rkt"))
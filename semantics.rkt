#lang racket/base

(define subtract -)
(define add +)
(define multiply *)
(define divide /)

(define-syntax-rule (binary left operator right)
    (operator left right))

; (struct (lox-call callee paren arguments))
; (struct (lox-get obj name))
; (struct (lox-grouping expr))
; (struct (lox-literal value))
; (struct (lox-logical left operator right))
; (struct (lox-unary operator right))
; (struct (lox-set obj name value))
; (struct (lox-super))

(provide subtract
         add
         multiply
         divide
         binary)


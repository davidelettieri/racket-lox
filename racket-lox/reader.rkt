#lang racket

(require racket/trace)
(require "scanner.rkt")

(require syntax/readerr)

(provide read read-syntax)

(define (read in)
  (syntax->datum (read-syntax #f in)))

(define (read-syntax src in)
  (skip-whitespace in)
  (read-lox src in))

(define (skip-whitespace in)
  (regexp-match #px"^\\s*" in))

(define (read-lox src in)
  (port-count-lines! in)
  (define token (get-token in)) 
  (if (eof-object? token) 
      token
      (datum->syntax #f 1)))

(define (parse-expr in)
  (parse-equality in))

(define (parse-equality in)
  in)

    
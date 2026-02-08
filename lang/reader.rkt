#lang racket/base

(require (only-in racket/base eof-object?)
         "scanner.rkt"
         "parser.rkt")

(define (read in)
  (define tokens (scan-tokens in))
  (define ast (parse tokens))
  `(module anonymous-module racket-lox
     ,@ast))

(define (read-syntax src in)
  (define source (or src (object-name in)))
  (define tokens (scan-tokens in))
  (define ast (parse tokens))
  (define module-stx
    `(module anonymous-module racket-lox
       ,@ast))
  module-stx)

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer) (dynamic-require 'racket-lox/lang/colorer 'color-lexer)]
      [else default])))

(provide read
         read-syntax
         get-info)

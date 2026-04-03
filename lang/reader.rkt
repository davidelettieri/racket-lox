#lang racket/base

(require "scanner.rkt"
         "parser.rkt")

(define (read in)
  (read-syntax #f in))

(define (read-syntax src in)
  (define source (or src (object-name in)))
  (define tokens (scan-tokens in))
  (define ast (parse tokens))
  (define module-datum
    `(module anonymous-module racket-lox
       (lox-module-wrapper ,@ast)))
  (datum->syntax #f module-datum (list source #f #f #f #f)))

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer) (dynamic-require 'racket-lox/lang/colorer 'color-lexer)]
      [else default])))

(provide read
         read-syntax
         get-info)

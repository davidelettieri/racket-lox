#lang racket
 
(module reader racket
  (require syntax/strip-context "lexer.rkt" "parser.rkt")
 
  (provide (rename-out [my-read read]
                       [my-read-syntax read-syntax]))

  (define (my-read in)
    (syntax->datum
     (my-read-syntax #f in)))
 
  (define (my-read-syntax src in)
    (with-syntax ([result (lox-parser (lambda () (lox-lexer in)))])
      (strip-context
       #'(module anything racket
           (require racket-lox/semantics)
           result)))))
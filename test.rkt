#lang racket

(module+ test
  (require rackunit
           parser-tools/lex
           "lexer.rkt"
           "parser.rkt"
           "semantics.rkt")

  (define (parse-string s)
    (let ([in (open-input-string s)])
      (lox-parser (lambda () (lox-lexer in)))))

  (test-case
   "Tokens are recognized correctly"
   (let ([in (open-input-string "()+-*/ 123\"test\" identifier")])
     (check-eqv? (position-token-token (lox-lexer in)) 'LEFT_PAREN)
     (check-eqv? (position-token-token (lox-lexer in)) 'RIGHT_PAREN)
     (check-eqv? (position-token-token (lox-lexer in)) 'PLUS)
     (check-eqv? (position-token-token (lox-lexer in)) 'MINUS)
     (check-eqv? (position-token-token (lox-lexer in)) 'STAR)
     (check-eqv? (position-token-token (lox-lexer in)) 'SLASH)
     (let ([t (lox-lexer in)])
       (check-eqv? (token-name (position-token-token t)) 'NUMBER)
       (check-eqv? (token-value (position-token-token t)) 123))
     (let ([t (lox-lexer in)])
       (check-eqv? (token-name (position-token-token t)) 'STRING)
       (check-equal? (token-value (position-token-token t)) "test"))
     (let ([t (lox-lexer in)])
       (check-eqv? (token-name (position-token-token t)) 'IDENTIFIER)
       (check-equal? (token-value (position-token-token t)) "identifier"))))
  (test-case
   "Simple expression are parsed correctly"
   (let ([sources (list
                   '("2+2" . (+ 2 2))
                   '("2-2" . (- 2 2))
                   '("2*3" . (* 2 3))
                   '("2/3" . (/ 2 3))
                   '("2+3*2" . (+ 2 (* 3 2))))])
     (for-each
      (lambda (el) (check-equal? (syntax->datum (parse-string (car el))) (cdr el))) sources))))
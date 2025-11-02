#lang racket

(require rackunit
         "scanner.rkt")

(define (types-from-string s)
  (map token-type (scan-tokens (open-input-string s))))

(define (lexemes-from-string s)
  (map token-lexeme (scan-tokens (open-input-string s))))

(module+ test
  (define expected-types
    '(LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE COMMA DOT MINUS PLUS SEMICOLON STAR EOF))

  (test-case "scan single-character punctuation tokens"
    (check-equal? (types-from-string "(){},.-+;*")
                  expected-types))

  (test-case "ignore whitespace between tokens"
    (check-equal? (types-from-string " ( \n ) { }\t, .  - + ;  *   ")
                  expected-types))

  (test-case "lexemes for single-character tokens"
    (define toks (scan-tokens (open-input-string "(){},.-+;*")))
    ;; take first 10 to skip EOF
    (define first10 (take toks 10))
    (check-equal? (map token-lexeme first10)
                  (list #\( #\) #\{ #\} #\, #\. #\- #\+ #\; #\*))))

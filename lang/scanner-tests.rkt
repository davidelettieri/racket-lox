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
                  (list #\( #\) #\{ #\} #\, #\. #\- #\+ #\; #\*)))

  (test-case "scan multi-character comparison operators"
    (check-equal? (types-from-string "!= == <= >=")
                  '(BANG_EQUAL EQUAL_EQUAL LESS_EQUAL GREATER_EQUAL EOF)))

  (test-case "lexemes for multi-character operators"
    (define toks (scan-tokens (open-input-string "!= == <= >=")))
    (define first4 (take toks 4))
    (check-equal? (map token-lexeme first4)
                  (list "!=" "==" "<=" ">=")))

  (test-case "mixed single and multi-character tokens"
    (check-equal? (types-from-string "( != ) { == } , <= . >= *")
                  '(LEFT_PAREN BANG_EQUAL RIGHT_PAREN LEFT_BRACE EQUAL_EQUAL RIGHT_BRACE COMMA LESS_EQUAL DOT GREATER_EQUAL STAR EOF)))

  (test-case "scan single-character comparison and assignment tokens"
    (check-equal? (types-from-string "! = < >")
                  '(BANG EQUAL LESS GREATER EOF)))

  (test-case "lexemes for single-character comparison and assignment tokens"
    (define toks (scan-tokens (open-input-string "! = < >")))
    (define first4 (take toks 4))
    (check-equal? (map token-lexeme first4)
                  (list #\! #\= #\< #\>)))

  (test-case "bang followed by equals produces BANG_EQUAL"
    (check-equal? (types-from-string "!=")
                  '(BANG_EQUAL EOF)))

  (test-case "equals followed by equals produces EQUAL_EQUAL"
    (check-equal? (types-from-string "==")
                  '(EQUAL_EQUAL EOF))))

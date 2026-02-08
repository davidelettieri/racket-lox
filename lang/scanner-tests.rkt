#lang racket

(require rackunit
         "scanner.rkt")

(define (types-from-string s)
  (map token-type (scan-tokens (open-input-string s))))

(module+ test
  (define expected-types
    '(LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE COMMA DOT MINUS PLUS SEMICOLON STAR EOF))

  (test-case "scan single-character punctuation tokens"
    (check-equal? (types-from-string "(){},.-+;*") expected-types))

  (test-case "ignore whitespace between tokens"
    (check-equal? (types-from-string " ( \n ) { }\t, .  - + ;  *   ") expected-types))

  (test-case "lexemes for single-character tokens"
    (define toks (scan-tokens (open-input-string "(){},.-+;*")))
    ;; take first 10 to skip EOF
    (define first10 (take toks 10))
    (check-equal? (map token-lexeme first10) (list #\( #\) #\{ #\} #\, #\. #\- #\+ #\; #\*)))

  (test-case "scan multi-character comparison operators"
    (check-equal? (types-from-string "!= == <= >=")
                  '(BANG_EQUAL EQUAL_EQUAL LESS_EQUAL GREATER_EQUAL EOF)))

  (test-case "lexemes for multi-character operators"
    (define toks (scan-tokens (open-input-string "!= == <= >=")))
    (define first4 (take toks 4))
    (check-equal? (map token-lexeme first4) (list "!=" "==" "<=" ">=")))

  (test-case "mixed single and multi-character tokens"
    (check-equal? (types-from-string "( != ) { == } , <= . >= *")
                  '(LEFT_PAREN BANG_EQUAL
                               RIGHT_PAREN
                               LEFT_BRACE
                               EQUAL_EQUAL
                               RIGHT_BRACE
                               COMMA
                               LESS_EQUAL
                               DOT
                               GREATER_EQUAL
                               STAR
                               EOF)))

  (test-case "scan single-character comparison and assignment tokens"
    (check-equal? (types-from-string "! = < >") '(BANG EQUAL LESS GREATER EOF)))

  (test-case "lexemes for single-character comparison and assignment tokens"
    (define toks (scan-tokens (open-input-string "! = < >")))
    (define first4 (take toks 4))
    (check-equal? (map token-lexeme first4) (list #\! #\= #\< #\>)))

  (test-case "bang followed by equals produces BANG_EQUAL"
    (check-equal? (types-from-string "!=") '(BANG_EQUAL EOF)))

  (test-case "equals followed by equals produces EQUAL_EQUAL"
    (check-equal? (types-from-string "==") '(EQUAL_EQUAL EOF)))

  (test-case "scan slash token"
    (check-equal? (types-from-string "/") '(SLASH EOF)))

  (test-case "lexeme for slash token"
    (define toks (scan-tokens (open-input-string "/")))
    (define first1 (take toks 1))
    (check-equal? (map token-lexeme first1) (list #\/)))

  (test-case "line comments are ignored until newline"
    (check-equal? (types-from-string "// this is a comment\n+") '(PLUS EOF)))

  (test-case "line comment at end of file is ignored"
    (check-equal? (types-from-string "// nothing after comment") '(EOF)))

  (test-case "slash not starting a comment is a token"
    (check-equal? (types-from-string "/=") '(SLASH EQUAL EOF)))

  (test-case "scan string literal token"
    (check-equal? (types-from-string "\"lox\"") '(STRING EOF)))

  (test-case "lexeme for string literal token"
    (define toks (scan-tokens (open-input-string "\"lox interpreter\"")))
    (define first1 (take toks 1))
    (check-equal? (map token-lexeme first1) (list "lox interpreter")))

  (test-case "scan integer number literal token"
    (check-equal? (types-from-string "123") '(NUMBER EOF)))

  (test-case "lexeme and literal for integer number literal"
    (define token (first (scan-tokens (open-input-string "123"))))
    (check-equal? (token-lexeme token) "123")
    (check-true (flonum? (token-literal token)))
    (check-equal? (token-literal token) 123.0))

  (test-case "lexeme and literal for floating-point number literal"
    (define token (first (scan-tokens (open-input-string "123.45"))))
    (check-equal? (token-lexeme token) "123.45")
    (check-true (flonum? (token-literal token)))
    (check-equal? (token-literal token) 123.45))

  (test-case "scan all keyword tokens"
    (define input "and class else false for fun if nil or print return super this true var while")
    (check-equal?
     (types-from-string input)
     '(AND CLASS ELSE FALSE FOR FUN IF NIL OR PRINT RETURN SUPER THIS TRUE VAR WHILE EOF)))

  (test-case "lexemes for keyword tokens"
    (define toks
      (scan-tokens (open-input-string
                    "and class else false for fun if nil or print return super this true var while")))
    (define first16 (take toks 16))
    (check-equal? (map token-lexeme first16)
                  '("and" "class"
                          "else"
                          "false"
                          "for"
                          "fun"
                          "if"
                          "nil"
                          "or"
                          "print"
                          "return"
                          "super"
                          "this"
                          "true"
                          "var"
                          "while")))

  (test-case "non-keyword identifiers produce IDENTIFIER tokens"
    (define toks (scan-tokens (open-input-string "lox foo123 bar_baz")))
    (define first3 (take toks 3))
    (check-equal? (map token-type first3) '(IDENTIFIER IDENTIFIER IDENTIFIER))
    (check-equal? (map token-lexeme first3) '("lox" "foo123" "bar_baz"))))

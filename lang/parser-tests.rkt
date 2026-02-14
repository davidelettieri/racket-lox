#lang racket

(require rackunit
         syntax/parse
         "parser.rkt"
         "scanner.rkt")

(define (parse-from-string source)
  "Helper to scan and parse source code"
  (define tokens (scan-tokens (open-input-string source)))
  (parse tokens))

(define (strip-tokens datum)
  (cond
    [(token? datum) (list 'token (token-type datum) (token-lexeme datum) (token-literal datum))]
    [(list? datum) (map strip-tokens datum)]
    [else datum]))

(define-syntax-rule (parse-and-assert str expected)
  (check-equal? (strip-tokens (map syntax->datum (parse-from-string str))) expected))

(module+ test
  ;; Declarations
  (test-case "class declaration"
    (parse-and-assert "class A {}" '((lox-class A #f ()))))

  (test-case "class declaration with superclass"
    (parse-and-assert "class A < B {}" '((lox-class A B ()))))

  (test-case "class declaration with methods"
    (parse-and-assert "class A { m() {} }" '((lox-class A #f ((lox-function m () ()))))))

  (test-case "fun declaration"
    (parse-and-assert "fun f(a, b) {}" '((lox-function f (a b) ()))))

  (test-case "var declaration"
    (parse-and-assert "var c;" '((lox-var-declaration c lox-nil))))

  (test-case "var declaration initialized"
    (parse-and-assert "var c = 1;" '((lox-var-declaration c (lox-literal 1.0)))))

  (test-case "parse var declaration with while loop (fixed)"
    (parse-and-assert
     "var c = 0;while (c < 3) c = c + 1;"
     '((lox-var-declaration c (lox-literal 0.0))
       (lox-while (lox-binary (lox-variable c) LESS (lox-literal 3.0))
                  (lox-assign c (lox-binary (lox-variable c) PLUS (lox-literal 1.0)))))))

  ;; Statements
  (test-case "expression statement"
    (parse-and-assert "1;" '((lox-literal 1.0))))

  (test-case "variable access"
    (parse-and-assert "a;" '((lox-variable a))))

  (test-case "for statement"
    (parse-and-assert "for(;;){}" '((lox-while (lox-literal #t) (lox-block)))))

  (test-case "for statement full"
    (parse-and-assert
     "for(var i=0; i<10; i = i + 1) {}"
     '((lox-block (lox-var-declaration i (lox-literal 0.0))
                  (lox-while (lox-binary (lox-variable i) LESS (lox-literal 10.0))
                             (lox-block (lox-block)
                                        (lox-assign
                                         i
                                         (lox-binary (lox-variable i) PLUS (lox-literal 1.0)))))))))

  (test-case "if statement"
    (parse-and-assert "if (condition) print 1;"
                      '((lox-if (lox-variable condition) (lox-print (lox-literal 1.0))))))

  (test-case "if else statement"
    (parse-and-assert "if (condition) print 1; else print 2;"
                      '((lox-if (lox-variable condition)
                                (lox-print (lox-literal 1.0))
                                (lox-print (lox-literal 2.0))))))

  (test-case "print statement"
    (parse-and-assert "print 1;" '((lox-print (lox-literal 1.0)))))

  (test-case "return statement"
    (parse-and-assert "return;" '((lox-return lox-nil))))

  (test-case "return value statement"
    (parse-and-assert "return 1;" '((lox-return (lox-literal 1.0)))))

  (test-case "while statement"
    (parse-and-assert "while (true) {}" '((lox-while (lox-literal #t) (lox-block)))))

  (test-case "block statement"
    (parse-and-assert "{ var a = 1; print a; }"
                      '((lox-block (lox-var-declaration a (lox-literal 1.0))
                                   (lox-print (lox-variable a))))))

  ;; Expressions
  (test-case "assignment"
    (parse-and-assert "a = 1;" '((lox-assign a (lox-literal 1.0)))))

  (test-case "logical or"
    (parse-and-assert "true or false;" '((lox-binary (lox-literal #t) OR (lox-literal #f)))))

  (test-case "logical and"
    (parse-and-assert "true and false;" '((lox-binary (lox-literal #t) AND (lox-literal #f)))))

  (test-case "equality"
    (parse-and-assert "1 == 2;" '((lox-binary (lox-literal 1.0) EQUAL_EQUAL (lox-literal 2.0)))))

  (test-case "comparison"
    (parse-and-assert "1 < 2;" '((lox-binary (lox-literal 1.0) LESS (lox-literal 2.0)))))

  (test-case "term"
    (parse-and-assert "1 + 2;" '((lox-binary (lox-literal 1.0) PLUS (lox-literal 2.0)))))

  (test-case "factor"
    (parse-and-assert "1 * 2;" '((lox-binary (lox-literal 1.0) STAR (lox-literal 2.0)))))

  (test-case "unary"
    (parse-and-assert "!true;" '((lox-unary BANG (lox-literal #t)))))

  (test-case "call"
    (parse-and-assert "f(1, 2);" '((lox-call (lox-variable f) (lox-literal 1.0) (lox-literal 2.0)))))

  (test-case "get"
    (parse-and-assert "a.b;" '((lox-get (lox-variable a) (token IDENTIFIER "b" #f)))))

  (test-case "set"
    (parse-and-assert "a.b = c;"
                      '((lox-set (lox-variable a) (token IDENTIFIER "b" #f) (lox-variable c)))))

  (test-case "super method"
    (parse-and-assert "super.method();"
                      '((lox-call (lox-super (token SUPER "super" #f)
                                             (token IDENTIFIER "method" #f))))))

  (test-case "this"
    (parse-and-assert "this;" '((lox-this (token THIS "this" #f)))))

  (test-case "grouping"
    (parse-and-assert "(1);" '((lox-grouping (lox-literal 1.0)))))

  (test-case "error limit arguments"
    (define args
      (string-join (for/list ([i (in-range 256)])
                     (number->string i))
                   ", "))
    (define source (format "f(~a);" args))
    (define error-out (open-output-string))

    (parameterize ([current-error-port error-out]
                   [exit-handler (lambda (code) (void))])
      (parse-from-string source))

    (check-regexp-match #rx"Error at '255': Can't have more than 255 arguments"
                        (get-output-string error-out)))

  (test-case "error limit parameters"
    (define params
      (string-join (for/list ([i (in-range 256)])
                     (format "a~a" i))
                   ", "))
    (define source (format "fun f(~a) {}" params))
    (define error-out (open-output-string))

    (parameterize ([current-error-port error-out]
                   [exit-handler (lambda (code) (void))])
      (parse-from-string source))

    (check-regexp-match #rx"Error at 'a255': Can't have more than 255 parameters"
                        (get-output-string error-out))))
